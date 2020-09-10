# Loads required CRAN packages
require(drake)
require(shiny)
require(ggplot2)
require(networkD3)
require(tidyr)
require(magrittr)
require(DT)
require(shinydashboard)
require(rprojroot)

# Loads required non-CRAN packages. Is there an equivalent to require for non-CRAN packages?
library(MKHthemes)
library(Recca)
library(SEAPSUTWorkflow)
library(PFUSetup)


################################################################################

# Set up the root directory
root <- rprojroot::is_rstudio_project

# Identifies the file path for the drake cache
cache_path <- root$find_file(".drake")

################################################################################

# Creates a list of the countries in the drake workflow, set in the _drake.R script
countries <- drake::readd(SEAPSUTWorkflow::target_names$countries, 
             path = cache_path, 
             character_only = TRUE)

################################################################################

#  Creates a list of years in the drake workflow
max_year <- drake::readd(SEAPSUTWorkflow::target_names$max_year, 
                         path = cache_path, 
                         character_only = TRUE)

years <- c(1960:max_year)

################################################################################

# Creates a df with the Balanced IEA Data for the selected countries

balanced_iea_data <- drake::readd(SEAPSUTWorkflow::target_names$BalancedIEAData, 
                                  path = cache_path, 
                                  character_only = TRUE)

balancediea_flowaggpoints <- unique(balanced_iea_data$Flow.aggregation.point)

balancediea_flows <- unique(balanced_iea_data$Flow)


# Creates a df with the final-to-useful efficiency (eta) data

etas_and_phis <- drake::readd(SEAPSUTWorkflow::target_names$CompletedEfficiencyTables, 
                         path = cache_path, 
                         character_only = TRUE)

etas <- etas_and_phis %>%
  dplyr::filter(Quantity == "eta.fu")

################################################################################

# Creates a df with the exergy-to-energy ratio (phi) data 

phis <- etas_and_phis %>%
  dplyr::filter(Quantity == "phi.u")

################################################################################

# Creates a df with the Destination-Machine&Eu.product allocation data

allocations <- drake::readd(SEAPSUTWorkflow::target_names$CompletedAllocationTables, 
                           path = cache_path, 
                           character_only = TRUE)

# Adds a combined Machine-Eu.product column
allocations$Machine_Eu.product = paste(allocations$Machine," - ", allocations$Eu.product) #%>%
 #dplyr::relocate(Machine_Eu.product, .after = Eu.product)


################################################################################

# Using SEAPSUTWorkflow::readd_by_country produces the error "Error: 'key' must be a scalar"

PSUT_final_data <- drake::readd(SEAPSUTWorkflow::target_names$PSUT_final, 
                               path = cache_path, 
                               character_only = TRUE) %>%
  dplyr::mutate(U = matsbyname::sum_byname(U_EIOU, U_feed)) %>% # Here I create U manually, this needs to be done in the drake workflow
  
  dplyr::relocate(U, .after = U_feed)

################################################################################
  
ui <- dashboardPage(
  dashboardHeader(title = "PFU Database"
  ),
  
  dashboardSidebar(
    sidebarMenu(id = "sidebarmenu",
                
                # 1 - Project description
                menuItem("Project Description", tabName = "description", icon = icon("chalkboard-teacher")),
                
                # 2 - Interactive dashboard displaying results, I will eventually split this into multiple dashboards
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"),
                         
                         # Allocations plots
                         menuSubItem("Allocations", tabName = "allocations", icon = icon("chart-pie")),
                         
                         # FU etas and phi plots
                         menuSubItem("Efficiencies", tabName = "eta_phi", icon = icon("chart-line")),
                         
                         # ECC sankey diagram plots
                         menuSubItem("Sankey", tabName = "sankey", icon = icon("project-diagram"))
                         
                ),
                
                # 3 - Data
                menuItem("Data", tabName = "data", icon = icon("database")),
                
                # 4 - SEA Studies
                menuItem("SEA Studies", tabName = "seastudies", icon = icon("database")),
                
                # 5 - Reference and contact information
                menuItem("Citation", tabName = "citation", icon = icon("book-open")), 
                
                # 6 - Direct hyperlink to Github repository (this is probably not necessary)
                menuItem("Github Repository", icon = icon("file-code"), 
                         href = "https://github.com/ZekeMarshall/PFU-Interface/")
                )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "description",
              fluidRow(
                box(title = "Project Description",
                    width = 12,
                    tags$p("This Shiny Dashboard App serves as an interactive interface for the PFU database project")
                ))),
      
      tabItem(tabName = "allocations",
              fluidRow(
                tabBox(
                  title = "Allocation Charts",
                  id = "tabset_allocations",
                  width = 9,
                  tabPanel(
                    title = "Final Energy to Machine and Useful Work Allocations ",
                    plotOutput(outputId = "allocations_plot")
                  )),
                
                box(
                  title = "Variables", status = "warning", solidHeader = TRUE, width = 3,
                  #"Box content here", br(), "More box content",
                  selectizeInput(inputId = "Country_allocations", 
                              label = "Country:",
                              # selected = FALSE,
                              # selectize = FALSE,
                              # size = 6,
                              choices = countries,
                              multiple = TRUE
                              %>% sort()
                  ),
                  selectInput(inputId = "Ef.product_allocations",
                              label = "Final Energy Carrier:",
                              #selected = FALSE,
                              #selectize = FALSE,
                              #size = 6,
                              choices = unique(allocations$Ef.product)
                              %>% sort()
                  ),
                  selectInput(inputId = "Destination_allocations",
                              label = "Destination:",
                              #selected = FALSE,
                              #selectize = FALSE,
                              #size = 6,
                              choices = unique(allocations$Destination)
                              %>% sort()
                  ),
                  selectInput(inputId = "Rows",
                               label = "Rows:",
                               choices = c(1:6)
                               %>% sort()
                  ),
                  selectInput(inputId = "Columns",
                               label = "Columns:",
                               choices = c(1:6)
                               %>% sort()
                  ))
                
                )),
      
      
      
      
      tabItem(tabName = "eta_phi",
              fluidRow(
                tabBox(
                  title = "Plots",
                  id = "tabset1",
                  width = 9,
                  tabPanel(
                    title = "FU Efficiency Plots",
                    plotOutput(outputId = "FU_etas_plot")
                  ),
                  tabPanel(
                    title = "Exergy-to-Energy Ratio Plots",
                    plotOutput(outputId = "phi_plot")
                  )),
                
                box(
                  title = "Variables", 
                  status = "warning", 
                  solidHeader = TRUE, 
                  width = 3,
                  #"Box content here", br(), "More box content",
                  selectizeInput(inputId = "State", # Need to change to EorX throughout
                                 label = "Energy Quantification:",
                                 choices = c(Energy = "eta", Exergy = "eta_X"), # `Exergy-to-energy ratio` = "phi"
                                 multiple = TRUE
                  ),
                  selectizeInput(inputId = "Country", 
                                 label = "Country:",
                                 choices = countries
                                 %>% sort(),
                                 multiple = TRUE
                  ),
                  selectizeInput(inputId = "Machine", # Need to change to FUMachine throughout
                                 label = "Final-to-useful machine:",
                                 choices = unique(etas_and_phis$Machine)
                                 %>% sort(),
                                 multiple = TRUE
                  ),
                  selectizeInput(inputId = "Eu.product",
                                 label = "Useful product:",
                                 choices = unique(etas_and_phis$Eu.product)
                                 %>% sort(),
                                 multiple = TRUE
                  )))),
      
      tabItem(tabName = "sankey",
              fluidRow(
                box(
                  title = "Primary-to-Final Energy flows from Production to Destination",
                  id = "sankey",
                  width = 11,
                  height = 950,
                  sankeyNetworkOutput(outputId = "sankey", height = 900)
                  
                ),
                box(
                  title = "Variables",
                  width = 1,
                  sliderInput(inputId = "Year_sankey", 
                              label = "Year",
                              min = 1960,
                              max = max_year,
                              value = 1960,
                              step = 1,
                              sep = ""
                  ),
                  
                  selectInput(inputId = "Country_sankey", 
                              label = "Country:",
                              choices = countries
                              %>% sort()
                  )))),
      
      tabItem(tabName = "seastudies",
              fluidRow(
                box(title = "Societal Exergy Analysis (SEA) Studies",
                    width = 12,
                    tags$p("This tab contains information on the societal exergy analysis studies which acted as exemplars")
                ))),
      
      tabItem(tabName = "data",
              fluidRow(
                tabBox(
                  title = "Data",
                  id = "tabset_data", width = 9,
                  tabPanel(
                    title = "Data",
                    DT::dataTableOutput(outputId = "data_table")
                    
                  )),
                
                box(
                  title = "Variables",
                  width = 3,
                  selectizeInput(inputId = "Quantity_DT", 
                                 label = "Quantity:",
                                 choices = c(`FU Energy Efficiency` = "eta.fu", 
                                             `FU Exergy Efficiency` = "eta_X", 
                                             `Exergy-to-Energy Ratio` = "phi.u"), 
                                 multiple = TRUE
                  ),
                  
                  selectizeInput(inputId = "Country_DT", 
                                 label = "Country:",
                                 choices = unique(etas_and_phis$Country)
                                 %>% sort(),
                                 multiple = TRUE
                  ),
                  selectizeInput(inputId = "Machine_DT",
                                 label = "Final-to-useful machine:",
                                 choices = unique(etas_and_phis$Machine)
                                 %>% sort(),
                                 multiple = TRUE
                  ),
                  selectizeInput(inputId = "Eu.product_DT",
                                 label = "Useful product:",
                                 choices = unique(etas_and_phis$Eu.product)
                                 %>% sort(),
                                 multiple = TRUE
                  ),
                  
                  downloadButton(outputId = "downloadData", label = "Download")
                  
                ))),
      
      
      tabItem(tabName = "citation",
              fluidRow(
                box(title = "Citation information",
                    width = 12,
                    tags$b("Contributors to the PFU Database"),
                    tags$ul(
                      tags$li("Paul E. Brockway"),
                      tags$li("Matthew K. Heun"),
                      tags$li("Zeke Marshall"),
                      tags$li("Emmanuel Aramendia"))
                ))))))


################################################################################

# The section below contains the server code

server <- function(input, output, session) {
  
  ################################################################################
  
  # The following section is designed to update the list of choices in each of the selection boxes,
  # based on the selection of the previous box
  
  # These observe events update the final-to-useful efficiency tab
  observeEvent(input$Country,  {
    req(input$Country)
    post_country_data <- etas_and_phis %>%
      dplyr::filter(Country %in% input$Country)
    
    updateSelectizeInput(session,
                         inputId = "Machine",
                         choices = sort(unique(etas_and_phis$Machine)))
    
  })
  
  observeEvent(input$Machine,  {
    req(input$Country)
    req(input$Machine)
    post_machine_data <- etas_and_phis %>%
      dplyr::filter(Country %in% input$Country) %>%
      dplyr::filter(Machine %in% input$Machine) 
    
    updateSelectizeInput(session,
                         inputId = "Eu.product", 
                         choices = sort(unique(post_machine_data$Eu.product)))
  })
  
  # These observe events update the exergy-to-energy factor tab
  #observeEvent(input$Country_phi,  {
  # req(input$Country_phi)
  #post_country_data_phi <- master_data %>%
  # dplyr::filter(Country %in% input$Country_phi)
  
  #updateSelectizeInput(session,
  #                    inputId = "Machine_phi",
  #                   choices = sort(unique(post_country_data_phi$Machine)))
  
  #})
  
  #observeEvent(input$Machine_phi,  {
  # req(input$Country_phi)
  #req(input$Machine_phi)
  #post_machine_data_phi<- master_data %>%
  # dplyr::filter(Country %in% input$Country_phi) %>%
  #dplyr::filter(Machine %in% input$Machine_phi) 
  
  #updateSelectizeInput(session,
  #                    inputId = "Eu.product_phi", 
  #                   choices = sort(unique(post_machine_data_phi$Eu.product)))
  #})
  
  
# These observe events update the allocations tab
  observeEvent(input$Country_allocations,  {
    req(input$Country_allocations)
    post_country_data_allocations <- allocations %>%
      dplyr::filter(Country %in% input$Country_allocations)
    
    updateSelectInput(session,
                      inputId = "Ef.product_allocations",
                      choices = sort(unique(post_country_data_allocations$Ef.product)))
    
  })
  
  observeEvent(input$Ef.product_allocations,  {
    req(input$Country_allocations)
    req(input$Ef.product_allocations)
    post_Ef.product_data_allocations <- allocations %>%
      dplyr::filter(Country %in% input$Country_allocations) %>%
      dplyr::filter(Ef.product %in% input$Ef.product_allocations) 
    
    updateSelectInput(session,
                      inputId = "Destination_allocations", 
                      choices = sort(unique(post_Ef.product_data_allocations$Destination)))
  })
  
  
  # These observe events update the data tab  
  observeEvent(input$Country_DT,  {
    req(input$Country_DT)
    post_country_data_DT <- etas_and_phis %>%
      dplyr::filter(Country %in% input$Country_DT)
    
    updateSelectizeInput(session,
                         inputId = "Machine_DT",
                         choices = sort(unique(post_country_data_DT$Machine)))
    
  })
  
  observeEvent(input$Machine_DT,  {
    req(input$Country_DT)
    req(input$Machine_DT)
    post_machine_data_DT <- etas_and_phis %>%
      dplyr::filter(Country %in% input$Country_DT) %>%
      dplyr::filter(Machine %in% input$Machine_DT) 
    
    updateSelectizeInput(session,
                         inputId = "Eu.product_DT", 
                         choices = sort(unique(post_machine_data_DT$Eu.product)))
  })
  
  
################################################################################
  
# This section acts to select the data used for each render object
  
  selected_data_eta <- reactive({
    validate(
      need(input$State != "", "Please select at least one State"), # Need to add this to dplyr::filter code when drake workflow includes exergy eta
      need(input$Country != "", "Please select at least one Country"),
      need(input$Machine != "", "Please select at least one Machine"),
      need(input$Eu.product != "", "Please select at least one form of Useful work")
    )
    dplyr::filter(etas,
                  Country == input$Country,  
                  Machine == input$Machine,
                  Eu.product == input$Eu.product)
  })
  
  selected_data_phi <- reactive({
    validate(
      need(input$Country != "", "Please select at least one Country"),
      need(input$Machine != "", "Please select at least one Machine"),
      need(input$Eu.product != "", "Please select at least one form of Useful work")
    )
    dplyr::filter(phis,
                  Country == input$Country,  
                  Machine == input$Machine,
                  Eu.product == input$Eu.product)
  })
  
  selected_data_allocations <- reactive({
    validate(
      need(input$Country_allocations != "", "Please select atleast one Country"),
      need(input$Ef.product_allocations != "", "Please select one Final energy carrier"),
      need(input$Destination_allocations != "", "Please select one Destination")
    )
    dplyr::filter(allocations,
                  Country %in% input$Country_allocations,  
                  Ef.product == input$Ef.product_allocations,
                  Destination == input$Destination_allocations)
  })
  
  selected_data_sankey <- reactive({
    #validate(
    #need(input$Year_sankey != "", "Please select at least one year"),
    #need(input$Country_sankey != "", "Please select at least one Country"))
    
    dplyr::filter(PSUT_final_data, Country == input$Country_sankey, Year == input$Year_sankey)
    
  })
  
  # What data should be included here, at the moment it is simply etas and phis, need to add:
  # allocations,
  # 
  selected_data_DT <- reactive({
    validate(
      need(input$Quantity_DT != "", "Please select at least one Quantity"),
      need(input$Country_DT != "", "Please select at least one Country"),
      need(input$Machine_DT != "", "Please select at least one Machine"),
      need(input$Eu.product_DT != "", "Please select at least one form of Useful work")
    )
    dplyr::filter(etas_and_phis,
                  Quantity == input$Quantity_DT,
                  Country == input$Country_DT,  
                  Machine == input$Machine_DT,
                  Eu.product == input$Eu.product_DT)
  })
  
  
################################################################################
  
# This section contains code for producing the render objects in the ui
  
  output$FU_etas_plot <- renderPlot(
    height = 600, {
      selected_data_eta = selected_data_eta()
      ggplot2::ggplot(data = selected_data_eta) +
        geom_line(mapping = aes(x = Year, y = .values, colour = Country)) + # I need to figure out how to display multiple combinations e.g. linetype = Machine
        scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
        scale_x_continuous(limits = c(1960, 2020), breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
        MKHthemes::xy_theme()
    })
  
  output$phi_plot <- renderPlot(
    height = 600, {
      selected_data_phi = selected_data_phi()
      ggplot2::ggplot(data = selected_data_phi) +
        geom_line(mapping = aes(x = Year, y = .values, colour = Country)) +
        scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
        scale_x_continuous(limits = c(1960, 2020), breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
        MKHthemes::xy_theme()
    })
  
  output$allocations_plot <- renderPlot(
    height = 600, {
      selected_data_allocations = selected_data_allocations()
      ggplot2::ggplot(data = selected_data_allocations) +
        geom_area(mapping = aes(x = Year, y = .values, 
                                group = Machine_Eu.product, 
                                #colour = Machine_Eu.product,  
                                fill = Machine_Eu.product 
        ),
                  position = "fill") +
        scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
        scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        MKHthemes::xy_theme() + 
        ggplot2::facet_wrap(vars(Country),
                   ncol = input$Columns,
                   nrow = input$Rows
                   )
    })
  
  output$allocations_plot_drakemethod <- renderPlot(
    height = 600, {
      ggplot2::ggplot(data =
                      drake::readd(SEAPSUTWorkflow::target_names$AllocationGraphs, 
                                   path = cache_path, 
                                   character_only = TRUE) %>%
                        dplyr::filter(AllocationGraphs,
                                      Country == input$Country_allocations,  
                                      Ef.product == input$Ef.product_allocations,
                                      Destination == input$Destination_allocations) # %>% dplyr::select(data)
                        
      )
    })
  
  output$sankey <- renderSankeyNetwork({ 
      selected_data_sankey() %>%
        Recca::make_sankey(fontSize = 15,
                           nodeWidth = 30) %>%
        magrittr::extract2("Sankey") %>%
        magrittr::extract2(1)
  })
  
  output$data_table <- DT::renderDataTable({
    selected_data_DT() %>%
      dplyr::select(Country, Quantity, Last.stage, Unit, Machine, Eu.product, Year, .values)
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste("PFU-Data-", Sys.Date(), ".csv", sep = "") # Add options to download in formats other than .csv
    },
    
    content = function(file) {
      utils::write.csv(selected_data_DT(), file, row.names = FALSE, sep = ",")
    })
  
}


################################################################################

# This knits the ui and server components together
shinyApp(ui = ui, server = server)