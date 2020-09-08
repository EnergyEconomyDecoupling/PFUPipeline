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

# Source init.R file in PFU Database
# How do i link the PFU-Database repository to this repository as the PFU-Database is not a package?????

# Here i temporarily define the countries string found in init.R
countries <- c("World", "ESP", "GRC")

################################################################################




################################################################################

# This approach is only temporary and for demonstration purposes,
# I will link this Shiny app to the PFU Drake work flow with MKH when ready

# Loads .csv data as a DF
etas_data <- read.csv("etas_data.csv")
phi_data <- read.csv("phi_data.csv")


# Combine DF's 
master_data <- etas_data %>%
  dplyr::full_join(phi_data %>% dplyr::select(X, phi), by = "X")
# Add column for eta_X
master_data <- master_data %>%
  dplyr::mutate(eta_X = eta * phi)
# Removes redundant columns
master_data[, c("Method", "Energy.type", "Maximum.values", "Quantity")] <- NULL


# Load allocations .csv data as a DF
allocations_data <- read.csv("allocations_data.csv")
# Adds a combined Machine-Eu.product column
allocations_data$Machine_Eu.product = paste(allocations_data$Machine," - ", allocations_data$Eu.product)


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
                
                # 4 - Reference and contact information
                menuItem("Citation", tabName = "citation", icon = icon("book-open")), 
                
                # 5 - Direct hyperlink to Github repository (this is probably not necessary)
                menuItem("Github Repository", icon = icon("file-code"), 
                         href = "https://github.com/ZekeMarshall/PFU-Interface/"))
    
  ),
  
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
                    title = "Final Energy to Machine and Useful Work",
                    plotOutput(outputId = "allocations_plot")
                  )),
                # tabPanel(
                #  title = "Final Energy to Destination",
                #  plotOutput(outputId = "")
                #)
                # ),
                
                box(
                  title = "Variables", status = "warning", solidHeader = TRUE, width = 3,
                  #"Box content here", br(), "More box content",
                  selectInput(inputId = "Country_allocations", 
                              label = "Country:",
                              # selected = FALSE,
                              # selectize = FALSE,
                              # size = 6,
                              choices = unique(allocations_data$Country)
                              %>% sort()
                  ),
                  selectInput(inputId = "Ef.product_allocations",
                              label = "Final Energy Carrier:",
                              #selected = FALSE,
                              #selectize = FALSE,
                              #size = 6,
                              choices = unique(allocations_data$Ef.product)
                              %>% sort()
                  ),
                  selectInput(inputId = "Destination_allocations",
                              label = "Destination:",
                              #selected = FALSE,
                              #selectize = FALSE,
                              #size = 6,
                              choices = unique(allocations_data$Destination)
                              %>% sort()
                  )))),
      
      
      
      
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
                                 choices = unique(master_data$Country)
                                 %>% sort(),
                                 multiple = TRUE
                  ),
                  selectizeInput(inputId = "Machine", # Need to change to FUMachine throughout
                                 label = "Final-to-useful machine:",
                                 choices = unique(master_data$Machine)
                                 %>% sort(),
                                 multiple = TRUE
                  ),
                  selectizeInput(inputId = "Eu.product",
                                 label = "Useful product:",
                                 choices = unique(master_data$Eu.product)
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
                  sankeyNetworkOutput(outputId = "sankey")
                  
                ),
                box(
                  title = "Variables",
                  width = 1,
                  sliderInput(inputId = "Year_sankey", 
                              label = "Year",
                              min = 1960,
                              max = 2018,
                              value = 1960,
                              step = 1,
                              sep = ""
                  
                  #selectInput(inputId = "Year_sankey", 
                  #            label = "Year:",
                  #            choices = year <- c(paste(1960:2017)) # Need to change this to be dependent on SEAPSUTWorkflow
                  ),
                  
                  selectInput(inputId = "Country_sankey", 
                              label = "Country:",
                              choices = countries
                              %>% sort()
                  )))),
      
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
                  selectizeInput(inputId = "Quantity", 
                                 label = "Quantity:",
                                 choices = c(`FU Energy Efficiency` = "eta", 
                                             `FU Exergy Efficiency` = "eta_X", 
                                             `Exergy-to-Energy Ratio` = "phi"), 
                                 multiple = TRUE
                  ),
                  
                  selectizeInput(inputId = "Country_DT", 
                                 label = "Country:",
                                 choices = unique(master_data$Country)
                                 %>% sort(),
                                 multiple = TRUE
                  ),
                  selectizeInput(inputId = "Machine_DT",
                                 label = "Final-to-useful machine:",
                                 choices = unique(master_data$Machine)
                                 %>% sort(),
                                 multiple = TRUE
                  ),
                  selectizeInput(inputId = "Eu.product_DT",
                                 label = "Useful product:",
                                 choices = unique(master_data$Eu.product)
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
    post_country_data <- master_data %>%
      dplyr::filter(Country %in% input$Country)
    
    updateSelectizeInput(session,
                         inputId = "Machine",
                         choices = sort(unique(post_country_data$Machine)))
    
  })
  
  observeEvent(input$Machine,  {
    req(input$Country)
    req(input$Machine)
    post_machine_data <- master_data %>%
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
    post_country_data_allocations <- allocations_data %>%
      dplyr::filter(Country %in% input$Country_allocations)
    
    updateSelectInput(session,
                      inputId = "Ef.product_allocations",
                      choices = sort(unique(post_country_data_allocations$Ef.product)))
    
  })
  
  observeEvent(input$Ef.product_allocations,  {
    req(input$Country_allocations)
    req(input$Ef.product_allocations)
    post_Ef.product_data_allocations <- allocations_data %>%
      dplyr::filter(Country %in% input$Country_allocations) %>%
      dplyr::filter(Ef.product %in% input$Ef.product_allocations) 
    
    updateSelectInput(session,
                      inputId = "Destination_allocations", 
                      choices = sort(unique(post_Ef.product_data_allocations$Destination)))
  })
  
  
  # These observe events update the data tab  
  observeEvent(input$Country_DT,  {
    req(input$Country_DT)
    post_country_data_DT <- master_data %>%
      dplyr::filter(Country %in% input$Country_DT)
    
    updateSelectizeInput(session,
                         inputId = "Machine_DT",
                         choices = sort(unique(post_country_data_DT$Machine)))
    
  })
  
  observeEvent(input$Machine_DT,  {
    req(input$Country_DT)
    req(input$Machine_DT)
    post_machine_data_DT<- master_data %>%
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
      need(input$State != "", "Please select at least one State"),
      need(input$Country != "", "Please select at least one Country"),
      need(input$Machine != "", "Please select at least one Machine"),
      need(input$Eu.product != "", "Please select at least one form of Useful work")
    )
    dplyr::filter(master_data,
                  Country == input$Country,  
                  Machine == input$Machine,
                  Eu.product == input$Eu.product) %>%
      dplyr::select(X, Country, Last.stage, Unit, Machine, Eu.product, Year, Quantity = input$State)
  })
  
  selected_data_phi <- reactive({
    validate(
      need(input$Country != "", "Please select at least one Country"),
      need(input$Machine != "", "Please select at least one Machine"),
      need(input$Eu.product != "", "Please select at least one form of Useful work")
    )
    dplyr::filter(master_data,
                  Country == input$Country,  
                  Machine == input$Machine,
                  Eu.product == input$Eu.product)
  })
  
  selected_data_allocations <- reactive({
    validate(
      need(input$Country_allocations != "", "Please select one Country"),
      need(input$Ef.product_allocations != "", "Please select one Final energy carrier"),
      need(input$Destination_allocations != "", "Please select one Destination")
    )
    dplyr::filter(allocations_data,
                  Country == input$Country_allocations,  
                  Ef.product == input$Ef.product_allocations,
                  Destination == input$Destination_allocations)
  })
  
  selected_data_sankey <- reactive({
    #validate(
    #need(input$Year_sankey != "", "Please select at least one year"),
    #need(input$Country_sankey != "", "Please select at least one Country"))
    
    # Using SEAPSUTWorkflow::readd_by_country produces the error "Error: 'key' must be a scalar"
    
    drake::readd(SEAPSUTWorkflow::target_names$PSUT_final, 
                             path = cache_path, 
                             character_only = TRUE) %>%
      dplyr::mutate(U = matsbyname::sum_byname(U_EIOU, U_feed)) %>% # Here I create U manually, this needs to be done in the drake workflow
      
      dplyr::relocate(U, .after = U_feed) %>%
    
      dplyr::filter(Country == input$Country_sankey, Year == input$Year_sankey)
  })
  
  selected_data_DT <- reactive({
    validate(
      need(input$Quantity != "", "Please select at least one Quantity"),
      need(input$Country_DT != "", "Please select at least one Country"),
      need(input$Machine_DT != "", "Please select at least one Machine"),
      need(input$Eu.product_DT != "", "Please select at least one form of Useful work")
    )
    dplyr::filter(master_data,
                  Country == input$Country_DT,  
                  Machine == input$Machine_DT,
                  Eu.product == input$Eu.product_DT)
  })
  
  
  ################################################################################
  
  # This section contains code for producing the render objects in the ui
  
  output$FU_etas_plot <- renderPlot(
    height = 600, {
      selected_data_eta2 = selected_data_eta()
      ggplot2::ggplot(data = selected_data_eta2) +
        geom_line(mapping = aes(x = Year, y = Quantity, colour = Country)) + # I need to figure out how to display multiple combinations e.g. linetype = Machine
        scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
        scale_x_continuous(limits = c(1960, 2020), breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
        MKHthemes::xy_theme()
    })
  
  output$phi_plot <- renderPlot(
    height = 600, {
      selected_data_phi2 = selected_data_phi()
      ggplot2::ggplot(data = selected_data_phi2) +
        geom_line(mapping = aes(x = Year, y = phi, colour = Country)) +
        scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
        scale_x_continuous(limits = c(1960, 2020), breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
        MKHthemes::xy_theme()
    })
  
  output$allocations_plot <- renderPlot(
    height = 600, {
      selected_data_allocations2 = selected_data_allocations()
      ggplot2::ggplot(data = selected_data_allocations2) +
        geom_area(mapping = aes(x = Year, y = Allocation, group = Machine_Eu.product, fill = Machine_Eu.product)) +
        scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
        scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        MKHthemes::xy_theme()
    })
  
  output$sankey <- renderSankeyNetwork({
    selected_data_sankey() %>%
      Recca::make_sankey(fontSize = 15, height = "100%", width = "100%") %>%
      magrittr::extract2("Sankey") %>%
      magrittr::extract2(1)
  })
  
  output$data_table <- DT::renderDataTable({
    selected_data_DT() %>%
      dplyr::select(X, Country, Last.stage, Unit, Machine, Eu.product, Year, input$Quantity)
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