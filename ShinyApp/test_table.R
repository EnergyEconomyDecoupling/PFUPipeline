# library(DT)
# 
# ui <- basicPage(
#   h2("The mtcars data"),
#   DT::dataTableOutput("machines")
# )
# 
# server <- function(input, output) {
#   output$machines = DT::renderDataTable({
#     machines
#   })
# }
# 
# shinyApp(ui, server)
# 



# Tab repex

library(shiny)
library(shinydashboard)


# Set up the root directory
root <- rprojroot::is_rstudio_project

# Identifies the file path for the drake cache
cache_path <- root$find_file(".drake")

#Load data
etas_and_phis <- drake::readd(SEAPSUTWorkflow::target_names$CompletedEfficiencyTables, 
                              path = cache_path, 
                              character_only = TRUE)

machines <- as.data.frame(unique(etas_and_phis$Machine))




# UI

ui <- dashboardPage(
  dashboardHeader(title = "Machines test"),
  
  dashboardSidebar(
    
    sidebarMenu(id = "sidebarmenu",
                menuItem("Table", tabName = "framework", icon = icon("chalkboard-teacher")),
                
                menuItem("Table2", tabName = "framework2", icon = icon("chalkboard-teacher")
                         
  ))),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "framework2",
              fluidRow(
                box(title = "f2",
                    width = 12,
                    DT::dataTableOutput(outputId = "machines"))
              )
        
      ),
      tabItem(tabName = "framework",
              fluidRow(
                tabBox(title = "PFU Framework",
                       id = "tabset_outline",
                       width = 12,
                       height = 600,
                       tabPanel(
                         title = "Machines",
                         DT::dataTableOutput(outputId = "machines")
                       ),
                       tabPanel(
                         title = "Machines",
                         DT::dataTableOutput(outputId = "machines")
                       )
                )))
    )
  )
)

server <- function(input, output) {
  
  output$machines = DT::renderDataTable({
    machines
  })
  
}

shinyApp(ui, server)