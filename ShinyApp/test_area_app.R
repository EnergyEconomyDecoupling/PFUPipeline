# PART 1 - Load Libraries and data
library(shiny)
library(ggplot2)

fourcities <- txhousing %>%
  dplyr::filter(city %in% c("Austin", "Dallas", "Midland", "Odessa")) 

# PART 2 - Define User Interface for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Texas house sales"),
    
    # Show a plot
    mainPanel(
      plotOutput("salesfacetwrap")
    )
)
# PART 3 - Define server logic required to run calculations and draw plots
server <- function(input, output) {
  
  output$salesfacetwrap <- renderPlot(
    height = 800, {
    ggplot2::ggplot(data = fourcities) +
      geom_area(mapping = aes(x = month, y = sales, fill = city)
                #,position = "fill" 
                #,position = "stack" 
      ) +
      facet_wrap(vars(year))
  })
}

# PART 4 - Run the application 
shinyApp(ui = ui, server = server)