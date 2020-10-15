library(shiny)
library(knitr)

rmdfiles <- c("RMarkdownFile.rmd")
sapply(rmdfiles, knit, quiet = T)

ui <- shinyUI(
  fluidPage(
    withMathJax(includeMarkdown("RMarkdownFile.md"))
  )
)
server <- function(input, output) { }

shinyApp(ui, server)