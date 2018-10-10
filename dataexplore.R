#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(shiny)
library(RColorBrewer)
dataset <- read.csv('~/RNASeqExample/sample_info.csv',header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, row.names = 1)
headerNames=colnames(dataset)
ui <- fluidPage(
  pageWithSidebar(
    
    headerPanel("Data Explorer"),
    
    sidebarPanel(
      selectInput('x', 'X', c("None"=FALSE,headerNames),headerNames[2]),
      selectInput('y', 'Y', c("None"=FALSE,headerNames),headerNames[3]),
      selectInput('fill', 'Fill', c("None"=FALSE,headerNames),headerNames[5]),
      selectInput('size', 'Size', c("None"=FALSE,headerNames),headerNames[5]),
      selectInput('color', 'Color', c("None"=FALSE,headerNames),headerNames[5]),
      selectInput('facet_row', 'Facet Row', c(None='.', headerNames)),
      selectInput('facet_col', 'Facet Column', c(None='.', headerNames)),
      #sliderInput("sizeRange", label = h3("slider"), min=0, max=100, value=50),
      
      checkboxInput('geom_point', 'geom_point',TRUE),
      checkboxInput('geom_dotplot', 'geom_dotplot'),
      checkboxInput('geom_bar', 'geom_bar'),
      checkboxInput('geom_density_2d', 'geom_density_2d'),
      checkboxInput('geom_bin2d', 'geom_bin2d'),
      checkboxInput('geom_violin','geom_violin'),
      checkboxInput('geom_histogram','geom_histogram'),
      checkboxInput('coord_polar','coord_polar')
),
    
    mainPanel(
      plotOutput('plot')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$plot <- renderPlot({
     p <- ggplot(dataset, aes_string(
       x=input$x, fill=input$fill, size=input$size, color=input$color))
     #+scale_size_continuous(range=input$sizeRange)
     if (input$geom_point)
       p <- p + geom_point(aes_string(y=input$y))
     
     facets <- paste(input$facet_row, '~', input$facet_col)
     if (facets != '. ~ .')
       p <- p + facet_grid(facets)
     
     if (input$geom_bar)
       p <- p + geom_bar() 
     if (input$geom_dotplot)
       p <- p + geom_dotplot()
     if (input$geom_density_2d)
       p <- p + geom_density_2d(aes_string(y=input$y))
     if (input$geom_violin)
       p <- p + geom_violin(aes_string(y=input$y))
     if (input$geom_bin2d)
       p <- p + geom_bin2d(aes_string(y=input$y))
     if (input$geom_histogram)
       p <- p + geom_histogram()
     if (input$coord_polar)
       p <- p + coord_polar()
     print(p)
     
  }, height=700)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

