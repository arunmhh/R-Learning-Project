#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Histogram Iris"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("instate",
                  "select a filed",choices = names(iris)
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("iris"),
      textOutput("usefulllable"),
      verbatimTextOutput("Summaryofvariable")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$iris <- renderPlot({
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2]
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(iris[,input$instate],col = "green", type= "black",
         xlim = c(0,10),
         ylim = c(0,40),
         xlab = input$instate,
         ylab = paste("Frequeny of",input$instate),
         main = paste("Comparsion of",input$instate)
         )
         
  })
    
  output$Summaryofvariable <- renderPrint({
    if(input$instate == "Sepal.Length"){iris <- iris[ ,1]}
    if(input$instate == "Sepal.Width"){iris <- iris[ ,2]}
    if(input$instate == "Petal.Length"){iris <- iris[ ,3]}
    if(input$instate == "Petal.Width"){iris <- iris[ ,4]}
    summary(iris)
    
  })
  output$usefulllable <- renderText({
    paste("Summary of",input$instate,":")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)