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
    titlePanel("Volcano Examination"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            radioButtons("ChoiceofVariable", "Variable",
                         choices = c("Waiting times", "Eruption times"),
                         selected = "Waiting times"),
            checkboxInput("chooseaxis", "axis")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           textOutput("usefulllable"),
           verbatimTextOutput("Summaryofvariable"),
           plotOutput("ScatterPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      if(input$ChoiceofVariable == "Waiting times"){x <- faithful[, 2]}
      if(input$ChoiceofVariable == "Eruption times"){x <- faithful[, 1]}
        
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'green', border = 'white', 
             xlab = input$ChoiceofVariable, 
             main =paste("Histogram of",input$ChoiceofVariable))
    })
    
    output$Summaryofvariable <- renderPrint({
      if(input$ChoiceofVariable == "Waiting times"){x <- faithful[, 2]}
      if(input$ChoiceofVariable == "Eruption times"){x <- faithful[, 1]}
      summary(x)
    })
    
    output$usefulllable <- renderText({
      paste("Summary of",input$ChoiceofVariable)
    })
    
    output$ScatterPlot <- renderPlot({
      if(input$chooseaxis== FALSE){
      y <- faithful$eruptions
      x <- faithful$waiting
      plot(y~x , xlab= "waiting times", ylab= "Eruption times")}
      
      if(input$chooseaxis== TRUE){
        x <- faithful$eruptions
        y <- faithful$waiting
        plot(y~x , xlab= "Eruption times", ylab= "waiting times")}
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
