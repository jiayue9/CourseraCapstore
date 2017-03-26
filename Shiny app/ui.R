library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Text Prediction"),

    mainPanel(textInput("inputStr", "Input:", ""),
              submitButton("Enter"),
              h3(textOutput("predictStr"))
              )
    )
)