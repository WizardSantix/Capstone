#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Natural Language Predictor"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textInput("string_input",
                   "Input your text:",
                   "I love how the same people are always in Adam Sandler's"),
       actionButton("predict", "Predict"),
       h3("About"),
       em("Type an incomplete sentence and push predict, the internal algorithm will calculate 
          the words that most likely come afterwards.Please do note that the fewer significant
          words that you write, the more coincidences the algorithm will find and higher time 
          might be required for the calculation, please be patient.
          For more info visit us at:"),
       a("https://github.com/WizardSantix/Capstone")
       ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Prediction"),
      tableOutput("table1"),
      plotOutput("plot1")
    )
  )
))
