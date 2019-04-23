#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hult Global Campus Rotation System is for WHO?"),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Frequency", plotOutput("wordfrequency",  width = "100%", height = "1000px")),
      tabPanel("Wordcloud", plotOutput("total",  width = "100%", height = "1000px")),
      tabPanel("Q4 How did you spend last SUMMER? (Yes I rotated)", plotOutput("Q14",  width = "100%", height = "1000px")),
      tabPanel("Q5 Why Hult? (Yes I rotated)", plotOutput("Q15",  width = "100%", height = "1000px")),
      tabPanel("Q4 How did you spend last SUMMER? (No I did not)", plotOutput("Q04",  width = "100%", height = "1000px")),
      tabPanel("Q5 Why Hult? (No I did not)", plotOutput("Q05",  width = "100%", height = "1000px")),
      tabPanel("Pairwise Correlation", plotOutput("correlation",  width = "100%", height = "1000px"))
    )
    
  )
)
)
