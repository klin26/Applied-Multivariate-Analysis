#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(fastICA)
library(ggplot2)
library(gridExtra)
library(gsignal)
library(readr)
library(tuneR)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("ICA"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("f1", "Frequence of Sine wave", min = 1, max = 10, value = 2),
      sliderInput("f2", "Frequence of Sawtooth wave", min = 1, max = 10, value = 1),
      sliderInput("width", "Width of Sawtooth wave", min = 0, max = 1, value = 1),
      sliderInput("f3", "Frequence of Square wave", min = 1, max = 10, value = 1),
      selectInput("mm", "Mixing matrix", list("A1","A2","A3") )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("sourcePlot",width = "90%",height = "800px"),
      
    )
  )
)
