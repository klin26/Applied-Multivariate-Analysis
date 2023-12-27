#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # output$distPlot <- renderPlot({
  #     # generate bins based on input$bins from ui.R
  #     x    <- faithful[, 2]
  #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
  # 
  #     # draw the histogram with the specified number of bins
  #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
  #          xlab = 'Waiting time to next eruption (in mins)',
  #          main = 'Histogram of waiting times')
  # })
  
  output$sourcePlot <- renderPlot({
    isScalar <- function(x) ifelse(is.character(x), nchar(x) == 1L, (is.atomic(x) && length(x) == 1L))
    sawtooth <- function(t, width) {
      
      if (length(t) <= 0)
        stop("t must be a vector with length > 0")
      if (!isScalar(width) || width < 0 || width > 1)
        stop("width must be a scalar between 0 and 1")
      
      t <- (t / (2 * pi)) %% 1
      y <- rep(0L, length(t))
      
      if (width != 0) {
        y[t < width] <- 2 * t[t < width] / width - 1
      }
      
      if (width != 1) {
        y[t >= width] <- -2 * (t[t >= width] - width) / (1 - width) + 1
      }
      y
    }
    
    # create source
    time <- seq(0, 8, length.out=1000)
    f1 = input$f1
    f2 = input$f2
    width = input$width
    f3 = input$f3
    s1 =  sin(f1 * time)
    s2 =  sawtooth(f2 * pi * time, width)
    s3 =  sign(sin(f3 *  time))
    S <- cbind(s1,s2,s3)
    set.seed(1)
    noise <- 0.2 * matrix( rnorm(3000,mean=0,sd=1),nrow = 1000)
    # S_noise = S
    S_noise <-  S + noise
    
    A1 = as.data.frame(rbind(c(2.0, 0.5, 1), c(0.25, 8, 0.5), c(1.5, 1/3, 2)))
    A2 = as.data.frame(rbind(c(3, 1/3 , 1), c(1.0, 2.0, 0.5), c(2.0, 0.25, 2.0)))
    A3 = as.data.frame(rbind(c(1, 1, 1), c(0.5, 2.0, 1.0), c(1.5, 1.0, 2.0)))
    A = list(A1=A1,A2=A2,A3=A3)
    
    # create mixture matirx
    if(input$mm=='A1'){
      X = S_noise %*% t(A$A1)
    }else if(input$mm=='A2'){
      X = S_noise %*% t(A$A2)
    }else if(input$mm=='A3'){
      X = S_noise %*% t(A$A3)
    }
    
    
    
    # Independent Component Analysis
    ICA_result = fastICA(X, n.comp = 3)
    S1_extracted = ICA_result$S[, 1]
    S2_extracted = ICA_result$S[, 2]
    S3_extracted = ICA_result$S[, 3]
    par(mfrow = c(3,3))
    plot(1:1000, S_noise[,1], type = "l",xlab = "S1", ylab = "")
    plot(1:1000, S_noise[,2], type = "l", xlab = "S2", ylab = "", main = "Source")
    plot(1:1000, S_noise[,3], type = "l", xlab = "S3", ylab = "")
    plot(1:1000, X[,1], type = "l",xlab = "X1", ylab = "")
    plot(1:1000, X[,2], type = "l", xlab = "X2", ylab = "", main = "Mix")
    plot(1:1000, X[,3], type = "l", xlab = "X3", ylab = "")
    plot(1:1000, S1_extracted, type = "l", xlab = "S'1", ylab = "")
    plot(1:1000, S2_extracted, type = "l", xlab = "S'2", ylab = "", main = "ICA")
    plot(1:1000, S3_extracted, type = "l", xlab = "S'3", ylab = "")
  })
  
}

