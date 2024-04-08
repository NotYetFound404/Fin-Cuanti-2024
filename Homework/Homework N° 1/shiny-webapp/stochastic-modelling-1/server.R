library(shiny)
source("C:/Users/RYZEN7/Desktop/Fin-Cuanti-2024/Homework/Homework N° 1/utils.R")
# Define server logic required to draw a histogram
# function(input, output, session) {
# 
#     output$distPlot <- renderPlot({
# 
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white',
#              xlab = 'Waiting time to next eruption (in mins)',
#              main = 'Histogram of waiting times')
# 
#     })
# 
# }


function(input, output) {
  observeEvent(input$simulate, {
    
    
    
    w_setup(input$n, input$T)
    W <- c(0, sapply(2:(n + 1), function(i) {
      W[i - 1] + sqrt(t[i] - t[i - 1]) * Z[i - 1]
    }))
    df["W"] <- W
    df["t"] <- t
    
    output$brownianPlot <- renderPlot({
      ggplot(data = df, aes(x = t, y = W)) +
        geom_step() +
        labs(
          title = "Standard Brownian motion path simulation",
          x = "Time (years)",
          y = "Brownian motion"
        )
    })
  })
}