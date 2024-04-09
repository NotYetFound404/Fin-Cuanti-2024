library(shiny)
source("C:/Users/RYZEN7/Desktop/Fin-Cuanti-2024/Homework/Homework N° 1/utils.R")
function(input, output, session) {
  #Debug mode
  # observeEvent(input$run_bm, {
  #   print("Button clicked, running simulation...")
  #   print(paste("n_bm:", input$n_bm))
  #   print(paste("T_bm:", input$T_bm))
  #   print(paste("mu_bm:", input$mu_bm))
  #   print(paste("sigma_bm:", input$sigma_bm))
  #   
  #   tryCatch({
  #     plot_data <- sapply_bm(input$n_bm, input$T_bm, input$mu_bm, input$sigma_bm)
  #     
  #     if (!is.null(plot_data) && !is.null(plot_data$p)) {
  #       print("Plot data generated successfully.")
  #       output$plot_bm <- renderPlot({
  #         print("Rendering plot...")
  #         plot_data$p
  #       })
  #     } else {
  #       print("Error: Plot data is NULL or empty.")
  #     }
  #   }, error = function(e) {
  #     print(paste("Error: An error occurred in the simulation:", e$message))
  #   })
  # })
  observeEvent(input$run_sbm, {
    plot_data <- sapply_sbm(input$n_sbm, input$T_sbm)
    output$plot_sbm <- renderPlot({
      plot_data$p
    })
  })
  
  observeEvent(input$run_bm, {
    plot_data <- sapply_bm(input$n_bm, input$T_bm, input$mu_bm, input$sigma_bm)
    output$plot_bm <- renderPlot({
      plot_data$p
    })
  })
  
  observeEvent(input$run_gbm, {
    plot_data <- floop_gbm(input$n_gbm, input$T_gbm, input$r_gbm, input$sigma_gbm, input$S.1_gbm)
    output$plot_gbm <- renderPlot({
      plot_data$p
    })
  })
}
