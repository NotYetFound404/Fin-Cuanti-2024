library(shiny)
source("C:/Users/RYZEN7/Desktop/Fin-Cuanti-2024/Homework/Homework N° 1/utils.R")
function(input, output, session) {
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
    plot_data <- sapply_gbm(input$n_gbm, input$T_gbm, input$r_gbm, input$sigma_gbm, input$S.1_gbm)
    output$plot_gbm <- renderPlot({
      plot_data$p
    })
  })
  
  #------------------
  vasicek_simulation <- reactiveVal(NULL)
  
  # Function to simulate Vasicek model
  simulate_vasicek_model <- function() {
    simulate_vasicek(input$n_vasicek, input$m_vasicek, input$T_vasicek,
                     input$alpha_vasicek, input$b_vasicek, input$sigma_vasicek)
  }
  
  # Observer to update simulation and plots when the simulation button is clicked
  observeEvent(input$run_vasicek, {
    vasicek_simulation(simulate_vasicek_model())
  })
  
  # Render Vasicek plot
  output$plot_vasicek <- renderPlot({
    if (!is.null(vasicek_simulation())) {
      plot(vasicek_simulation()$p)
    }
  })
  
  # Render statistics table
  output$stats_table <- renderTable({
    if (!is.null(vasicek_simulation())) {
      calculate_stats_vasicek(vasicek_simulation()$df)$stats
    }
  })
  
  # Render histogram plot
  output$hist_plot <- renderPlot({
    if (!is.null(vasicek_simulation())) {
      calculate_stats_vasicek(vasicek_simulation()$df)$hist_plot
    }
  })
}

