library(shiny)
fluidPage(
  titlePanel("Financial Simulations"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Standard Brownian Motion (SBM)",
                 numericInput("n_sbm", "Number of Steps:", value = 100),
                 numericInput("T_sbm", "Time Horizon (years):", value = 1),
                 actionButton("run_sbm", "Run Simulation")
        ),
        tabPanel("Brownian Motion (BM)",
                 numericInput("n_bm", "Number of Steps:", value = 100),
                 numericInput("T_bm", "Time Horizon (years):", value = 1),
                 numericInput("mu_bm", "Mean (μ):", value = 0.05, min = 0, max = 1, step = 0.01),
                 numericInput("sigma_bm", "Volatility (σ):", value = 0.2, min = 0, max = 1, step = 0.01),
                 actionButton("run_bm", "Run Simulation")
        ),
        tabPanel("Geometric Brownian Motion (GBM)",
                 numericInput("n_gbm", "Number of Steps:", value = 100),
                 numericInput("T_gbm", "Time Horizon (years):", value = 1),
                 numericInput("r_gbm", "Risk-free Rate (r):", value = 0.03, min = 0, max = 1, step = 0.01),
                 numericInput("sigma_gbm", "Volatility (σ):", value = 0.2, min = 0, max = 1, step = 0.01),
                 numericInput("S.1_gbm", "Initial Price:", value = 100),
                 actionButton("run_gbm", "Run Simulation")
        )
      )
    ),
    mainPanel(
      plotOutput("plot_sbm"),
      plotOutput("plot_bm"),
      plotOutput("plot_gbm")
    )
  )
)
