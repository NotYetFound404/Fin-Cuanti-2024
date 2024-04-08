library(shiny)
# Define UI for application that draws a histogram
# fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#             plotOutput("distPlot")
#         )
#     )
# )
fluidPage(
  titlePanel("Brownian Motion Simulation"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Number of Steps:", value = 100),
      numericInput("T", "End Time (years):", value = 1),
      actionButton("simulate", "Simulate")
    ),
    mainPanel(
      plotOutput("brownianPlot")
    )
  )
)