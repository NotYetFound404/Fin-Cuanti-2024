# Load required libraries
library(plotly)

# Set parameters
n_steps <- 10  # Number of time steps
n_paths <- 2  # Number of paths (trajectories)

# Generate Brownian motion increments (standard normal random variables)
increments <- array(rnorm(n_steps * 3 * n_paths), dim = c(n_steps, 3, n_paths))

# Initialize paths with zeros
paths <- array(0, dim = c(n_steps + 1, 3, n_paths))

# Cumulatively sum the increments to obtain the Brownian motion paths
for (j in 1:n_paths) {
  for (i in 1:n_steps) {
    paths[i + 1, , j] <- paths[i, , j] + increments[i, , j]
  }
}

# Create a data frame for plotly with time steps for animation
path_df <- data.frame(X = c(paths[, 1, ]),
                      Y = c(paths[, 2, ]),
                      Z = c(paths[, 3, ]),
                      Path = rep(1:n_paths, each = n_steps + 1),
                      Time = rep(0:n_steps, times = n_paths))

# Print path_df to inspect its contents
print(path_df)

# Create animated 3D plot using plotly
plot_ly(data = path_df, x = ~X, y = ~Y, z = ~Z, color = ~as.factor(Path),
        type = "scatter3d", mode = "lines") %>%
  layout(title = "Animated 3D Brownian Motion Simulation",
         scene = list(xaxis = list(title = "X-axis"),
                      yaxis = list(title = "Y-axis"),
                      zaxis = list(title = "Z-axis")))
