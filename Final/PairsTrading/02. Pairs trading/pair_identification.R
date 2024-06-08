# Load necessary libraries
library(tidyverse)
library(tseries)

# Load the close prices data
load("closePrices.RData")

# Function to identify trading pairs
identify_pairs <- function(closePrices) {
  train <- log(closePrices[1:220])
  
  left_side <- NULL
  right_side <- NULL
  correlation <- NULL
  beta <- NULL
  pvalue <- NULL
  symbols <- colnames(train)
  
  for (i in 1:length(symbols)) {
    for (j in 1:length(symbols)) {
      if (i > j) {
        left_side <- c(left_side, symbols[i])
        right_side <- c(right_side, symbols[j])
        correlation <- c(correlation, cor(train[, symbols[i]], train[, symbols[j]]))
        
        m <- lm(train[, symbols[i]] ~ train[, symbols[j]] - 1)
        beta <- c(beta, as.numeric(coef(m)[1]))
        
        sprd <- residuals(m)
        pvalue <- c(pvalue, adf.test(sprd, alternative = "stationary", k = 0)$p.value)
      }
    }
  }
  
  df <- data.frame(left_side, right_side, correlation, beta, pvalue)
  mypairs <- df %>% filter(pvalue <= 0.05, correlation > 0.95) %>% arrange(-correlation)
  
  return(mypairs)
}

# Identify pairs
mypairs <- identify_pairs(closePrices)

# Save pairs to an RData file
save(mypairs, file = "mypairs.RData")

