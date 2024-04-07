#--------------------------
#Intentare usar (i) replicate y (ii) sapply para evitar loops, alternativamente incluso hacerlo sin loop
#--------------------------

library(ggplot2)
set.seed(2024)
n <- 10000
T <- 1
df <- data.frame(index = seq(1, n+1))
head(df)

## Standard Brownian motion simulation ------------------------

# 1st step
t <- sort(runif(n, min = 0, max = T))
t <- c(0, t)
#estan generando un vector de time, en la clase fue una versión discreta
#los parametros del movimiento browniano es time-dependent7

W <- numeric(n + 1)
W[1] <- 0

# 2nd step
Z <- rnorm(n)

# 3rd step
for (i in 1:n) {
  W[i + 1] <- W[i] + sqrt(t[i + 1] - t[i])*Z[i]
}
df["t"] <- t
df["W"] <- W

#intentar hacer un browninan motion sin loops


ggplot(data = df, aes(x = t, y = W)) +
  geom_step() +
  labs(
    title = "Standard Brownian motion path simulation",
    x = "Time (years)",
    y = "Brownian motion"
  )

## Brownian motion simulation ---------------------------------

mu <- 5
sigma <- 2

X <- mu*t + sigma*W

df["X"] <- X

ggplot(data = df, aes(x = t, y = X)) +
  geom_step() +
  labs(
    title = "Brownian motion path simulation",
    x = "Time (years)",
    y = "Stock price"
  )

## Geometric Brownian motion simulation ------------------------

r <- 0.065
sigma <- 0.45

S <- numeric(n + 1)
S[1] <- 100

# 2nd step
Z <- rnorm(n)

# 3rd step
for (i in 1:n) {
  S[i + 1] <- S[i]*exp((r - 0.5*sigma^2)*(t[i + 1] - t[i]) + 
  sigma*sqrt(t[i + 1] - t[i])*Z[i])
  
}

df["S"] <- S

ggplot(data = df, aes(x = t, y = S)) +
  geom_step() +
  labs(
    title = "Geometric Brownian motion path simulation",
    x = "Time (years)",
    y = "Stock price"
  )


### Homework ---------------------------------------------
# Simulate the Vasicek model simulation. Understand the previous
# simulations and try to implemented it on your way. Explain each
# part of the code. Use the stochastic equation of the Vasick model 
# Hacer la tarea, te da puntos para el final hasta el sabado proximo
# es en grupo

