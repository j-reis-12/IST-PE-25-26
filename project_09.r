seed <- 782
set.seed(seed)

n <- 32
m <- 1500
alpha <- 0.1

a0 <- 3
a1 <- 3.28
b  <- 3

mu0 <- a0 / (a0 + b)
mu1 <- a1 / (a1 + b)

sigma0_sq <- (a0 * b) / ((a0 + b)^2 * (a0 + b + 1))
sigma1_sq <- (a1 * b) / ((a1 + b)^2 * (a1 + b + 1))
SE0 <- sqrt(sigma0_sq / n)
SE1 <- sqrt(sigma1_sq / n)

# One-tailed test
zcrit <- qnorm(1 - alpha)
xcrit <- mu0 + zcrit * SE0
beta <- pnorm((xcrit - mu1) / SE1)

# Simulation for beta-hat
beta_hat_count <- 0

for (i in 1:m) {
  x <- rbeta(n, a1, b)
  xbar <- mean(x)
  Z0 <- (xbar - mu0) / SE0
  if (Z0 <= zcrit) beta_hat_count <- beta_hat_count + 1
}

beta_hat <- beta_hat_count / m
ratio <- beta_hat / beta
print(round(ratio, 4))
