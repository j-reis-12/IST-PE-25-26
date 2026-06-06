seed <- 2425
M <- 131 # number of samples
n <- 244
lambda <- 4
delta <- 0.11

set.seed(seed)
means <- numeric(M)

# Simulate M samples of size n from a Poisson distribution
for (i in 1:M) {
  sample_i <- rpois(n, lambda)
  means[i] <- mean(sample_i)
}

p_sim <- mean(means >= lambda - delta & means <= lambda + delta) # P(|X̄ - λ| ≤ δ)

# Use Central Limit Theorem to approximate P(|X̄ - λ| ≤ δ) = P(|Z| ≤ δ / SE)
SE <- sqrt(lambda / n)
lower <- ((lambda - delta) - lambda) / SE
upper <- ((lambda + delta) - lambda) / SE

p_clt <- pnorm(upper) - pnorm(lower)

ratio <- p_sim / p_clt
print(round(ratio, 4))