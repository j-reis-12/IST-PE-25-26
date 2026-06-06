seed <- 4732
m  <- 1200
n  <- 31
lam_true <- 9
gamma <- 0.91
alpha <- 1 - gamma

set.seed(seed)

# chi-square quantiles for exact pivotal in CI 1
df <- 2 * n
chi_low  <- qchisq(alpha/2, df)
chi_high <- qchisq(1 - alpha/2, df)

# z for normal pivotal in CI 2
z <- qnorm(1 - alpha/2)

p1_hits <- 0
p2_hits <- 0

for (k in 1:m) {
  x  <- rexp(n, rate = 1/lam_true) # mean = lambda
  xbar <- mean(x)

  # CI 1: exact (chi-square pivotal)
  lam1_low  <- 2 * n * xbar / chi_high
  lam1_high <- 2 * n * xbar / chi_low
  if (lam1_low <= lam_true && lam_true <= lam1_high) p1_hits <- p1_hits + 1

  # CI 2: approximate (normal pivotal)
  lam2_low  <- xbar / (1 + z / sqrt(n))
  lam2_high <- xbar / (1 - z / sqrt(n))
  if (lam2_low <= lam_true && lam_true <= lam2_high) p2_hits <- p2_hits + 1
}

p1 <- p1_hits / m
p2 <- p2_hits / m
ratio <- p1 / p2
print(round(ratio, 4))