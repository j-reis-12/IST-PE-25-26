library(extraDistr)

mu <- 3150
sigma <- 560

seed <- 3730
n <- 2700

# Compute L_A and L_E
LA <- mu - sigma * log(-log(0.9)) # derivated F(x) function from f(x)
LE <- LA + 1000

# Probability calculation; P(X > LE | X > LA) = P(X > LE) / P(X > LA)
p1 <- (1 - pgumbel(LE, mu, sigma)) / (1 - pgumbel(LA, mu, sigma))

# Simulation
set.seed(seed)
X <- rgumbel(n, mu, sigma)

# Proportion of X > LE among X > LA
p2 <- sum(X > LE) / sum(X > LA)

# Ratio
ratio <- p2 / p1
print(round(ratio, 4))