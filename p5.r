seed <- 1794
K <- 100
n <- 800
props <- numeric(K) # stores the proportions from each simulation

set.seed(seed)

for (k in 1:K) {
  u <- runif(n)
  x <- sqrt(u)
  z <- runif(n)
  y <- x + z * (1 - x) # derivated from F(y) = (y - a) / (b - a) where a = x and b = 1
  props[k] <- mean(y > 1.5 * x)
}

p_sim <- mean(props)
p_exact <- integrate(function(x) 2*x*(1 - 1.5*x)/(1 - x), lower = 0, upper = 2/3)$value
ratio <- p_sim / p_exact
print(round(ratio, 4))