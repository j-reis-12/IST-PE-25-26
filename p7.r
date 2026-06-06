x <- c(1.53, 3.09, 1.52, 2.71, 1.8, 3.22, 2.29, 3.21, 2.79, 2.41, 2.1)
n <- length(x)

# Equation for alpha
alpha_expression <- function(alpha) {
  term1 <- 1/alpha
  term2 <- mean(log(x))
  term3 <- sum(x^alpha * log(x)) / sum(x^alpha)
  term1 + term2 - term3
}

# Solve for alpha in [4.6, 7.4]
alpha_hat <- uniroot(alpha_expression, c(4.6, 7.4))$root

# Solve for lambda
lambda_hat <- ( mean(x^alpha_hat) )^(1/alpha_hat)

# Solve for median
median_hat <- lambda_hat * (log(2))^(1/alpha_hat)
print(round(median_hat, 2))