seed <- 2717
set.seed(seed)

file <- "data/Modular_P800.txt"
x <- scan(file)

h0_var <- 1.44 # variance under H0

# subsample
n_sub <- round(0.65 * length(x))
sub <- sample(x, n_sub)

# k values
k_sturges <- nclass.Sturges(sub)
k_fd <- nclass.FD(sub)

# Computes p-value for a given k
pval_fun <- function(k) {
  # equiprobable breaks under H0
  breaks <- qnorm(seq(0, 1, length.out = k + 1), mean = 0, sd = sqrt(h0_var))
  
  obs <- table(cut(sub, breaks = breaks, include.lowest = TRUE))
  exp <- rep(length(sub) / k, k)
  
  chisq <- sum((obs - exp)^2 / exp)
  pchisq(chisq, df = k - 1, lower.tail = FALSE)
}

p1 <- pval_fun(k_sturges)
p2 <- pval_fun(k_fd)

ratio <- max(p1, p2) / min(p1, p2)
print(round(ratio, 4))
