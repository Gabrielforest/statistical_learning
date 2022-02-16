library(ISLR)
library(boot)

# Bootstrap ---------------------------------------------------------------

# minimum risk investment - Section 5.2 ISLR

alpha <- function(x, y) {
  vx <- var(x)
  vy <- var(y)
  cxy <- cov(x, y)
  (vy - cxy)/ (vx + vy - 2 * cxy)
}

alpha(Portfolio$X, Portfolio$Y)

# What is the standard error of alpha?
alpha_fn <- function(data, index) {
  with(data[index,], alpha(X, Y))
}

alpha_fn(Portfolio, 1:100)

set.seed(1)
alpha_fn(Portfolio, sample(1:100, 100, replace = TRUE))

boot_out <- boot(Portfolio, alpha_fn, R = 1000)
boot_out
plot(boot_out)
