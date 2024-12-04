library(Estimater)
result <- mc_int(x_range = c(0, 1), fun = "x^2", B = 10^5)
str(result)
loss_function({1}, {1}, 1)
X <- matrix(c(1, 1, 1, 1, 2, 3, 4, 5), ncol = 2)
y <- c(2, 3, 4, 5)
beta_initial(X, y)



beta <- c(1, 2)
X <- matrix(c(1, 1, 2, 3), ncol = 2)
y <- c(1, 0)
loss_function(beta, X, y)

