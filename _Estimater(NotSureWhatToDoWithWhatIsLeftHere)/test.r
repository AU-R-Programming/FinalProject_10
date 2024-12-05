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

predictors <- generate_predictors(5, 3)
print(predictors)



beta_true <- c(0.5, -1, 2)
X <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 3)
response <- generate_response(X, beta_true)
print(response)


X <- matrix(c(1, 1, 1, 1, 2, 3, 4, 5), ncol = 2)
y <- c(2, 3, 4, 5)
beta_initial(X, y)
optimized_beta <- fit_model(X, y)
print(optimized_beta)




X <- matrix(c(1, 1, 1, 1, 2, 3, 4, 5), ncol = 2)
y <- c(2, 3, 4, 5)
print(bootstrap(X, y, 20, .05))



X <- matrix(c(1, 1, 1, 1, 2, 3, 4, 5), ncol = 2)
y <- c(2, 3, 4, 5)
b <- beta_initial(X,y)
print(compute_metrics(X,b,y))
