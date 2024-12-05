library(Estimater)

#beta_initial
result <- mc_int(x_range = c(0, 1), fun = "x^2", B = 10^5)
str(result)
loss_function({1}, {1}, 1)
X <- matrix(c(1, 1, 1, 1, 2, 3, 4, 5), ncol = 2)
y <- c(2, 3, 4, 5)
beta_initial(X, y)


#loss_function
beta <- c(1, 2)
X <- matrix(c(1, 1, 2, 3), ncol = 2)
y <- c(1, 0)
loss_function(beta, X, y)

#generate_predictors
predictors <- generate_predictors(5, 3)
print(predictors)


#generate_response
beta_true <- c(0.5, -1, 2)
X <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 3)
response <- generate_response(X, beta_true)
print(response)

#fit_model
X <- matrix(c(1, 1, 1, 1, 2, 3, 4, 5), ncol = 2)
y <- c(2, 3, 4, 5)
beta_initial(X, y)
optimized_beta <- fit_model(X, y)
print(optimized_beta)



#bootstrap
X <- matrix(c(1, 1, 1, 1, 2, 3, 4, 5), ncol = 2)
y <- c(2, 3, 4, 5)
print(bootstrap(X, y, 20, .05))


#compute_metrics
X <- matrix(c(1, 1, 1, 1, 2, 3, 4, 5), ncol = 2)
y <- c(2, 3, 4, 5)
b <- beta_initial(X,y)
print(compute_metrics(X,b,y))


#logistic_regression_pipeline
n <- 100
lambda <- 3
beta_true <- c(0.5, -1, 2)
B <- 50
alpha <- 0.05
results <- logistic_regression_pipeline(n, lambda, beta_true, B, alpha)
print(results$beta_hat)
print(results$confidence_interval)
print(results$metrics)
