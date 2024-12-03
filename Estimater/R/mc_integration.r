#' @title Simple Monte-Carlo integration
#'
#' @description Compute an approximation of the integral of the function f(x)
#' with respect to dx in the range [a, b] by Monte-Carlo integration using
#' uniform sampling.
#' @param x_range A \code{vector} of dimension 2 used to denote the integration
#' region of interest, i.e. [a, b].
#' @param fun A \code{string} containing the function to be integrated. It
#' is assumed that \code{x} is used as the variable of interest.
#' @param B A \code{numeric} (integer) used to denote the number of simulations.
#' @param seed A \code{numeric} used to control the seed of the random number
#' generator used by this function.
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{I}{Estimated value of the integral}
#'      \item{var}{Estimated variance of the estimator}
#' }
#' @author Stephane Guerrier
#' @importFrom stats runif
#' @export
#' @examples
#' mc_int(x_range = c(0,1), fun = "x^2", B = 10^5)
#' mc_int(x_range = c(0,1), fun = "x^2*sin(x^2/pi)", B = 10^5)
mc_int = function(x_range, fun, B, seed = 1291){
  # A few checks
  # Check x_range
  if (length(x_range) != 2 || x_range[1] >= x_range[2]){
    stop("x_range is incorrectly specified")
  }

  # Check fun
  if (class(fun) != "character"){
    stop("fun is incorrectly specified and should be a character")
  }

  x = mean(x_range)
  test_fun = try(eval(parse(text = fun)), silent = TRUE)
  if (class(test_fun) == "try-error"){
    stop("fun cannot be evaluated")
  }

  # Check B
  if (B < 1){
    error("B is incorrectly specified")
  }

  # Set seed
  set.seed(seed)

  # Compute the length of the interval, i.e. (b-a)
  interval_length = diff(x_range)

  # Let's draw some uniforms to get Ui and Xi
  Ui = runif(B)
  Xi = x_range[1] + Ui*interval_length

  # Compute \hat{I}
  x = Xi
  I_hat = interval_length*mean(eval(parse(text = fun)))

  # Compute \hat{I}_2
  I2_hat = interval_length*mean((eval(parse(text = fun)))^2)
  var_I_hat = (interval_length*I2_hat - I_hat^2)/B

  # Output list
  out = list(I = I_hat, var = var_I_hat,
             fun = fun, x_range = x_range, B = B)
  class(out) = "MCI"
  out
}


#' @export
#' @examples
plot.MCI = function(x, ...){
  obj = x
  x_range = obj$x_range
  fun = obj$fun

  Delta = diff(x_range)
  x_range_graph = c(x_range[2] - 1.15*Delta, x_range[1] + 1.15*Delta)
  x = seq(from = x_range_graph[1], to = x_range_graph[2], length.out = 10^3)
  f_x = eval(parse(text = fun))
  plot(NA, xlim = range(x), ylim = range(f_x), xlab = "x", ylab = "f(x)")
  grid()
  title(paste("Estimated integral: ", round(obj$I,4),
              " (", round(sqrt(obj$var),4),")", sep = ""))
  lines(x, f_x)
  x = seq(from = x_range[1], to = x_range[2], length.out = 10^3)
  f_x = eval(parse(text = fun))
  cols = hcl(h = seq(15, 375, length = 3), l = 65, c = 100, alpha = 0.4)[1:3]
  polygon(c(x, rev(x)), c(rep(0, length(x)), rev(f_x)),
          border = NA, col = cols[1])
  abline(v = x_range[1], lty = 2)
  abline(v = x_range[2], lty = 2)
}


# Our functions

# loss function, just following the equations and lectures codes 11_12 and 11_14
loss_function <- function(beta, X, y) {
  p_i <- 1 / (1 + exp(-X %*% beta))
  -sum(y * log(p_i) + (1 - y) * log(1 - p_i))
}

# User enters the data and we convert it to a numeric so we can do operations
set.seed(1204)
n <- readline(prompt = "Enter the number of observations: ")
n <- as.integer(n)
lambda <- readline(prompt = "Enter a value for lambda: ")
lambda <- as.numeric(lambda)

# Generate predictor variables: first column is normal, second column is Poisson with lambda equal to whatever number the user says
X <- cbind(rnorm(n), rpois(n, lambda = lambda))
beta_true <- c(0.5, -0.3, 0.7)
X <- cbind(1, X)
y <- rbinom(n, 1, 1 / (1 + exp(-X %*% beta_true)))

# Optimization for beta hat

# Initial beta comes from least-squares
beta_initial <- solve(t(X) %*% X) %*% t(X) %*% y


# fn argument is the optimization function
fit <- optim(beta_initial, loss_function, X = X, y = y, method = "BFGS")

# Print the optimized beta values obtained from the minimization
beta_hat <- fit$par
beta_hat

# Now bootstrapping for confidence intervals

# User enters the number of bootstrap samples
B <- readline(prompt = "Enter the number of bootstrap samples (default 20): ")
B <- ifelse(B == "", 20, as.integer(B))
beta_boot <- matrix(NA, nrow = B, ncol = ncol(X))

for (b in 1:B) {
  sample <- sample(1:n, n, replace = TRUE)
  X_boot <- X[sample, ]
  y_boot <- y[sample]

  fit_boot <- optim(beta_initial, loss_function, X = X_boot, y = y_boot, method = "BFGS")
  beta_boot[b, ] <- fit_boot$par
}

# Calculate Confidence Intervals
alpha <- readline(prompt = "Enter significance level (default 0.05): ")
alpha <- ifelse(alpha == "", 0.05, as.numeric(alpha))
confidence_interval <- apply(beta_boot, 2, quantile, probs = c(alpha / 2, 1 - alpha / 2))
confidence_interval

# Confusion Matrix and Metrics

p_predicted <- 1 / (1 + exp(-X %*% beta_hat))  # Predicted probabilities
y_predicted <- ifelse(p_predicted > 0.5, 1, 0)  # Classify predictions
confusion_matrix <- table(Predicted = y_predicted, Actual = y)

# Metrics

prevalence <- sum(y) / length(y)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[, 1])
false_discovery_rate <- confusion_matrix[2, 1] / sum(confusion_matrix[2, ])
diagnostic_odds_ratio <- (sensitivity / (1 - specificity)) / ((1 - sensitivity) / specificity)

# Print Metrics
cat("Prevalence:\n")
print(prevalence)
cat("Confusion Matrix:\n")
print(confusion_matrix)
cat("Metrics:\n")
cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("False Discovery Rate:", false_discovery_rate, "\n")
cat("Diagnostic Odds Ratio:", diagnostic_odds_ratio, "\n")

