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


# Our functions

# loss function, just following the equations and lectures codes 11_12 and 11_14





# Initial beta comes from least-squares
#' @title Initial Beta Function
#'
#' @description Calculates the initial coefficient vector beta_i using the least squares formula
#' @param X The design \code{matrix} of independent variables
#' @param y The \code{vector} of dependent variables (Response variables)
#' @return Initial estimate of the coefficient \code{vector} beta_i
#' @author Destanie Pitt, Hayden Billmann, Ashton Wise
#' @export
#' @examples
#' # Example usage:
#' X <- matrix(c(1, 1, 1, 1, 2, 3, 4, 5), ncol = 2)
#' y <- c(2, 3, 4, 5)
#' beta_initial(X, y)
beta_initial <- function(X,y) {

  X <- as.matrix(X)
  y <- as.matrix(y)

  beta_i <-  solve(t(X) %*% X) %*% t(X) %*% y


  return(beta_i)

}




#' @title Loss Function
#'
#' @description Calculates the negative log likelihood p_i and plugs it into the loss function
#' @param beta A \code{vector} of coefficients associated with the features in xi.
#' @param X The \bold{ith} row of the predictors
#' @param y the \bold{ith} observation of the response
#' @return The calculated loss
#' @author Destanie Pitt, Hayden Billmann, Ashton Wise
#' @export
#' @examples
#' # Example usage:
#'beta <- c(1, 2)
#'X <- matrix(c(1, 1, 2, 3), ncol = 2)
#'y <- c(1, 0)
#'loss_function(beta, X, y)
loss_function <- function(beta, X, y) {
  p_i <- 1 / (1 + exp(-X %*% beta))

  epsilon <- 1e-15
  p_i <- pmax(pmin(p_i, 1 - epsilon), epsilon)

  -sum(y * log(p_i) + (1 - y) * log(1 - p_i))
}





# Generate predictor variables: first column is normal, second column is Poisson with lambda equal to whatever number the user says
# Function to generate predictor variables
#' @title Predictor Variable Generation
#'
#' @description Generate the predictor variables for the data
#' @param n An \code{integer} specifying the number of observations
#' @param lambda The rate parameter for the Poisson distribution
#' @return Predictor variables \code{matrix} with 2 columns
#' \describe{
#'      \item{Column 1}{Intercept column}
#'      \item{Column 2}{Normally distributed predictor variables}
#'      \item{Column 3}{Poisson-distributed predictor variables}
#' }
#' @author Destanie Pitt, Hayden Billmann, Ashton Wise
#' @export
#' @examples
#' # Example usage:
#' # Generate 5 observations with Poisson distribution parameter lambda = 3
#' predictors <- generate_predictors(5, 3)
#' print(predictors)
generate_predictors <- function(n, lambda)
{
  X <- cbind(rnorm(n), rpois(n, lambda = lambda))
  X <- cbind(1, X)  # Add intercept column
  return(X)
}



# Function to generate response variable
#' @title Generate Response Variable
#'
#' @description Calculates the predicted probabilities for each observation, then generates random
#' binary responses for each observation based on the probabilities
#' @param X A \code{matrix} of predictor variables
#' @param beta_true A \code{vector} of true coefficients
#' @return The random binary responses for each observation
#' @author Destanie Pitt, Hayden Billmann, Ashton Wise
#' @export
#' @examples
#' # Example usage:
#' beta_true <- c(0.5, -1, 2)
#' X <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 3)
#' response <- generate_response(X, beta_true)
#' print(response)
generate_response <- function(X, beta_true) {
  p <- 1 / (1 + exp(-X %*% beta_true))
  rbinom(nrow(X), 1, p)
}




# fn argument is the optimization function
#' @title Model Fit
#'
#' @description Estimates the initial coefficients and uses the \code{optim()} function to minimize the
#' loss function and find the optimized coefficients
#' @param X The design \code{matrix} of independent variables
#' @param y The \code{vector} of dependent variables (Response variables)
#' @return The optimized coefficients
#' @author Destanie Pitt, Hayden Billmann, Ashton Wise
#' @export
#' @examples
#' # Example usage:
#' X <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 3)
#' y <- c(0, 1)
#' optimized_beta <- fit_model(X, y)
#' print(optimized_beta)
fit_model <- function(X, y) {
  beta_init <- beta_initial(X, y)
  fit <- optim(beta_init, loss_function, X = X, y = y, method = "BFGS")
  return(fit$par)
}

# Now bootstrapping for confidence intervals

# User enters the number of bootstrap samples
#' @title Bootstrap
#'
#' @description Calculates bootstrap confidence intervals
#' @param X The design \code{matrix} of independent variables
#' @param y The \code{vector} of dependent variables (Response variables)
#' @param B The number of bootstraps. Default is 20
#' @param alpha The significance level to obtain for the 1 - alpha confidence intervals for Beta. Default is .05
#' @return The confidence interval
#' @author Destanie Pitt, Hayden Billmann, Ashton Wise
#' @export
#' @examples
#' # Example usage:
#' X <- matrix(c(1, 1, 1, 1, 2, 3, 4, 5), ncol = 2)
#' y <- c(2, 3, 4, 5)
#' print(bootstrap(X, y, 20, .05))
bootstrap <- function(X, y, B = 20, alpha = 0.05) {
  beta_boot <- matrix(NA, nrow = B, ncol = ncol(X))

  for (b in 1:B) {
    sample <- sample(1:nrow(X), nrow(X), replace = TRUE)
    X_boot <- X[sample, ]
    y_boot <- y[sample]

    beta_boot[b, ] <- fit_model(X_boot, y_boot)
  }

  # Calculate Confidence Intervals
  confidence_interval <- apply(beta_boot, 2, quantile, probs = c(alpha / 2, 1 - alpha / 2))
  return(confidence_interval)
}




# Function to compute confusion matrix and metrics
#' @title Metric Computation
#'
#' @description Calculates the confusion matrix using the predictors and response variables, then
#' calculates the key performance metrics for the logistic regression model
#' @param X The design \code{matrix} of independent variables
#' @param beta_hat The \code{vector} of estimated coefficients
#' @param y The \code{vector} of dependent variables (Response variables)
#' @return The key performance metrics for the logistic regression model as a \code{list}
#' \describe{
#'      \item{prevalence}{The proportion of positive cases in the response}
#'      \item{confusion_matrix}{Representation of predicted vs actual classes}
#'      \item{accuracy}{The proportion of correctly classified instances out of the total instances}
#'      \item{sensitivity}{The proportion of actual positives correctly identified by the model}
#'      \item{specificity}{Measures the proportion of true negatives identified by the model}
#'      \item{false_discovery_rate}{The proportion of predicted positives that are actually negative}
#'      \item{diagnostic_odds_ratio}{Single metric that sumarizes the performance of the diagnostic test}
#' }
#' @author Destanie Pitt, Hayden Billmann, Ashton Wise
#' @export
#' @examples
#' # Example usage:
#' X <- matrix(c(1, 1, 1, 1, 2, 3, 4, 5), ncol = 2)
#' y <- c(2, 3, 4, 5)
#' b <- beta_initial(X,y)
#' print(compute_metrics(X,b,y))
compute_metrics <- function(X, beta_hat, y) {
  p_predicted <- 1 / (1 + exp(-X %*% beta_hat))
  y_predicted <- ifelse(p_predicted > 0.5, 1, 0)
  confusion_matrix <- table(Predicted = y_predicted, Actual = y)

  if (!all(c(0, 1) %in% rownames(confusion_matrix))) {
    if (!('0' %in% rownames(confusion_matrix))) {
      confusion_matrix <- rbind(confusion_matrix, '0' = c(0, 0))
    }
    if (!('1' %in% rownames(confusion_matrix))) {
      confusion_matrix <- rbind(confusion_matrix, '1' = c(0, 0))
    }
  }

  if (!all(c(0, 1) %in% colnames(confusion_matrix))) {
    if (!('0' %in% colnames(confusion_matrix))) {
      confusion_matrix <- cbind(confusion_matrix, '0' = c(0, 0))
    }
    if (!('1' %in% colnames(confusion_matrix))) {
      confusion_matrix <- cbind(confusion_matrix, '1' = c(0, 0))
    }
  }

  prevalence <- sum(y) / length(y)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[, 1])
  false_discovery_rate <- confusion_matrix[2, 1] / sum(confusion_matrix[2, ])
  diagnostic_odds_ratio <- (sensitivity / (1 - specificity)) / ((1 - sensitivity) / specificity)

  metrics <- list(
    prevalence = prevalence,
    confusion_matrix = confusion_matrix,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    false_discovery_rate = false_discovery_rate,
    diagnostic_odds_ratio = diagnostic_odds_ratio
  )

  return(metrics)
}

logistic_regression_pipeline <- function(n, lambda, beta_true, B = 20, alpha = 0.05) {
  # Generate data
  X <- generate_predictors(n, lambda)
  y <- generate_response(X, beta_true)

  # Fit the model
  beta_hat <- fit_model(X, y)

  # Perform bootstrap
  confidence_interval <- bootstrap(X, y, B, alpha)

  # Compute metrics
  metrics <- compute_metrics(X, beta_hat, y)

  # Print results
  cat("Beta Estimates:\n")
  print(beta_hat)
  cat("\nConfidence Intervals:\n")
  print(confidence_interval)
  cat("\nMetrics:\n")
  print(metrics)

  return(list(beta_hat = beta_hat, confidence_interval = confidence_interval, metrics = metrics))
}