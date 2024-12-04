# Set the number of observations
n <- 100

# Set a seed for reproducibility
set.seed(123)

# Generate predictor variables: first column is normal, second column is Poisson with lambda = 3
X <- cbind(rnorm(n), rpois(n, lambda = 3))

# Define the true coefficients (beta) for the linear model, including the intercept term
beta <- c(2, -3, 1)

# Add an intercept column of 1s to the design matrix X for calculating y
design <- cbind(rep(1, n), X)

# Generate response variable 'y' as a linear combination of X and beta, adding random normal noise
y <- design %*% beta + rnorm(n)

# Create a data frame containing y and predictors for the regression model
data <- data.frame(cbind(y , X))
names(data) <- c("y", "x1", "x2")

# Fit a linear model to the generated data and print summary of model coefficients
fit_lm <- lm(y ~ ., data = data)
summary(fit_lm)

# Compute the least squares solution manually for verification
solve(t(design) %*% design) %*% t(design) %*% y

# Set the number of bootstrap samples for confidence interval estimation
B <- 100
n <- nrow(data)

# Initialize a matrix to store bootstrapped estimates of coefficients
beta_hat <- matrix(NA, B, 3)

# Perform bootstrapping to generate confidence intervals for coefficients
for(b in 1:B) {
  
  # Resample the data with replacement
  boot_data <- data[sample(1:n, n, replace = T), ]
  
  # Fit linear model to the bootstrap sample
  boot_lm <- lm(y ~ ., data = boot_data)
  
  # Store the coefficients from the bootstrapped model
  beta_hat[b, ] <- boot_lm$coefficients
  
}

# Visualize distribution of the intercept estimates using a boxplot
boxplot(beta_hat[, 1])

# Calculate 95% confidence intervals for the intercept estimate
quantile(beta_hat[, 1], probs = c(0.025, 0.975))

# Calculate 95% confidence intervals for all coefficients
apply(beta_hat, 2, quantile, probs = c(0.025, 0.975))





### Simulation Study: Impact of Contaminated Data on Regression Estimates ###

# Set the number of observations
n <- 100

# Set a seed for reproducibility
set.seed(123)

# Generate predictor variables: first column is normal, second column is Poisson with lambda = 3
X <- cbind(rnorm(n), rpois(n, lambda = 3))

# Define true coefficients for the linear model
beta <- c(2, -3, 1)

# Add an intercept column of 1s to the design matrix X
design <- cbind(rep(1, n), X)

# Set number of iterations for the simulation study
H <- 100

# Initialize matrices to store coefficient estimates with and without contamination
beta_hat <- beta_hat_cont <- matrix(NA, H, 3)

# Loop to generate regression estimates over multiple iterations with and without contamination
for(h in 1:H) {
  
  # Generate uncontaminated response variable y with random normal noise
  y <- design %*% beta + rnorm(n)
  
  # Copy y to y_star and introduce contamination by setting 5 random y values to 20
  y_star <- y
  y_star[sample(1:n, 5)] <- 20
  
  # Fit a linear model to the uncontaminated data
  fit <- lm(y ~ X[,1] + X[, 2])
  
  # Fit a linear model to the contaminated data
  fit_cont <- lm(y_star ~ X[,1] + X[, 2])
  
  # Store coefficients from both models
  beta_hat[h, ] <- fit$coefficients
  beta_hat_cont[h, ] <- fit_cont$coefficients
  
}

# Visualize the distribution of coefficients with uncontaminated data
boxplot(beta_hat)
abline(h = beta[1], col = "green")  # True intercept
abline(h = beta[2], col = "red")    # True coefficient for X[,1]
abline(h = beta[3], col = "blue")   # True coefficient for X[,2]

# Visualize the distribution of coefficients with contaminated data
boxplot(beta_hat_cont)
abline(h = beta[1], col = "green")  # True intercept
abline(h = beta[2], col = "red")    # True coefficient for X[,1]
abline(h = beta[3], col = "blue")   # True coefficient for X[,2]
