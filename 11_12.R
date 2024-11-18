# Define the least squares loss function
loss_ls <- function(X, y, beta) {
  
  # Add a column of 1s to X to include the intercept in the model
  X <- cbind(rep(1, n), X)
  
  # Calculate the sum of squared residuals between observed values (y) and predicted values (X %*% beta)
  sum((y - X %*% beta)^2)
}

# Set the number of observations
n <- 100

# Set a seed for reproducibility
set.seed(123)

# Generate predictor variables: first column is normal, second column is Poisson with lambda = 3
X <- cbind(rnorm(n), rpois(n, lambda = 3))

# Define the true coefficients (beta) for the linear model, including the intercept term
beta <- c(2, -3, 1)

# Add an intercept column of 1s to the design matrix X
design <- cbind(rep(1, n), X)

# Generate response variable 'y' as a linear combination of X and beta, adding random normal noise
y <- design %*% beta + rnorm(n)

y

B <- 100

beta_hat <- beta_hat_cont <- matrix(NA, B, 3)


for (i in 1:B) {
  
  y <- design %*% beta + rnorm(n)
  y_star <- y
  y_star[sample(1:n,5,)] <- 20
  
  fit <- lm(y~X[,1] + X[,2])
  fit_cont <- lm(y_star~~X[,1] + X[,2])
  
  beta_hat[i,] <- fit$coefficients
  beat_hat_cont[i,] <- fit_cont$coefficients
  
}


ShellExec  <- function(x) {
  # replacement for shell.exe (doesn't exist on MAC)
  if (exists("shell.exec",where = "package:base"))
    return(base::shell.exec(x))
  comm <- paste("open",x)
  return(system(comm))
}
Samples <- 1:3
SampleVideo <- sample(Samples, 1, FALSE)
OpenVid <- 
  if(SampleVideo == 1) {
    "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
  } else if(SampleVideo == 2) {
    "https://www.youtube.com/watch?v=CsGYh8AacgY&t=12s"
  } else {
    "https://www.youtube.com/watch?v=1g9sneS2MF4" 
  }
ShellExec(OpenVid)
SampleVideo <- sample(Samples[!1:3 %in% SampleVideo], 1, FALSE)


install.packages("plotrix")
install.packages("emojifont")

# Required Libraries
library(ggplot2)
library(plotrix)
library(emojifont)

# Parameters
set.seed(42)
N <- 20               # Number of bacteria
n_steps <- 150        # Number of time steps
radius <- 3           # Radius of the food circle
food_center <- c(0, 0) # Center of the food circle
a <- 0                # Minimum step size
b <- 2                # Maximum step size

# Initialize positions
positions <- data.frame(
  bacterium = rep(1:N, each = n_steps + 1),
  step = rep(0:n_steps, times = N),
  x = NA,
  y = NA
)

# Initial positions
positions$x[positions$step == 0] <- runif(N, -9, 9)
positions$y[positions$step == 0] <- runif(N, -9, 9)

# Simulate random walk
for (i in 1:N) {
  for (t in 1:n_steps) {
    current <- positions[positions$bacterium == i & positions$step == t - 1, ]
    x <- current$x
    y <- current$y
    
    if (sqrt((x - food_center[1])^2 + (y - food_center[2])^2) <= radius) {
      # If inside the food circle, stay within it
      theta <- runif(1, 0, 2 * pi)
      x_new <- food_center[1] + radius * cos(theta)
      y_new <- food_center[2] + radius * sin(theta)
    } else {
      # Otherwise, continue random walk
      step_length <- runif(1, a, b)
      angle <- runif(1, 0, 2 * pi)
      x_new <- x + step_length * cos(angle)
      y_new <- y + step_length * sin(angle)
    }
    
    # Record new position
    positions$x[positions$bacterium == i & positions$step == t] <- x_new
    positions$y[positions$bacterium == i & positions$step == t] <- y_new
  }
}

# Plot the trajectory
food_circle <- data.frame(
  x = food_center[1],
  y = food_center[2],
  radius = radius
)

ggplot() +
  geom_path(data = positions, aes(x = x, y = y, group = bacterium, color = as.factor(bacterium)), alpha = 0.5) +
  geom_point(data = positions[positions$step == 0, ], aes(x = x, y = y), color = "blue", size = 2, shape = 19) +
  geom_point(data = positions[positions$step == n_steps, ], aes(x = x, y = y), color = "red", size = 2, shape = 19) +
  annotate("text", x = 0, y = 0, label = "SUGAR", size = 6, color = "white", fontface = "bold") +
  draw.circle(food_center[1], food_center[2], radius, col = "orange", border = "orange", lwd = 2, lty = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Bacteria Motility", x = "X Position", y = "Y Position")






# Required Libraries
library(ggplot2)
library(dplyr)

# Parameters
set.seed(42)
N <- 20               # Number of bacteria
n_steps <- 150        # Number of time steps
radius <- 3           # Radius of the food circle
food_center <- c(0, 0) # Center of the food circle
a <- 0                # Minimum step size
b <- 2                # Maximum step size

# Initialize positions
positions <- data.frame(
  bacterium = rep(1:N, each = n_steps + 1),
  step = rep(0:n_steps, times = N),
  x = NA,
  y = NA
)

# Initial positions
positions$x[positions$step == 0] <- runif(N, -9, 9)
positions$y[positions$step == 0] <- runif(N, -9, 9)

# Simulate random walk
for (i in 1:N) {
  for (t in 1:n_steps) {
    current <- positions[positions$bacterium == i & positions$step == t - 1, ]
    x <- current$x
    y <- current$y
    
    if (sqrt((x - food_center[1])^2 + (y - food_center[2])^2) <= radius) {
      # If inside the food circle, stay within it
      theta <- runif(1, 0, 2 * pi)
      x_new <- food_center[1] + radius * cos(theta)
      y_new <- food_center[2] + radius * sin(theta)
    } else {
      # Otherwise, continue random walk
      step_length <- runif(1, a, b)
      angle <- runif(1, 0, 2 * pi)
      x_new <- x + step_length * cos(angle)
      y_new <- y + step_length * sin(angle)
    }
    
    # Record new position
    positions$x[positions$bacterium == i & positions$step == t] <- x_new
    positions$y[positions$bacterium == i & positions$step == t] <- y_new
  }
}

# Generate Circle Points for ggplot
circle_points <- data.frame(
  x = food_center[1] + radius * cos(seq(0, 2 * pi, length.out = 100)),
  y = food_center[2] + radius * sin(seq(0, 2 * pi, length.out = 100))
)

# Plot the trajectory
ggplot() +
  geom_path(data = positions, aes(x = x, y = y, group = bacterium, color = as.factor(bacterium)), alpha = 0.5) +
  geom_point(data = positions[positions$step == 0, ], aes(x = x, y = y), color = "blue", size = 2, shape = 19) +
  geom_point(data = positions[positions$step == n_steps, ], aes(x = x, y = y), color = "red", size = 2, shape = 19) +
  geom_polygon(data = circle_points, aes(x = x, y = y), fill = "orange", alpha = 0.3, color = "orange") +
  annotate("text", x = 0, y = 0, label = "SUGAR", size = 6, color = "white", fontface = "bold") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Bacteria Motility", x = "X Position", y = "Y Position")









# Load libraries
library(ggplot2)

# Generate random bacteria positions
set.seed(123) # For reproducibility
bacteria <- data.frame(
  x = runif(30, -10, 10),
  y = runif(30, -10, 10)
)

# Plot
ggplot() +
  geom_point(data = bacteria, aes(x = x, y = y), shape = 21, fill = "black", size = 5) +
  geom_point(aes(x = 0, y = 0), size = 30, shape = 21, fill = "orange", color = "black") +
  annotate("text", x = 0, y = 0, label = "SUGAR", color = "white", size = 8, fontface = "bold") +
  coord_fixed(xlim = c(-12, 12), ylim = c(-12, 12)) +
  labs(title = "Bacteria Motility", x = "X Position", y = "Y Position") +
  theme_minimal()


# Function to simulate random walk
random_walk <- function(n, start = c(0, 0)) {
  x <- cumsum(runif(n, -1, 1)) + start[1]
  y <- cumsum(runif(n, -1, 1)) + start[2]
  data.frame(x = x, y = y)
}

# Simulate bacteria trajectories
set.seed(123)
trajectories <- do.call(rbind, lapply(1:30, function(i) {
  traj <- random_walk(100, start = bacteria[i, ])
  traj$id <- i
  traj
}))

# Plot trajectories
ggplot() +
  geom_path(data = trajectories, aes(x = x, y = y, group = id, color = as.factor(id)), alpha = 0.6) +
  geom_point(data = bacteria, aes(x = x, y = y), shape = 21, fill = "black", size = 5) +
  geom_point(aes(x = 0, y = 0), size = 30, shape = 21, fill = "orange", color = "black") +
  annotate("text", x = 0, y = 0, label = "SUGAR", color = "white", size = 8, fontface = "bold") +
  coord_fixed(xlim = c(-12, 12), ylim = c(-12, 12)) +
  labs(title = "Bacteria Motility", x = "X Position", y = "Y Position") +
  theme_minimal() +
  theme(legend.position = "none")

