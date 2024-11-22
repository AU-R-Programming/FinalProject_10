library(Estimater)
result <- mc_int(x_range = c(0, 1), fun = "x^2", B = 10^5)
str(result)
plot(result)
