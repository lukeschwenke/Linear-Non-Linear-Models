# Luke Schwenke
# Linear & Non-Linear Models
# Generalized Linear Models HW Assignment
# January 2023

# Q3 ----------------------------------------------------------------------
# Assume that the linear predictor, x_i^t β, takes the values between −5 and 5 
# equally spaced by 0.01. If the link function is logit, 
# log (μi/(1-μi)) = x_i^t β, then compute and plot the mean response, μi, 
# against the linear predictor, x_i^t β.
LinearPredictor = seq(-5, 5, 0.01)
head(LinearPredictor)

link_func <- function(x) exp(x)/(1+exp(x))
MeanResponse <- as.vector(link_func(LinearPredictor))

plot(LinearPredictor, MeanResponse, main='Logit Link Function')


# Q4 ----------------------------------------------------------------------
# Assume that the linear predictor, x_i^t β, 
# takes the values between 0.5 and 5 equally spaced by 0.01. 
# If the link function is negative inverse,  -  〖μi〗^(-1) = 〖 x〗_i^t β, 
# compute and plot the mean response, μi, against the linear predictor, x_i^t β. 
LinearPredictor = seq(0.5, 5, 0.01)
head(LinearPredictor)

link_func <- function(x) -1/(x) #-x^(-1)
MeanResponse <- as.vector(link_func(LinearPredictor))

plot(LinearPredictor, MeanResponse, main="Negative Inverse Link Function")

# Q6 ----------------------------------------------------------------------
# Read poisson_mle.csv file. The distribution is Poisson. 
# Reproduce a plot (either in R or Python) which shows how the likelihood 
# varies over different values of lambda and show the MLE of lambda for 
# which the likelihood is the highest (Hint: take lambda sequence from 1 to 20).
pois <- read.csv('poisson_mle.csv')

y <- pois$data
# Assuming we can create lambda
lb <- seq(1, 20, by = 0.01)

# Calculate the log likelihood for various values of p
loglike <- log(lb)*sum(y) - length(y)*lb - sum(log(factorial(y)))

# Calculate the MLE estimate
lb_hat <- mean(y)

# How does likelihood vary with lambda?
plot(lb, loglike, main="Log Likelihood - Poisson",
                  xlab="Lambda", ylab="Log-Likelihood")
abline(h=lb_hat)

# Plot
par(mfrow=c(1,2))
plot(y, type="h", col="red")
abline(h=lb_hat)

hist(y)
abline(v=lb_hat, col="red")
