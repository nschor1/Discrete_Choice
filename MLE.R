
# ========
# Packages
# ========

pacman::p_load(tidyverse)

# ====================
# Generate Pseudo-Data
# ====================

set.seed(1)


x <- rnorm(1000)
z <- -3*x 
pr <- 1/(1 + exp(-z))
y <- rbinom(1000, 1, pr)


# ==============================================================
# Write Down Objective Function & Optimize to Recover Parameters
# ==============================================================


standardizer <- 1 # Train, Discrete Choice Methods with Simulation, p. 24


LL <- function(beta) sum(y * log(exp(t(x) * (beta / standardizer)) / (1 + exp(t(x) * (beta / standardizer)))) + (1 - y) * log(1 / (1 + exp(t(x) * (beta / standardizer)))))


optimize(f = LL,
         interval = c(-4, 10),
         maximum = T)

search_values <- seq(-10, 10, .01)
LL_values <- map_dbl(search_values, LL) 

max_point <- tibble(x = search_values[which.max(LL_values)],
                    y = LL_values[which.max(LL_values)])
windows()
ggplot() +
  geom_line(aes(x = search_values, y = LL_values)) +
  geom_vline(xintercept = (z / x)[1], color = "red") + # true beta
  geom_point(aes(x = search_values[which.max(LL_values)], y = LL_values[which.max(LL_values)]), color = "green", size = 2) +
  xlab(expression(beta)) +
  ylab("Log-Likelihood") +
  annotate(geom = "curve", x = max_point$x + .1, y = -500, xend = max_point$x, yend = max_point$y, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = max_point$x - .05, y = -505, label = max_point$x, hjust = "left") +           
             # geom_text(data = max_point, aes(x = x, y = y, label = paste(max_point$x)), nudge_y = -500, nudge_x = .25) +
  ggtitle(paste("True", expression(Beta), "and Simulated", expression(Beta))) +
  theme_bw()

# =====================================
# Monte Carlo for Distribution of Beta
# =====================================

set.seed(1234)

Make_Histogram <- function() {

  x <<- rnorm(1000)
  z <<- 5*x 
  pr <- 1/(1 + exp(-z))
  y <- rbinom(1000, 1, pr)


# ==============================================================
# Write Down Objective Function & Optimize to Recover Parameters
# ==============================================================


  standardizer <- 1 # Train, Discrete Choice Methods with Simulation, p. 24
  
  
  LL <- function(beta) sum(y * log(exp(t(x) * (beta / standardizer)) / (1 + exp(t(x) * (beta / standardizer)))) + (1 - y) * log(1 / (1 + exp(t(x) * (beta / standardizer)))))
  
  
  optimizing <- optimize(f = LL,
                         interval = c(-4, 10),
                         maximum = T)
  
  optimizing$maximum
} 


# =====================================
# Make Draws with Histogram of Results
# =====================================

betas <- replicate(n = 10000, Make_Histogram())

windows()
ggplot() +
  geom_histogram(aes(x = betas), bins = 50) +
  geom_vline(xintercept = (z / x)[1], color = "red") +
  xlab(expression(beta)) +
  ggtitle("Distribution of Beta") +
  theme_bw()


# # ==============================================================================================
# # Function Factory Approach, https://adv-r.hadley.nz/function-factories.html#MLE Section 10.4.3
# # ==============================================================================================
# 
# Log_Likelihood <- function(x, y) {
#   
#   
#   function(beta) {
#     
#     exponent <- x * beta
#     
#     return(sum(y * log(exp(x * beta) / (1 + exp(x * beta))) + (1 - y) * log(1 / (1 + exp(x * beta)))))
#     
#   }
#   
# }
# 
# 
# 
# h <- seq(from = -10, to = 10, by = .1)
# 
# r <- map_dbl(h, LL)
# 
# plot(h, r)
# 
# glm(y ~ x, family = "binomial")
# 
# 
# optimize(f = Log_Likelihood,
#          x = x1, 
#          y = y,
#          interval = c(-3, 0))
# 
# 
# log(exp(beta * x1) / (1 + exp(beta * x1))) 
# log(beta * x / (1 + (beta * x)))
# 
# 
# Log_Likelihood(x1, y)
# 
# LL <- function(y, pr) {
#   
#   
#   return(sum(y * log(pr)) + (1 - y) * log(1 / (pr)))
#   
# }
# 
# optimise(LL, c(-10, 10), y = y, pr = pr, maximum = T)
# 
# 
# 
# optim(par = 1, fn = LL, y = y, pr = pr)
# 
# 
# exponent <- x1*-2
# 
# sum(y * log(pr) + (1 - y) * log(1 - pr))
# 
# 
# # ========================
# # Functions to investigate
# # ========================
# 
# 
# # nlm()
# # optim()
# 
# 
# 
# 
# # From http://courses.atlas.illinois.edu/spring2016/STAT/STAT200/RProgramming/Maximum_Likelihood.html#fn04
# 
# 
# loglike <- function(beta0,beta1,x,y) {
#   # Define the logistic function 
#   logit <- function(x,beta0,beta1) {
#     1/(1 + exp(-beta0-beta1*x))
#   }
#   
#   p <- logit(x,beta0,beta1)
#   sum( y*log(p) + (1-y)*log(1-p) )
# }
# 
# 
# 
# 
# 
# # Find the coefficients of logistic regression using the Monte Carlo method.
# # N is the number of beta's to choose
# # beta0_range is a numeric vector of length 2:
# #    choose beta0 in the range (beta0_range[1], beta0_range[2])
# # beta1_range is a numeric vector of length 2:
# #    choose beta1 in the range (beta1_range[1], beta1_range[2])
# #
# # Find beta0 and beta1 by minimizing the log-likelihood functon
# #
# find_betaMC <- function(N, beta0_range, beta1_range, x, y) {
#   # Generate N random values of beta0 and beta1 in the specified ranges.
#   many_beta0 <- runif(N, beta0_range[1], beta0_range[2])
#   many_beta1 <- runif(N, beta1_range[1], beta1_range[2])
#   # Calculate the ln(likelihood) for these beta0 and beta1
#   loglikelihood <- mapply(loglike, many_beta0,many_beta1, MoreArgs=list(x=x,y=y))
#   # Search for maximum
#   ind_max <- which.max(loglikelihood)
#   output <- c(many_beta0[ind_max],many_beta1[ind_max],loglikelihood[ind_max])
#   names(output) <- c("beta0", "beta1", "ln(likelihood)")
#   output
# }
# 
# 
# set.seed(1)
# 
# x1 <- rnorm(1000)
# x2 <- rnorm(1000)   
# z <- 1 - 2*x1
# pr <- 1/(1 + exp(-z))
# y <- rbinom(1000, 1, pr)
# 
# b1 <- replicate(1000, find_betaMC(N = 2, beta0_range = c(-2,2), beta1_range = c(-2.5, -1.5), x = x1, y = y))
# 
# t(b1)[t(b1)[, "ln(likelihood)"] %>% which.max(), ]
# t(b1)[, "beta1"] %>% mean()
# 





# =============
# Miscellaneous
# =============

# x1 <- rnorm(1000)
# x2 <- rnorm(1000)   
# z <- 1 - 2*x1 + 3*x2
# pr <- 1/(1 + exp(-z))
# y <- rbinom(1000, 1, pr)
# 
# 
# Log_Likelihood <- sum(y * log(pr) + (1 - y) * log(1 - pr))
# 
# LL <- function(p) sum()
# 
# optimize(Log_Likelihood, lower = -5, upper = 5, maximum = T)
# 
# y <- c(0, 0, 0, rep(1, 7))
# 
# n <- 1
# 
# logL <- function(p) sum(log(dbinom(y, n, p)))
# 
# logL(.2)
# 
# p.seq <- seq(0, .99, .01)
# 
# plot(p.seq, sapply(p.seq, logL), type = "l")
# optimize(logL, lower = 0, upper = 1, maximum = T)
