## Random Variables and Sampling Models ##

# Roleta


urna <- rep(c("Black", "Red", "Green"), c(18,18,2))
n <- 1000

X <- sample(ifelse(urna == "Red",-1,1),n,replace = T)

sum(X)


X <- sample(c(-1,1), n, replace = T, prob = c(9/19,10/19))

b <- 10000

S <- replicate(b, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
  sum(X)    # determine total profit
})

mean(S < 0) #Probability casino looses money

library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")


p_guess_correct <- 1/5
p_guess_not_correct <- 1 - p_guess_correct

avg <- p_guess_correct*1 + p_guess_not_correct*-0.25

se <- abs(-0.25 - 1) * sqrt(p_guess_correct*p_guess_not_correct)


pnorm(8,44*avg,sqrt(44)*se,lower.tail = F)

#run a Monte Carlo simulation of 10,000 students guessing on the test.

set.seed(21, sample.kind = "Rounding")

n <- 10000

S <- replicate(n,sum(sample(c(1,-0.25),44,replace = T,prob = c(p_guess_correct,p_guess_not_correct))))
mean(S > 8)


# new SAT


p_guess_correct <- 1/4
p_guess_not_correct <- 1 - p_guess_correct

avg <- p_guess_correct*1 + p_guess_not_correct*0

se <- abs(0 - 1) * sqrt(p_guess_correct*p_guess_not_correct)

expected_score_test <- avg*44


p <- seq(0.25, 0.95, 0.05)
p_not <- 1- p


avg_range_test <- p*1*44
se_range_test <- sqrt(p*p_not)*sqrt(44)


pnorm(35,avg_range_test,se_range_test,lower.tail = F) 

p[13]


#  a bet on five pockets (00, 0, 1, 2, 3) out of 38 total pockets.
# The bet pays out 6 to 1. In other words, a losing bet yields -$1 and a successful bet yields $6.

p_win <- 5/38
p_not_win <- 1 - p_win


e_win_1 <- (6*p_win + -1*p_not_win)
se_1 <- abs(-1 - 6)* sqrt(p_win*p_not_win)

## Expected value and expected average value of x games is the same

se_average_500 = se_1 / sqrt(500)


e_win_500 = 500 * e_win_1
se_500 = se_1 * sqrt(500)

pnorm(0,e_win_500,se_500)

n <- 500
X <- sample(c(6,-1),n,replace = T, prob = c(p_win,p_not_win))

mean(X > 0)

mean(X)


## Case study Big Short

options(digits = 4)

p_default <- 0.02
p_not_default <- 1 - p_default

n <- 1000 # number of loans

l <- 180000 # value of loan
f <- -200000 #foreclosure cost


exp_profit <- n * (p_default*f + p_not_default*0)
se <- sqrt(n)*abs(f)*sqrt(p_default*p_not_default)


defaults <- sample( c(0,f), n, prob=c(p_not_default, p_default), replace = TRUE)
sum(defaults)

B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,l), n, prob=c(p_not_default, p_default), replace = TRUE) 
  sum(defaults)
})


library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.5, col = "black")

## calculate the interest rate profit that makes exp_profit whole


# exp_profit = (p_default*f + p_not_default*x)

x = (-p_default*f) / p_not_default
x
interest = x/l


## Calculate interest rate for 1% probability of losing money


z <- qnorm(0.01)
x <- -f*( n*p_default - z*sqrt(n*p_default*p_not_default)) / ( n*p_not_default + z*sqrt(n*p_default*p_not_default))
x/180000    # interest rate
f*p_default + x*(p_not_default)    # expected value of the profit per loan
n*(f*p_default + x*(p_not_default)) # expected value of the profit over n loans


B <- 100000

profit <- replicate(B, {
  draws <- sample(c(x,f),n,replace = T, prob = c(p_not_default,p_default))
  sum(draws)
})

mean(profit)
mean(profit < 0)


## Calculating number of loans for desired probability of losing money

p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)

z <- qnorm(0.001)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans


# Monte Carlo
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)

# Monte Carlo with unkown interest-rate

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million

hist(profit)

