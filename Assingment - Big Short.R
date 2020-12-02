library(tidyverse)
library(dslabs)

# Just as banks must decide how much to charge as interest on loans 
# based on estimates of loan defaults, insurance companies must decide 
# how much to charge as premiums for policies given estimates of the probability 
# that an individual will collect on that policy. 

data(death_prob)
head(death_prob)

# Question 1

p <- death_prob %>%
  filter(age == 50 & sex == "Female") %>%
  select(prob) %>%
  return

p <- as.numeric(p)

# Expected value of one 50 year old female client 

policy_pay <- -150000
premium <- 1150

rate <- abs(premium / policy_pay)*100
rate

exp_profit <- policy_pay*p + premium*(1-p)
se_exp_profit <- abs(premium - policy_pay)*sqrt(p*(1-p))

n <- 1000

avg_1000 <- n*exp_profit # expected profit with 1000 clients with independent probabilties of dying

se_1000 <- sqrt(n)*se_exp_profit

# Use the Central Limit Theorem to calculate the probability that the insurance company loses money on this set of 1,000 policies.

pnorm(0,avg_1000,se_1000)


# Probability of a 50 year old male death

p_male <- death_prob %>%
  filter(age == 50 & sex == "Male") %>%
  select(prob) %>%
  return

p_50_male <- as.numeric(p_male)

profit_50_male <- 700000

premium_50_male <- ((profit_50_male/n) - policy_pay*p_50_male) / (1 - p_50_male)

se_50_male <- sqrt(n)*abs(policy_pay - premium_50_male)*sqrt(p_50_male * (1-p_50_male)) 


pnorm(0,700000,se_50_male)



# probability of death within 1 year for a 50 year old to .015. 
# Unable to predict the outbreak, the company has sold 1,000 
# $150,000 life insurance policies for $1,150.


p_new <- 0.015

policy_pay
premium
n

expect_profit <- n*(p_new*policy_pay + (1-p_new)*premium)

se_profit <- sqrt(n)*abs(policy_pay - premium)*sqrt(p_new*(1-p_new))

pnorm(0,expect_profit,se_profit)

options(digits = 6)
pnorm(-1000000,expect_profit,se_profit, lower.tail = T) # probability of losing more than 1mm

p <- seq(.01, .03, .0025) # prob range

exp_profit_new <- n*(p*policy_pay + (1-p)*premium)
se_profit_new <- sqrt(n)*abs(policy_pay - premium)*sqrt(p*(1-p))

pnorm(0,exp_profit_new,se_profit_new)

pnorm(-1000000,exp_profit_new,se_profit_new, lower.tail = T)

## Define a sampling model for simulating the total profit over 1,000 loans

p_loss <- 0.015
n
policy_pay
premium
set.seed(25, sample.kind = "Rounding")

X <- sample(c(policy_pay,premium),n, replace = T, prob = c(p_loss, (1-p_loss)))

mean(X)
sum(X)/10^6

set.seed(27, sample.kind = "Rounding")

B <- 10000
S <- replicate (B, {
  result <- sample(c(policy_pay,premium),n, replace = T, prob = c(p_loss, (1-p_loss)))
  sum(result)  
})

mean(S)
sum(S)
mean(S < -1000000)

# find a premium cost for which the probability of losing money is under 5%, 
# assuming the death rate stays stable at  𝑝=0.015 .

p <- 0.015
z <- qnorm(0.05)
l <- policy_pay

premium_new <- -l*(n*p - z*sqrt(n*p*(1-p))) / (n*(1-p) + z*sqrt(n*p*(1-p)))


exp_prof <- p*l + (1-p)*premium_new

n*exp_prof

## Monte Carlo Symulatoin


set.seed(28, sample.kind = "Rounding")

B <- 10000
p <- 0.015

S <- replicate(B, {
  result <- sample(c(policy_pay,premium_new),n, replace = T, prob = c(p, (1-p)))
  sum(result) 
})

mean(S < 0)
hist(S)


set.seed(28, sample.kind = "Rounding")

B <- 10000
p <- 0.015

S <- replicate(B, {
  result <- sample(c(policy_pay,premium_new),n, replace = T, prob = c(p, (1-p)))
  sum(result) 
})

mean(S < 0)
hist(S)

set.seed(29, sample.kind = "Rounding")

S <- replicate(B, {
  p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  result <- sample(c(policy_pay,premium_new),n, replace = T, prob = c(p, (1-p)))
  sum(result) 
})


mean(S)
mean(S < -1000000)
hist(S)


  