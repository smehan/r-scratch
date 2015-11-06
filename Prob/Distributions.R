###########################################################
# Class demonstrating dealing with reference distributions
# of a random variable.
###########################################################

# Binomial distribution
# Two possible outcomes for N trials, each with prob = p
#
# mean = Np
# var = Np(1-p)
# P(x) = (N choose x)(p^x)(1-p)^(N - x)
# (N choose x) = N! / (x!(N-x)!)

# P(0H) for 2 flips of 0.5 coin
dbinom(0,2,.5)
# P(1H)
dbinom(1,2,.5)
# cumalative probability of 0 to 1H for 2 flips of 0.4 coin
pbinom(1,2,.4)

# need to create a sequence of x, which represents the successes in the trials
# so 0H, 1H, 2H, 3H, etc. Then map probabilities via dbinom
x <- seq(0,50,by=1)
y <- dbinom(x,50,0.2)
plot(x,y)
y <- dbinom(x,50,0.6)
plot(x,y)
x <- seq(0,100,by=1)
y <- dbinom(x,100,0.6)
plot(x,y)

# Note that if there are more than two outcomes, binomial changes to multinomial, and 
# P = N!/((n1!)(n2!)...(nk!)*PI(pk), with k different outcomes.
t(rmultinom(10, size = 12, prob=c(0.1,0.2,0.8)))
# yields 10 different random draws of these three classes, p = p1 + p2 + p3

# In a certain town, 40% of the eligible voters prefer candidate A, 
# 10% prefer candidate B, and the remaining 50% have no preference.
# You randomly sample 10 eligible voters. What is the probability that 
# 4 will prefer candidate A, 1 will prefer candidate B,and the 
# remaining 5 will have no preference?
dmultinom(c(4,1,5),10,c(.4,.1,.5)) # 0.1008


# Poisson distribution probability distribution of independent event occurrences 
# in an interval. If λ is the mean occurrence per interval, then the probability 
# of having x occurrences within a given interval is
#  P(x) = λ^xe^(-λ) / x!

# probability of seeing 12 occurances in an event when mean of event is 21
dpois(12,21) #0.016

# If there are twelve cars crossing a bridge per minute on average, 
#find the probability of having seventeen or more cars crossing the bridge in a particular minute.
# The probability of having sixteen or less cars crossing the bridge in a particular 
# minute is given by the function ppois.

ppois(16, lambda=12)   # lower tail 
# 0.89871

# Hence the probability of having seventeen or more cars crossing the bridge 
# in a minute is in the upper tail of the probability density function.

ppois(16, lambda=12, lower=FALSE)   # upper tail 
# 0.10129 which is the probability sought.

# plot the discrete probability distribution for this particular poission data
ggplot(transform(data.frame(x=c(0:22)), y=dpois(x, 12)), aes(x, y)) + 
    geom_bar(stat="identity")


### Gaussian distribution is the distribution which by the Central Limit Theorem
# is the distribution arrived at in the limit as n->Inf. It has a shape(μ, σ^2)
# where P = (1/(2pi*σ^2)^1/2) * e^(-(x-μ)^2/(2*σ^2))

# create a N(0,1^2) distribution
x <- seq(-1,1, by=0.01)
dnorm(x,0,1)
# plot it
plot(x,dnorm(x,0,1))

# some others
plot(x,dnorm(x,0,0.1))
plot(x,dnorm(x,0,0.2))
plot(x,dnorm(x,0,0.5))

# probability of getting between 115 and 120 from N(90,12^2)
pnorm(115,90,12, lower.tail = FALSE) - pnorm(120,90,12, lower.tail = FALSE) # 0.01240076

# z = (x-μ)/σ
# here we would use qnorm to find a z for a given P

x <- seq(0,1,by=.05)
y <- qnorm(x)
plot(x,y)
y <- qnorm(x,mean=3,sd=2)
plot(x,y)
y <- qnorm(x,mean=3,sd=0.1)
plot(x,y)