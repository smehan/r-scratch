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

# Poisson distribution