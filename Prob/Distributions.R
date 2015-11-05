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

