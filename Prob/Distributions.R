###########################################################
# Class demonstrating dealing with reference distributions
# of a random variable.
###########################################################

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

###########################################################
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

# Create a data matrix of 100x100, and sample 30% of the total.
sample<-matrix(data=rbinom(10000,1,0.3), nrow=100, ncol=100, byrow=TRUE)
num_cells <- sum(sample)
# Using the randomly sampled cells in the grid, populate them with values
# from a poisson distribution with mu=15
sample[sample == 1] <- rpois(num_cells, 15)
# Have a look at the values
plot(x = 1:100, y =  Matrix::colMeans(sample))
# pull out the sampled cells
sampled <- sample[sample != 0]
# create a hist of those counts, but don't plot
h <- hist(sampled, breaks = 11, plot=FALSE)
# normalize the hist to create a pdf of the samples, then plot
h$counts=h$counts/sum(h$counts)
plot(h)

###########################################################
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


# Assume a normal distribution with a mean of 70 and a standard deviation of 12. 
# What limits would include the middle 65% of the cases?
x <- seq(1,150,.1)
lim <- (.65/2)
lower <- qnorm(.5-lim, 70, 12)
upper <- qnorm(.5+lim, 70, 12)
ggplot() + geom_point(aes(x=x,y=dnorm(x,70,12))) +
    geom_vline(xintercept = qnorm(.5 - lim,70,12), color="red", linetype = 2) +
    geom_vline(xintercept = qnorm(.5 + lim,70,12), color="red", linetype = 2)

# A normal distribution has a mean of 20 and a standard deviation of 4. 
# Find the z scores for the following numbers: (a) 28 (b) 18 (c) 10 (d) 23”
zscores <- unlist(lapply(c(28,18,10,23), function(x) (x-20)/4)) # 2.00 -0.50 -2.50  0.75
x <- seq(1,40,.1)
ggplot() + geom_point(aes(x=x,y=dnorm(x,20,4))) +
    geom_vline(xintercept = 28, color="red", linetype = 2) +
    geom_vline(xintercept = 18, color="red", linetype = 2) +
    geom_vline(xintercept = 10, color="blue", linetype = 3) +
    geom_vline(xintercept = 23, color = "blue", linetype = 3)

# Assume the speed of vehicles along a stretch of I-10 has an approximately normal distribution 
# with a mean of 71 mph and a standard deviation of 8 mph.
# The current speed limit is 65 mph. What is the proportion of vehicles less than or equal to the speed limit?
pnorm(65,71,8)

# What proportion of the vehicles would be going less than 50 mph? 
pnorm(50,71,8)
# A new speed limit will be initiated such that approximately 10% of vehicles will be over the speed limit. 
# What is the new speed limit based on this criterion?
qnorm(.90,71,8)

# A variable is normally distributed with a mean of 120 and a standard deviation of 5. 
# One score is randomly sampled. What is the probability it is above 127?
pnorm(127,120,5,lower.tail = FALSE)

# Use the normal distribution to approximate the binomial distribution. 
# Find the probability of obtaining exactly 7 heads out of 12 flips.
# The mean is N*p, var = N*p * (1-p)
dm <- 12 * 0.5 # fair coin
dsd <- (dm * (1 - 0.5))^1/2
x <- seq(1,12,1)
y <- dnorm(x,dm,dsd)
# create a second data frame of normally distributed values with more points
distdf <- data.frame(x=seq(1,12,.1),y=dnorm(seq(1,12,.1),dm,dsd))
ggplot() + aes(x=x, y=y) + geom_point() +
    geom_point(data=distdf, color="red", alpha=0.2) +
    ggtitle("Binomial distribution with 12 points\n and a continuous normal distribution") + 
    theme(plot.title = element_text(lineheight=.8, face="bold")) +
    geom_vline(xintercept=7, color="green", linetype = 2) +
    geom_vline(xintercept=dm+dsd, color="lightblue", linetype=1) +
    geom_vline(xintercept=dm-dsd, color="lightblue", linetype=1)
    
pnorm(6.5,dm,dsd,lower.tail = FALSE) - pnorm(7.5,dm,dsd, lower.tail = FALSE)
# 0.2107861 which matches what's in the plot

# A distribution is normal with a mean of 25, and a standard deviation of 4. 
# Everyone who scores in the top 30% of the distribution gets a certificate. 
# What is the lowest score someone can get and still earn a certificate? 
# (b) The top 5% of the scores get to compete in a statewide history contest. 
# What is the lowest score someone can get and still go onto compete with the rest of the state?
x <- seq(1,50,.2)
y <- dnorm(x,25,4)
cert <- qnorm(.7,25,4) # 27.0976
state <- qnorm(.95,25,4) # 31.57941
ggplot() + aes(x=x,y=y) + geom_point() +
    geom_vline(xintercept=25+4,color="lightblue",linetype=1) +
    geom_vline(xintercept=25-4,color="lightblue",linetype=1) +
    geom_vline(xintercept=cert,color="lightgreen",linetype=1) +
    geom_vline(xintercept=state,color="darkgreen",linetype=1) +
    ggtitle("Scores with +/-1sd and\n certificate threshold\n and state contest") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))

# Use the normal distribution to approximate the binomial distribution 
# and find the probability of getting 15 to 18 heads out of 25 flips. 
# Compare this to what gets when  calculate the probability using 
# the binomial distribution, with 4 significant digits.
dm <- 25 * 0.5 # fair coin
dsd <- (dm * (1 - 0.5))^1/2
x <- seq(1,25,1)
y <- dnorm(x,dm,dsd)
# create a second data frame of normally distributed values with more points
distdf <- data.frame(x=seq(1,25,.1),y=dnorm(seq(1,25,.1),dm,dsd))
ggplot() + aes(x=x, y=y) + geom_point() +
    geom_point(data=distdf, color="red", alpha=0.2) +
    ggtitle("Binomial distribution with 25 flips\nand interval 15-18") + 
    theme(plot.title = element_text(lineheight=.8, face="bold")) +
    geom_vline(xintercept=15, color="green", linetype = 2) +
    geom_vline(xintercept=18, color="green", linetype = 2) +
    geom_vline(xintercept=dm+dsd, color="lightblue", linetype=1) +
    geom_vline(xintercept=dm-dsd, color="lightblue", linetype=1)

normpvalue <- pnorm(18,dm,dsd) - pnorm(15,dm,dsd) # 0.17265
binompvalue <- pbinom(18,25,0.5) - pbinom(15,25,0.5) # 0.10744
difference <- signif((normpvalue - binompvalue),4) # 0.06521
