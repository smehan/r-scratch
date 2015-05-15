# pull N numbers from a normal distribution
rnorm(10)
rnorm(10, mean = 100, sd = 25)
myRand <- rnorm(20)
dnorm(myRand)


# build a density curve for N points in the normal dist
require(ggplot2)
myRand <- rnorm(50000)
myRandDensity <- dnorm(myRand)
ggplot(data.frame(x=myRand, y=myRandDensity)) +
    aes(x=x,y=y) +
    geom_point() +
    labs(x="Random numbers", y="Density")

# cumulative probability distribution is P of x or less
pnorm(myRand)
pnorm(c(3,0,3))
pnorm(c(-1,0,1))
# if not left tail but interval, diff the Ps
pnorm(1)-pnorm(0)

# binomial dist
rbinom(n = 5, size = 10, prob = 0.4)
#hist it
myBinomData <- rbinom(10000, 10, 0.3)
ggplot(data.frame(Successes = myBinomData)) +
    aes(x = Successes) +
    geom_histogram(binwidth=1)

# build different binomial dists of differing sizes, then combine into 
# a single data frame

myBinom10 <- data.frame(Successes=rbinom(10000, 10, 0.3), Size=10)
myBinom100 <- data.frame(Successes=rbinom(10000, 100, 0.3), Size=100)
myBinom1000 <- data.frame(Successes=rbinom(10000, 1000, 0.3), Size=1000)
myBinom10000 <- data.frame(Successes=rbinom(10000, 10000, 0.3), Size=10000)
myBinomAll <- rbind(myBinom10, myBinom100, myBinom1000, myBinom10000)

ggplot(myBinomAll) +
    aes(x=Successes) +
    geom_histogram(bindiwth=1) +
    facet_wrap(~Size, scales = "free")

# some functions working with the Normal dist
dbinom(x=3, size = 10, prob = 0.3)
pbinom(q = 3, size = 10,prob = 0.3)
qbinom(p = 0.4, size = 10, prob = 0.3)
# i.e., need RETURN draws
qbinom(p = c(0.2, 0.25, 0.3, 0.35, 0.5, 0.6), size = 10, prob = 0.3)

#build up differing poisson distributions
myPois1 <- rpois(n = 10000, lambda = 1)
myPois2 <- rpois(n = 10000, lambda = 2)
myPois5 <- rpois(n = 10000, lambda = 5)
myPois10 <- rpois(n = 10000, lambda = 10)
myPois20 <- rpois(n = 10000, lambda = 20)
myPoisAll <- data.frame(Lambda.1=myPois1, 
                   Lambda.2=myPois2, 
                   Lambda.5=myPois5,
                   Lambda.10=myPois10,
                   Lambda.20=myPois20)

ggplot(myPoisAll) +
    aes(x=Draws) +
    geom_histogram(binwidth=1) +
    facet_wrap(~Mean, scales = "free")

require(reshape2)
myPoisAll <- melt(data = myPoisAll, variable.name = "Lambda", value.name = "x")
# lets extract the x from Lambda.x and make it cleaner to read
require(stringr)
myPoisAll$Lambda <- str_extract(string = myPoisAll$Lambda, pattern = "\\d+")
# need to cast Lanmda vector as a Factor to use nicely in ggplot
myPoisAll$Lambda <- as.factor(as.numeric(myPoisAll$Lambda))
# build a plot
ggplot(myPoisAll) +
    aes(x=x) +
    geom_density(aes(group=Lambda, color=Lambda, fill=Lambda), adjust=4, alpha=1/2) +
    scale_color_discrete() +
    scale_fill_discrete() +
    ggtitle("Probability Mass Function")

#9.2
x <- sample(x = 1:100, size = 100, replace = TRUE)
first <- sample(x=1:50, size = 5, replace=FALSE)
second <- sample(x=1:15, size=1, replace=FALSE)
xNA <- x
xNA[sample(x=1:100, size=20, replace=FALSE)] <- NA
mean(xNA, na.rm = TRUE)
var(xNA, na.rm = TRUE)
sd(xNA, na.rm = TRUE)
summary(x)
quantile(x, probs = c(0.25, 0.75))

# correlations using sample data
cor(economics$pce, economics$psavert)
myEconCor <- cor(economics[, c(2,4:6)])
myEconCorMelted <- melt(data = myEconCor, varnames = c("x", "y"), value.name = "Correlations")
#now order the result for plotting
myEconCorMeltedOrdered <- myEconCorMelted[order(myEconCorMelted$Correlations), ]

#now plot as a heat map
require(scales)
ggplot(myEconCorMeltedOrdered) +
    aes(x=x, y=y) +
    geom_tile(aes(fill=Correlations)) +
    scale_fill_gradient2(low=muted("red"), mid="white", high="steelblue",
                         guide=guide_colorbar(ticks = FALSE, barheight = 12),
                         limits=c(-1,1)) +
    theme_minimal() +
    labs(x=NULL, y=NULL) +
    ggtitle("Correlation Heat Map of Economic Factors")

# t-test
data("tips", package="reshape2")
# 1-sample t-test first. testing for equal to, not greater/less than
t.test(tips$tip, alternative="two.sided", mu=2.5)
randT <- rt(30000, df=NROW(tips)-1)
tipTTest <- t.test(tips$tip, alternative="two.sided", mu=2.5)

ggplot(data.frame(x=randT)) +
    geom_density(aes(x=x), fill="grey", color="grey") +
    geom_vline(xintercept=tipTTest$statistic) +
    geom_vline(xintercept=mean(randT) + c(-2,2)*sd(randT), linetype=2)

#result shows mu is far from center of distribution

# 1 sided to show if less or more than
t.test(tips$tip, alternative="greater", mu=2.5)

# compare tips of females to males
aggregate(tip~sex, data=tips, mean)

#to t test effectively between two groups, need to have the variance equal, so lets test
aggregate(tip~sex, data=tips, var)
#appears different, but lets test for normally distributed 
shapiro.test(tips$tip)
# shows with low p value NOT normally distributed as a whole
shapiro.test(tips$tip[tips$sex == "Female"])
shapiro.test(tips$tip[tips$sex == "Male"])
# low p values show subsets are both not normally distributed, so normal test wont work.
# still let's plot and visualize
ggplot(tips, aes(x=tip, fill=sex)) + 
    geom_histogram(binwidth=0.5, alpha=1/2)
# both are skewed. Thus, parametric test will not work. Need to use non-parametric test
ansari.test(tip~sex, tips)
#high p values mean we can use this test
t.test(tip~sex, data=tips, var.equal=TRUE)
# p value above threshold, so tips between the sexes are roughly equivalent
# let's visualize
library(plyr)
tipSummary <- ddply(tips, "sex", summarize, 
                    tip.mean=mean(tip), tip.sd=sd(tip),
                    Lower=tip.mean - 2*tip.sd/sqrt(NROW(tip)),
                    Upper=tip.mean + 2*tip.sd/sqrt(NROW(tip)))
ggplot(tipSummary, aes(x=tip.mean, y=sex)) + 
    geom_point() +
    geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.2)
#paired t test
library(UsingR)
#have data set of father and son variables
t.test(father.son$fheight, father.son$sheight, paired=TRUE)
# results show low pvalues, so little difference between paired observations
#what about more than two samples? Need Anova

#want to test whether tips vary by day
tipAnova <- aov(tip ~ day -1, tips)
tipIntercept <- aov(tip ~ day, tips)
tipAnova$coefficients
tipIntercept$coefficients
# we have an intercept != 0, so can only compute on N-1 values
# look at summary to see what is p value for anova
summary(tipAnova)
# let's visualize
tipsByDay <- ddply(tips, "day", summarise,
                   tip.mean=mean(tip), tip.sd=sd(tip),
                   Length=NROW(tip),
                   tfrac=qt(p=.9, df=Length-1),
                   Lower=tip.mean - tfrac*tip.sd/sqrt(Length),
                   Upper=tip.mean + tfrac*tip.sd/sqrt(Length))

ggplot(tipsByDay, aes(x=tip.mean, y=day)) +
    geom_point() +
    geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.3)
# see the non-overlapping confidence intervals
