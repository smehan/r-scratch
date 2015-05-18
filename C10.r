***********************************************************
    C10 Class: Linear Models
***********************************************************

# Fitting simple linear models

data(father.son, package = "UsingR")
library(ggplot2)
library(useful)
library(coefplot)

ggplot(father.son, aes(fheight, sheight)) +
    geom_point() +
    geom_smooth(method="lm") +
    labs(x="Fathers", y="Sons")

heights_lm <- lm(sheight ~ fheight, data=father.son)
summary(heights_lm)

# digression into anova and different data set
data(tips, package="reshape2")

tipsAnova <- aov(tip~day-1, data=tips)
tipsLM <- lm(tip~day-1, data=tips)
summary(tipsAnova)
summary(tipsLM)
# LM has more information than Anova and thus is preferred

# Explore the data for multiple regression, predict one variable on multiple variables
# get some data from website
housing <- read.table("http://www.jaredlander.com/data/housing.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
# clean up col names
names(housing) <- c("Neighborhood", "Class", "Units", "Years_Built", "SqFt", "Income", "IncomePerSqFt",
                    "Expense", "ExpensePerSqFt", "NetIncome", "Value", "ValuePerSqFt", "Boro")
# make an exploratory viz
ggplot(housing, aes(x=ValuePerSqFt, fill=Boro, alpha=1/2)) +
    geom_histogram(binwidth=10) +
    labs(x="Value per sq ft") + 
    facet_wrap(~Boro)

ggplot(housing, aes(x=SqFt)) + 
    geom_histogram()

ggplot(housing, aes(x=SqFt, y=ValuePerSqFt)) +
    geom_point()

# see that there are very large buildings skewing results, so subset

ggplot(housing[housing$Units < 1000, ], aes(x=SqFt)) + 
    geom_histogram()

ggplot(housing[housing$Units < 1000, ], aes(x=SqFt, y=ValuePerSqFt)) +
    geom_point()

# right track, so determine how many units to exclude
sum(housing$Units >= 1000)
# yields 6, so few, so remove outliers
housing <- housing[housing$Units < 1000, ]

# name the first model
house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data=housing)
summary(house1)

library(coefplot)
coefplot(house1)
# seek coefficients with CI > 0
# let's try other models
house2 <- lm(ValuePerSqFt ~ Units*SqFt + Boro, data=housing)
coefplot(house2)

house3 <- lm(ValuePerSqFt ~ Units:SqFt, data=housing)
coefplot(house3)

house4 <- lm(ValuePerSqFt ~ SqFt*Units*Income, housing)
coefplot(house4)

house5 <- lm(ValuePerSqFt ~ Class*Boro, housing)
coefplot(house5)

house6 <- lm(ValuePerSqFt ~ I(SqFt/Units) + Boro, housing)
coefplot(house6)

house7 <- lm(ValuePerSqFt ~ I(Units + SqFt)^2, housing)
coefplot(house7)

# plot multiple models
multiplot(house1, house2, house3)

# now let's use a regression model for prediction

housingNew <- read.table("http://www.jaredlander.com/data/housingNew.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

housePredict <- predict(house1, newdata=housingNew, se.fit=TRUE, interval="prediction", level=0.95)

# logistic regression
# modelling a yes/no response

# lets grab some data
acs <- read.table("http://www.jaredlander.com/data/acs_ny.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

acs$Income <- with(acs, FamilyIncome >= 150000)
names(acs)[length(acs)] <- "Income" # error in names in data fixed

ggplot(acs) +
    aes(x=FamilyIncome) +
    geom_density(fill="grey", color="blue") +
    geom_vline(xintercept=150000, color="red") +
    scale_x_continuous(label=multiple.dollar, limits=c(0,1000000)) +
    ggtitle("Density of National Income")

# fit some glms
income1 <- glm(Income ~ HouseCosts + NumWorkers + OwnRent +
                   NumBedrooms + FamilyType, 
               data=acs, family=binomial(link="logit"))

# summary of data and coefplot
summary(income1)
coefplot(income1)
# shows that OwnRentOutright is the most positive coefficient in set.

income1$coefficients
#this shows scale of coefficients is still on logit scale. want to return scale back to original for 
#further understanding.
invlogit <- function(x) {
    1/(1 + exp(-x))
}

invlogit(income1$coefficients)
# now back on normal P scale

# Poisson regression
# models count data, number of childre, accidents, errors, etc.

# make a plot of number of children
ggplot(acs) +
    aes(x=NumChildren) +
    geom_histogram(binwidth=1)

# appears close to poisson, so fit a glm poisson
children1 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent,
                 data=acs, family=poisson(link="log"))

summary(children1)
coefplot(children1)

# for a nice poisson fit, we need to check for overdipsersion
z <- (acs$NumChildren - children1$fitted.values) /
    sqrt(children1$fitted.values)
sum(z^2) / children1$df.residual
# > 1, considered no overdispersion
# one more test
pchisq(sum(z^2), df=children1.df)
# does indicate overdispersion, so let's refit poisson
children2 <- glm(NumChildren ~ FamilyIncome + FamilyType + OwnRent,
                 data=acs, family=quasipoisson(link="log"))

# let us compare the two models
multiplot(children1, children2)


# Analyze survival data

library(survival)

head(bladder)
# in this dataset there are censored data that we need to take into account to not
# over-fit. Need to find events (whether there was a reccurrence) and stop (if they dropped or something else).
# event = 1 is the area of concern

survObject <- with(bladder[100:105, ] Surv(stop, event))

# fit a model to see if treatment was significant

cox1 <- coxph(Surv(stop, event) ~ rx + number + size + enum, data=bladder)
summary(cox1)
coefplot(cox1)
plot(survfit(cox1), xlab="Days", ylab="Survival Rate", conf.int=TRUE)

# in this analysis, we have munged real and placebos together. Let's separate them
cox2 <- coxph(Surv(stop, event) ~  strata(rx) + number + size + enum, data=bladder)
summary(cox2)
plot(survfit(cox2), xlab="Days", ylab="Survival Rate", conf.int=TRUE, col=1:2)
legend("bottomleft", legend=c(1,2), lty=1, col=1:2, text.col=1:2, title="rx")

# test proportionality
cox.zph(cox1)
cox.zph(cox2)

# Model to predict number of events rather than just time to event
# use a different dataset with start and stop times
ag1 <- coxph(Surv(start, stop, event) ~ rx + number + size + enum +
                 cluster(id), data=bladder2)
ag2 <- coxph(Surv(start, stop, event) ~ strata(rx) + number + size + enum +
                 cluster(id), data=bladder2)
plot(survfit(ag1), conf.int = TRUE)
plot(survfit(ag2), conf.int = TRUE, col=1:2)
legend("topright", legend=c(1,2), lty=1, col=1:2, text.col=1:2, title="rx")

