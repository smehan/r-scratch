***********************************************************
    C10 Class: Linear Models
***********************************************************

# Fitting simple linear models

data(father.son, package = "UsingR")
library(ggplot2)

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
