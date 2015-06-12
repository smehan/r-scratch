###########################################################
### Class for working with other models beyond regression, including
### reguilarization with elastic net, weakly informative pairs
### nonlinear least squares, splines, GAMs, decision trees, random forests
###########################################################
library(ggplot2)
library(scales)
library(ggthemes)
library(XML)
library(plyr)
library(stringr)
library(reshape2)


# select variables and improve predictions with the elastic net
# useful for high-dimensional data
# load acs data to work with.

acs <- read.table("http://www.jaredlander.com/data/acs_ny.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
acs$Income <- with(acs, FamilyIncome >= 150000)
names(acs)[length(acs)] <- "Income" # error in names in data fixed
head(acs)

# load the package for elastic net
library(glmnet)
library(useful)

# Now need to build up two required matrices for elasticnet.
acsX <- build.x(Income ~ NumBedrooms + NumChildren + NumPeople + NumRooms + 
                NumUnits + NumVehicles + NumWorkers + OwnRent + YearBuilt +
                ElectricBill + FoodStamp + HeatingFuel + Insurance + Language -1,
                data = acs, 
                contrasts = FALSE)
# observe the hogh-dimensionality of the resulting matrix
class(acsX)
dim(acsX)
# now build out Y
acsY <- build.y(Income ~ NumBedrooms + NumChildren + NumPeople + NumRooms + 
                NumUnits + NumVehicles + NumWorkers + OwnRent + YearBuilt +
                ElectricBill + FoodStamp + HeatingFuel + Insurance + Language -1,
                data = acs)

# now we need to determine the lambda for the penalty coefficient.
# to get there we use k-fold cross validation
set.seed(2975961)


acsCV1 <- cv.glmnet(acsX, acsY, family="binomial", nfolds = 5)
# this yields lambdas in a vector
acsCV1
# plot them
plot(acsCV1)
# now look at the coefficients of the correlation vars for one particular lambda
coef(acsCV1, s="lambda.1se")
# plot it out
plot(acsCV1$glmnet.fit, xvar = "lambda")
# draw in our lambdas
abline(v = log(c(acsCV1$lambda.min, acsCV1$lambda.1se)), lty = 2)

# This has used the default weighting of L1 and L2 in the elasticnet, which is lasso (1)
# for comparison, let's change the weight to the other extreme, 0, all ridge regression
# usually looking for alpha = [0.7,1.0]
acsCV2 <- cv.glmnet(acsX, acsY, family="binomial", nfolds = 5, alpha = 0)
plot(acsCV2$glmnet.fit, xvar = "lambda")
abline(v = log(c(acsCV2$lambda.min, acsCV2$lambda.1se)), lty = 2)
# here we see that the coefficients never get thrown out but squashed towards 0 to
# minimize the error

