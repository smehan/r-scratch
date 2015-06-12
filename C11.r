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
