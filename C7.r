#Data munging
# repeate op on a matrix using apply
# repeat op on a list
# mapply
# aggregate

# Don't use a loop, really

myMatrix <- matrix(1:9, nrow=3)
apply(myMatrix, MARGIN = 2, sum)

# but there are built in ops for these
colSums(myMatrix)
rowSums(myMatrix)

myMatrix[2,1] <- NA
rowSums(myMatrix)
# get around NA
apply(myMatrix, 1, sum, na.rm=TRUE)

#iterate over a list
myList <- list(A=myMatrix, B=1:5, C=matrix(1:4, 2), D=2)
lapply(myList, sum, na.rm=TRUE)
sapply(myList, sum, na.rm=TRUE)

#lapply works on vectors as they are lists
someNames <- c("Shawn", "Sean", "Seathan")
lapply(someNames, nchar)

#mapply
firstList <- list(A=matrix(1:16,4), B=matrix(1:16,2), C=1:5)
secondList <- list(A=matrix(1:16,4), B=matrix(1:16,8), C=15:1)

mapply(identical, firstList, secondList)
mapply(sum, firstList, secondList)


#subsets of data using aggregate
require(ggplot2)
data(diamonds)
head(diamonds)
mean(diamonds$price)
aggregate(price ~ cut, diamonds, mean)
aggregate(price ~ cut + color, diamonds, mean)
aggregate(cbind(price, carat) ~ cut + color, diamonds, mean)

#plyr
require(plyr)
head(baseball)
baseball$sf[baseball$year < 1954] <- 0
any(is.na(baseball$sf))
baseball$hbp[is.na(baseball$hbp)] <- 0
any(is.na(baseball$hbp))
baseball <- baseball[baseball$ab >= 50, ]
baseball$obp <- with(baseball, (h + bb + hbp) / (ab + bb + hbp + sf))
obp <- function(data){
    return(c(OBP= with(data, sum(h + bb + hbp) / sum(ab + bb + hbp + sf))))
}
careerOBP <- ddply(baseball, .variables = "id", obp)
careerOBP <- careerOBP[order(careerOBP$OBP, decreasing = TRUE), ]


#llply
myList <- list(A=matrix(1:9, 3), B=1:5, C=matrix(1:4, 2), D=2)
lapply(myList, sum)
llply(myList, sum)
identical(lapply(myList, sum), llply(myList, sum))

sapply(myList, sum)
laply(myList, sum)

#helper functions
aggregate(price ~ cut, diamonds, each(mean, median))
numcolwise(sum, na.rm=TRUE)(diamonds)
sapply(diamonds[, sapply(diamonds, is.numeric)], sum)

#combine datasets
sports <- c("Hockey", "Baseball", "football")
leagues <- c("NHL", "MBA", "NFL")
trophy <- c("Stanley Cup", "Commissioner's Trophy", "Vince Lombardi Trophy")

sports1 <- cbind(sports, leagues, trophy)

sports2 <- data.frame(sports=c("Basketball", "Golf"),
                      leagues=c("NBA", "PGA"),
                      trophy=c("Larry O'Brien Championship Trophy", "Wanamaker Trophy"))

sports <- rbind(sports1, sports2)

#cbind is for adding columns from different vectors or df
#rbind will add rows to a new df

#join datasets

codes <- read.table("http://www.jaredlander.com/data/countryCodes.csv ", sep = ",", stringsAsFactors = FALSE)
countries <- read.table("http://www.jaredlander.com/data/GovType.csv ", sep = ",", stringsAsFactors = FALSE)

View(countries)

#to join we can use merge
countryMerged <- merge(x=codes, y=countries, by.x="Country name", by.y="Country")

