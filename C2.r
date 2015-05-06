x <- 10:1
y <- -4:5
q <- c("Hockey", "Football", "Rugby", "Fencing", "Baseball", "Basketball", 
       "Curling", "Lacrosse", "Golf", "Tennis")
myDF <- data.frame(x,y,q)
myDF
myDF <- data.frame(First=x, Second=y, Sport=q)
class(myDF$Sport)
class(myDF)
myDF <- data.frame(first=x, second=y, sport=q, stringsAsFactors = FALSE)
class(myDF$sport)
nrow(myDF)
ncol(myDF)
dim(myDF)
NROW(myDF)
NROW(x)
names(myDF)
names(myDF)[3]
rownames(myDF)
rownames(myDF) <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
myDF
head(myDF)
head(myDF, 4)
tail(myDF, 3)
myDF$sport
myDF[3,2]
myDF[3,2:3]
list1 <- list(1,2,3)
list1
list2 <- list(c(1,2,3))
list2
list3 <- list(c(1,2,3), c(3,4,5,6,7))
list3
myDF
list4 <- list(myDF, 1:10)
list4
list5 <- list(TheDataFrame=myDF, AVector=1:10, ThisList=list3)
list5
names(list5) <- c("data.frame", "vector", "list")
names(list5)
emptyList <- vector(mode="list", length=4)
emptyList
emptyList[[1]] <- 5
list5[["TheDataFrame"]][, 2, drop=FALSE]
list5[[4]] <- 3
list5
A <- matrix(1:10, nrow=5)
A
B <- matrix(21:30, nrow=5)
C <- matrix(21:40, nrow=2)
A, B, C
B
A + B
A * B
A - B
A / B
A == B
A %*% t(B)
colnames(A)
colnames(B) <- c("First", "Second")
rownames(B) <- c("One", "Two", "Three", "Four", "Five")
LETTERS
colnames(C) <- LETTERS[1:10]
rownames(C) <- c("Top", "Bottom")
C
A
t(A)

A %*% C 

myArray <- array(1:12, dim=c(2,3,2))
myArray

tomato <- read.table(file="~/Downloads/Tomato First.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
head(tomato)
class(tomato$Tomato)





require(RODBC)
require(ggplot2)
data(diamonds)
diamonds
data("tips", package="reshape2")
head(tips)
data()


require(XML)


gameURL <- "http://www.jaredlander.com/2012/02/another-kind-of-super-bowl/"
bowlgame <- readHTMLTable(gameURL, which=1, header=FALSE)
bowlgame
head(diamonds)


hist(diamonds$carat, main="Carat Histogram", xlab="Carat")
plot(price ~ carat, data=diamonds, main="Price versus Carats", xlab="Label")
boxplot(diamonds$carat)
class(diamonds)
ggplot(diamonds) + geom_histogram(aes(diamonds$carat), binwidth=.25)
ggplot(diamonds) + geom_density(aes(diamonds$carat), fill="grey50")
ggplot(diamonds, aes(carat, price)) + geom_point(aes(color=color, shape=cut))
ggplot(diamonds, aes(carat,x=cut)) + geom_boxplot()
ggplot(diamonds, aes(carat,x=cut)) + geom_jitter() + geom_violin()

head(economics)
ggplot(economics, aes(date, pop)) + geom_line()

require(lubridate)
economics$year <- year(economics$date)
economics$month <- month(economics$date)
head(economics)

econ2000 <- economics[which(economics$year >= 2000), ]
head(econ2000)
econ2000$month <- month(econ2000$date, label=TRUE)


require(scales)
g <- ggplot(econ2000, aes(month, pop)) 
g <- g + geom_line(aes(color=factor(year), group=year))
g <- g + scale_color_discrete(name="Year")
g <- g + scale_y_continuous(labels=comma) 
g <- g + labs(title="Population Growth", x="Month", y="Population")
g <- g + theme(axis.text.x=element_text(angle = 90, hjust=1))
g


g <- ggplot(diamonds, aes(carat, price))
g <- g + geom_point(aes(color=color)) + facet_wrap(~color)
g <- g + geom_point(aes(color=color)) + facet_grid(cut~clarity)
g

g <- ggplot(diamonds, aes(carat))
g <- g + geom_histogram(aes()) + facet_wrap(~color)
g

require(ggthemes)

g <- ggplot(diamonds, aes(carat, price, color=color))
g <- g + geom_point()
g

g + theme_tufte() + scale_color_colorblind()
