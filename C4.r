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
