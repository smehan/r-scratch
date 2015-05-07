require(ggplot2)
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