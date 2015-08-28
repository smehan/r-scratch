###########################################################
### Financial Time Series plotted
###########################################################

# Time Series Plotting
library(ggplot2)
library(xts)
library(dygraphs)

# Get Apple and Google stock data from Yahoo Finance
appl_url <- "http://real-chart.finance.yahoo.com/table.csv?s=AAPL"
goog_url <- "http://real-chart.finance.yahoo.com/table.csv?s=GOOG"

yahoo.read <- function(url){
    dat <- read.table(url,header=TRUE,sep=",")
    df <- dat[,c(1,5)]
    df$Date <- as.Date(as.character(df$Date))
    return(df)}

appl  <- yahoo.read(appl_url)
goog <- yahoo.read(goog_url)

ggplot(appl,aes(Date,Close)) + 
    geom_line(aes(color="apple")) +
    geom_line(data=goog,aes(color="google")) +
    labs(color="Legend") +
    scale_colour_manual("", breaks = c("apple", "google"),
                        values = c("blue", "brown")) +
    ggtitle("Closing Stock Prices: Apple & Google") + 
    theme(plot.title = element_text(lineheight=.7, face="bold"))

# Plot with the htmlwidget dygraphs
# dygraph() needs xts time series objects
appl_xts <- xts(appl$Close,order.by=appl$Date,frequency=365)
goog_xts <- xts(goog$Close,order.by=goog$Date,frequency=365)

stocks <- cbind(appl_xts,goog_xts)

dygraph(stocks,ylab="Close", 
        main="Apple and Google Closing Stock Prices") %>%
    dySeries("..1",label="Apple") %>%
    dySeries("..2",label="Google") %>%
    dyOptions(colors = c("blue","brown")) %>%
    dyRangeSelector()
