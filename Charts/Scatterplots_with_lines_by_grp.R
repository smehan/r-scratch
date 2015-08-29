library(ggplot2)
library(lubridate)
library(dplyr)

#first some data
xdate <- as.Date(c(seq.POSIXt(ymd("2005-01-01"), ymd("2007-03-04"), by = "30 days"), 
                   seq.POSIXt(ymd("2007-07-03"), ymd("2007-12-31"), by = "28 days"),
                   seq.POSIXt(ymd("2008-05-15"), ymd("2010-10-10"), by = "25 days"),
                   seq.POSIXt(ymd("2012-01-01"), ymd("2014-12-31"), by = "31 days")))
set.seed(321)                  
df <- data.frame(date = rep(xdate,3), cluster=rep(c("Cl","PO4","NO3")), y=rnorm(318,1,0.2))

# then calculate groups with dplyr 
# gap identifies where there is a break in the data
df <- df %>% group_by(cluster) %>% 
    arrange(date) %>% 
    mutate(gap = cumsum(c(0, diff(date) > 60)))

# extract the first and the last of every group
thefirst <- 
    df %>% group_by(gap,cluster) %>% 
    arrange(date) %>% 
    summarise(first(date),first(y))
thelast <-
    df %>% group_by(gap,cluster) %>% 
    arrange(date) %>% 
    summarise(last(date),last(y))

# equalize colnames for rbind and ggplot
colnames(thefirst) <- colnames(thelast) <- colnames(df)[c(4,2,1,3)]

# add 1 to match with thelast of every group with the first of the next group
# and calculate max
thelast$gap <- thelast$gap+1
maxgap <- max(thelast$gap)

gaplines <- rbind(filter(thefirst, gap != 0), filter(thelast,gap != maxgap))

#ggplot the connected lines
(p <-
    ggplot(df, aes(x=date, y=y)) +
    geom_point(size=2) +
    geom_line(aes(group=factor(gap))) +
    facet_wrap(~cluster, nrow=3))
# add the dotted lines
p +  geom_line(data=gaplines, aes(group = factor(gap)),linetype='dotted')
