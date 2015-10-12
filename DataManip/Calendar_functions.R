###########################################################
### Class working with dates, building a sequence of calendar
### dates and various categorizations based on week, month, etc.
###########################################################

library(lubridate)
library(dplyr)

### Let's build our calendar data frame
calendarDF <- data.frame(YMD_date = seq(as.Date("2014-01-01"), as.Date("2014-12-31"), "day"))

### now let's add some important columns of categorization
calendarDF$Month <- month(calendarDF$YMD_date)
calendarDF$Month_name <- month(calendarDF$YMD_date, label = TRUE)
calendarDF$Day_of_month <- day(calendarDF$YMD_date)
calendarDF$Week_of_month <- ceiling(as.numeric(format(calendarDF$YMD_date, "%d")) / 7L)
calendarDF$Week_of_year <- week(calendarDF$YMD_date)
calendarDF$Day_of_week <- wday(calendarDF$YMD_date)
calendarDF$Day_of_week_name <- wday(calendarDF$YMD_date, label = TRUE)

# an alternative cast here uses mutate for perhaps better legibility
calendar_alt_DF <- calendarDF %>% 
               mutate(Month = month(YMD_date),
                      Week_of_month = ceiling(as.numeric(format(YMD_date, "%d")) / 7L),
                      Day_of_month = day(YMD_date))


# some alternatives not using lubridate
# cal$month        <- as.numeric(format(DF$DATE, "%m"))
# calendarDF$day_of_month <- as.numeric(format(calendarDF$YMD_date, "%d")
# DF$WEEKDAY       <- format(DF$DATE, "%A")

# find the last work weekday of each month
library(data.table)
# chron and data.table are hard on lubridate so be wary
library(chron)
setDT(calendarDF)[, 
          last_weekdaydate_o_month := last(YMD_date[!chron::is.weekend(YMD_date)])
          , by = month(YMD_date)]
# cleanup from hard chron
detach(package:chron)
detach(package:data.table)


# or an alternative
calendarDF$last_weekdaydate_o_month <- ave( 
    calendarDF$YMD_date, 
    months(calendarDF$YMD_date), 
    FUN = function(x) tail(x[ !(weekdays(x) %in% c("Sat","Sun")) ], 1) 
)

# find last weekday name of month
calendarDF$last_weekday_o_month <- ave( 
    weekdays(calendarDF$YMD_date), 
    months(calendarDF$YMD_date), 
    FUN = function(x) tail(x[ !(x %in% c("Saturday","Sunday")) ], 1) 
)

# group calendar for actions by month
calendarDF %>% group_by(Month_name)

# output last work weekday name of each month
calendarDF %>% group_by(Month) %>% 
    filter(!Day_of_week_name %in% c("Sat", "Sun")) %>%
    summarise(last_weekday = wday(max(YMD_date), label = TRUE))


# Let's add a variable for temps to each day
TEMP = c(-10, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
temps <- mutate(calendarDF, temp = sample(TEMP, nrow(calendarDF), replace = TRUE))
# Here is an example using within() to categorize the entire df
temps <- within(temps, {
    Feels                           <- NA
    Feels[30 <= temp & temp <=  60] <- "Cold"
    Feels[60 <= temp & temp <=  70] <- "Pleasant"
    Feels[80 <= temp & temp <= 100] <- "Hot"
    Feels[is.na(Feels)]             <- "Freezing"
})

# this doesn't work and needs something like cut
# TODO
# mutate(temps, {
#     Feels                           <- NA
#     Feels[60 <= TEMP & TEMP <=  70] <- "Pleasant"
#     Feels[80 <= TEMP & TEMP <= 100] <- "Hot"
#     Feels[is.na(Feels)]             <- "Cold"
# })

###########################################################
### Plotting
# Here is the example data with monthplot
fit <- stl(log(co2), s.window = 20, t.window = 20)
plot(fit)
op <- par(mfrow = c(2,2))
monthplot(co2, ylab = "data", cex.axis = 0.8)
monthplot(fit, choice = "seasonal", cex.axis = 0.8)
monthplot(fit, choice = "trend", cex.axis = 0.8)
monthplot(fit, choice = "remainder", type = "h", cex.axis = 0.8)
par(op)

# some attempt
plot(temps)
op <- par(mfrow = c(2,2))
monthplot(temps$temp, ylab = "Temp (F)", cex.axis = 0.8)
monthplot(temps$temp, choice = "seasonal", cex.axis = 0.8)
par(op)

# TODO: build at least the co2 if not temps up in ggplot or other timeseries
# would be nice to have faceting worked out



