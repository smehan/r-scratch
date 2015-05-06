require(XML)
require(stringr)
myURL <- "http://www.loc.gov/rr/print/list/057_chron.html"
presidents <- readHTMLTable(myURL, which=3, as.data.frame = TRUE,
                            skip.rows = 1, 
                            header = TRUE, 
                            stringsAsFactors = TRUE)
presidents <- presidents[1:64, ]
yearList <- str_split(presidents$YEAR, pattern="-")
yearMatrix <- data.frame(Reduce(rbind, yearList))
names(yearMatrix) <- c("Start", "Stop")
presidents <- cbind(presidents, yearMatrix)
presidents[str_sub(string=presidents$Start, start = 4, end = 4) == 1,
            c("YEAR", "PRESIDENT", "Start", "Stop")]
View(presidents[str_detect(presidents$PRESIDENT, ignore.case("john")), ])

#get a rdata file from jaredlander site with wartimes data
con <- url("http://www.jaredlander.com/data/warTimes.rdata")
load(con)
close(con)

theTimes <- str_split(warTimes, "(ACAEA)|-", n = 2)
theStart <- sapply(theTimes,FUN = function(x) x[1])
theStart <- str_trim(theStart)

str_extract(theStart,pattern = "January")
theStart[str_detect(theStart, "January")]
head(str_extract(theStart, "^\\d{4}$"), 30)
head(str_replace_all(theStart, "\\d{1,4}", "x"), 30)

#practice replacing bad html scraped
badSyntax <- c("<a href=index.html>The link is here</a>",
               "<b>This is bold text</b>")
# grab everything between the anchors with regex
str_replace(badSyntax, "<.+?>(.+?)<.+?>", "\\1")
