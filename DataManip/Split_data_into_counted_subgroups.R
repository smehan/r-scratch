***********************************************************
*** Class to segment existing columns into smaller groups,
*** then count those members and melt the data into a wide format
***********************************************************
    
Times <- read.table(text = "Times
                    1     NA
                    2  0.448
                    3  0.130
                    4     NA
                    5     NA
                    6  0.462
                    7  0.427
                    8  0.946
                    9  0.227
                    10    NA", header = TRUE)
    
#group values that belong together
Times$grp <- cumsum(is.na(Times$Times)) %/% 2  + 1
    
#remove NA values
Times <- na.omit(Times)
    
# sequence the columns
Times$col <- unlist(tapply(Times$grp, factor(Times$grp), seq_along))
    
#reshape to wide format
reshape(Times, timevar = "col", idvar="grp", direction="wide")