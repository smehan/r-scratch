###########################################################
### Class that uses rle to determine the lengths of an equal value sequence
### in a data frame, then finds the max of those sequences
###########################################################
    
set.seed (12345)

data <- paste(sample(c("A","C","G","T"),100000,replace=TRUE,prob=rep(0.25,4)))
data <- ifelse(data=="A",1,0)

run_lengths = rle(data == 'A')
max(run_lengths$lengths[run_lengths$values])