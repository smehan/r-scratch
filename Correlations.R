###########################################################
### Read in text block as a matrix, then manipulate the matrix
### into a df suitable for correlation calcs
###########################################################

library(ggplot2)

x <- as.matrix(read.table(text = "AUS   AUT   CAN   CHE   DEU  EU15   FRA   GBR   ITA   JPN   USA
                          AUS  1.000 0.058 0.476 0.313 0.111 0.277 0.184 0.296 0.202 0.192 0.267
                          AUT  0.058 1.000 0.254 0.658 0.749 0.761 0.626 0.387 0.460 0.410 0.278
                          CAN  0.476 0.254 1.000 0.390 0.321 0.534 0.377 0.538 0.391 0.231 0.746
                          CHE  0.313 0.658 0.390 1.000 0.604 0.706 0.610 0.310 0.565 0.437 0.305
                          DEU  0.111 0.749 0.321 0.604 1.000 0.859 0.620 0.387 0.472 0.520 0.369
                          EU15 0.277 0.761 0.534 0.706 0.859 1.000 0.808 0.682 0.713 0.601 0.531
                          FRA  0.184 0.626 0.377 0.610 0.620 0.808 1.000 0.467 0.553 0.444 0.357
                          GBR  0.296 0.387 0.538 0.310 0.387 0.682 0.467 1.000 0.324 0.407 0.591
                          ITA  0.202 0.460 0.391 0.565 0.472 0.713 0.553 0.324 1.000 0.492 0.315
                          JPN  0.192 0.410 0.231 0.437 0.520 0.601 0.444 0.407 0.492 1.000 0.321
                          USA  0.267 0.278 0.746 0.305 0.369 0.531 0.357 0.591 0.315 0.321 1.000"))
x[lower.tri(x)] <- NA # clean up half the matrix
diag(x) <- NA # then the diagonal

df <- subset(as.data.frame(as.table(x), responseName = 'Corr'),!is.na(Corr)) # make a df
df <- df[order(df$Corr), ] # order the df

# now plot the corr matrix as a scatterplot isolating a single column of the df
ggplot(df, aes(x=1:nrow(df),y=Corr,col=Var2=='USA')) + geom_point()

# normal correlation heatmap
corrplot(x, is.corr = FALSE, method='square', diag=FALSE)