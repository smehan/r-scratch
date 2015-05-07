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
