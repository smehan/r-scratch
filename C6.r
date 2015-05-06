#function declaration
say.hello <- function(){
    print("Hello, World!")
}

say.hello()

sprintf("Hello, %s", "Boss")
sprintf("Hello, %s, today is %s", "Boss", "Sunday")

#function declaration with a parameter
hello.person <- function(name){
    print(sprintf("Hello, %s", name))
}

hello.person("Minnie")

#indefinite number of parameters
hello.person <- function(first, last="Doe"){
    print(sprintf("Hello, %s %s", first, last))
}

hello.person("Minnie", "Mouse")

#unlabeled param using ...
hello.person <- function(first, last="Doe", ...){
    print(sprintf("Hello %s %s", first, last))
}

hello.person("Mickey", "Mouse", extra="goodbye")

double.num <- function(x){
    return(x * 2)
    print("Hello")
    return(17)
}

double.num(3)

fib <- function(n){
    if (n == 1){
        return(1)
    } else if (n ==2){
        return(1)
    } else {
        return (fib(n-1) + fib(n-2))
    }
}

fib(8)


use.switch <- function(x){
    switch(x,
           "a"="first",
           "b"="second",
           "z"="last",
           "c"="third",
           "other")
}

x <- 1
y <- 2
ifelse(x == 1 & y == 3, "Yes", "No")

#For loops
states <- c("California", "North Carolina", "Nebraska", "Oklahoma", 
            "Missouri", "Virginia", "New York")
statesLength <- rep(NA, length(states))
names(statesLength) <- states
for (i in states){
    statesLength[i] <- nchar(i)    
}

# but better is to verctorize
statesLength <- nchar(states)
names(statesLength) <- states

#while loop
x <- 1
while (x <= 5){
    print(x)
    x <- x + 1
}

#also have next and break