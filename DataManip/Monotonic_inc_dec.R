###########################################################
# Determine if a vector is monotonically increasing or decreasing
###########################################################

# first, generate a couple of sequences

set.seed(1)
x = sample(10) 
set.seed(2)
y = sample(10)
# monotonic increasing
all(x == cummax(x))

# also

all(diff(x) >= 0)

# where are inflection points, note flip of comparison operator
which(diff(x) <= 0)
# first element is discardable
# this yields first inflection point, without the adjustment as above, but only the first one
which.max( x < cummax(x) )

# alternatively, rather than test for equality, one should cast as
all(!diff(x) < 0)

# and similarly with decreasing
all(x == cummin(x))
all(diff(x) <= 0)
which(diff(x) >= 0)

# the following affect the sequence in a vector fashion based upon the first inflection point
replace(x, seq_along(x) >= which(x < cummax(x))[1], NA)
# replacing all values past the first inflection point with NA that are monotonically decreasing
# and for a strictly decreasing sequence
r <- rank(x) + sort(runif(length(x)), decreasing=TRUE)
replace(x, seq_along(x) >= which(r < cummax(r))[1], NA)


