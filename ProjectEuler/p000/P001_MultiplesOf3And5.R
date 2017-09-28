# This is a variation of the sum(1..n) formula.
#
# Sum(1...kn) = k * n * (n + 1)
#                   -----------
#                        2
#

# The formula includes n, the problem does not.
 calculateSumBelowLimit <- function(upperLimit, divisor) {
  n <- floor((upperLimit - 1)/divisor)
  floor(divisor * n * ( n + 1 ) / 2) 
}

# Strategy: Simple Mathematics. Time complexity is O(1)
multiplesOf3And5 <- function(upperLimit)  {
    calculateSumBelowLimit(upperLimit, 3) +
      calculateSumBelowLimit(upperLimit, 5) -
      calculateSumBelowLimit(upperLimit, 15);
}

# Unit test
result = multiplesOf3And5(1000)
if ( answer != 233168 ) {
    cat( "Result = ", result,", fail")
  } else {
    cat( "Result = ", result,", pass")
  }
