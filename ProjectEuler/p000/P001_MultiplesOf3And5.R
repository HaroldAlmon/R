# This is a variation of the sum(1..n) formula.
#
# Sum(1...kn) = k * n * (n + 1)
#                   -----------
#                        2
#

# The formula includes n, the problem does not.
 calculateSumBelowLimit <- function(upperLimit, divisor) {
  n <- floor((upperLimit - 1)/divisor)
  result <- floor(divisor * n * ( n + 1 ) / 2) 
  result
}

# Strategy: Simple Mathematics. Time complexity is O(1)
multiplesOf3And5 <- function(upperLimit)  {
    result <- calculateSumBelowLimit(upperLimit, 3) +
      calculateSumBelowLimit(upperLimit, 5) -
      calculateSumBelowLimit(upperLimit, 15);
    result
}

multiplesOf3And5(1000)
