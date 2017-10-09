# Strategy: Brute Force.
  pythagoreanTriplet <- function(tripletSum) {
    enumerateAllXValues(tripletSum)
  }
  
  enumerateAllXValues <- function(tripletSum) {
    tripletProduct = 0;
    for (x in 3:tripletSum - 2) {
      tripletProduct = enumerateAllYValues(tripletSum, x);
      if (tripletProduct !=  0)
        break
    }
    tripletProduct
  }
  
  enumerateAllYValues <- function(tripletSum, x) {
    isDebug = T
    tripletProduct = 0
    
    for (y in x + 1 : tripletSum - 2) {
      # Choose an r such that r + x + y = 1000
      r = tripletSum - x - y
      if ( isPythogoreanTriplet(x, r, y) ) {
        tripletProduct = x*y*r
        if (isDebug) {
          str <- sprintf("Triplet (x, y, r) = (%d,%d,%d) where x + y + r = 1000", x,y,r)
          cat(str, "\n")
        }
      }
    }
    tripletProduct
  }
  
  isPythogoreanTriplet<- function(x, r, y) {
    x*x + y*y == r*r
  }
  
  PythagoreanTriplet <-function() {
    result = pythagoreanTriplet(1000);
    cat(result, "\n")
    if (result == 31875000) {
      cat("Pass", "\n")
    } else 
      cat("Fail", "\n")
  }
  
  PythagoreanTriplet()
