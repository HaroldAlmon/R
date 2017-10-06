# Strategy: Brute Force.
# Similar to finding the maximum palindrome in the set (n1 thru 1) * (n2 thru 1)

largestPalindromeProduct <- function(numberLength) {
      if (numberLength < 1) {
        cat("Number length must be greater than zero")
        return -1
      }
      upperLimit = 10^numberLength - 1
      largestProduct = 0
      
      # No need to go to 1 for the lower limit, use a higher limit (0.9 of upper limit) for faster speed...
      largestProduct = enumerate_n1(upperLimit, upperLimit * 0.9, largestProduct)
      largestProduct
}

enumerate_n1 <- function(upperLimit, lowerLimit, largestProduct) {
      product = largestProduct
      
      for (num1 in upperLimit:lowerLimit) {
        product = enumerate_n2(upperLimit, lowerLimit, product, num1)
      }
      
      product
    }
    
enumerate_n2 <- function(upperLimit, lowerLimit, inputProduct, num1) {
      largestProduct = inputProduct
      
      for ( num2 in upperLimit:lowerLimit ) {
        
        #See comment at the beginning...
        product = num1 * num2
        if (isPalindrome(product) ) {
          if (product > largestProduct) {
            largestProduct = product
            break
          }
        }
      }
      
      largestProduct
    }
    
isPalindrome <- function(n) {
      number = as.character(n)
      left = 1
      right = nchar(number)
      
      while (left < right ) {
        if (substring(number, left, left) != substring(number, right, right)) 
          return(FALSE)
        
        left <- left + 1
        right <- right - 1
      }
      TRUE
}

result <- largestPalindromeProduct(4)
cat(result, "\n")

if (result == 99000099) {
  cat("Pass")
} else 
  cat("Fail")
