# Strategy: Brute Force.
largestPalindromeProduct <- function(numberLength) {
      if (numberLength < 1) 
      {
        cat("Number length must be greater than zero")
        return -1
      }
      upperLimit = 10^numberLength - 1
      largestProduct = 0
      
      largestProduct = reverseEnumerateNum1(upperLimit, largestProduct)
      largestProduct
}

reverseEnumerateNum1 <- function(upperLimit, largestProduct) {
      product = largestProduct
      num1 = upperLimit
      while ( num1 > upperLimit/10 ) {
        product = reverseEnumerateNum2(upperLimit, product, num1)
        num1 <- num1 - 1
      }
      product
    }
    
reverseEnumerateNum2 <- function(upperLimit, inputProduct, num1) {
      largestProduct = inputProduct
      num2 = upperLimit
      while ( num2 > upperLimit/10 ) {
        product = num1 * num2
        if (isPalindrome(product) )
        {
          if (product > largestProduct) {
            largestProduct = product
            break
          }
        }
        num2 <- num2 - 1
      }
      largestProduct
    }
    
isPalindrome <- function(n) {
      number = as.character(n)
      left = 0
      right = length(number) - 1
      while (left < right ) {
        if (substring(number, left, left) != substring(number, right, right)) {
          return(FALSE)
        }
        left <- left + 1
        right <- right - 1
      }
      TRUE
}

result <- largestPalindromeProduct(3)
result
