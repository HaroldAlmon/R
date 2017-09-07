
fiboSumImpl <- function(fibo1, fibo2, total, upperLimit) {
  nextFibo = fibo1 + fibo2;
  if ( nextFibo < upperLimit) {
    if(nextFibo %% 2 == 0)
      return (fiboSumImpl(fibo2, nextFibo, total + nextFibo, upperLimit))
    else
      return (fiboSumImpl(fibo2, nextFibo, total, upperLimit))
  }
  else
    total
}

fiboSum <- function(upperLimit) {
  fiboSumImpl(1, 2, 2, upperLimit);
}

fiboSum(4000000)
  

