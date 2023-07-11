library("purrr")
library("microbenchmark")

Factorial_loop <- function(n) {
  if ((n%%1 == 0) && (n>=0)) {
    ans = 1
    while (n>0){
      ans <- ans*n
      n <- n-1
    }
  }
  else {
    ans = NA
  }
  ans
}

Factorial_reduce <- function(n){
  if ((n%%1 == 0) && (n>0)) {
    ans <- reduce(c(n:1), function(x, y){
      x * y
    })
  }
  else if (n == 0) {
    ans = 1
  }
  else {
    ans = NA
  }
  ans
}

Factorial_func <- function(n){
  if ((n%%1 == 0) && (n >= 0)) {
    stopifnot(n >= 0)
    if (n == 0) {
      1
    } else if (n == 1) {
      1
    } else {
      n * Factorial_func(n - 1)
    }
  }
  else {
    return(NA)
  }
}

Factorial_mem <- function(n){
  if ((n%%1 == 0) && (n >= 0)) {
    if (n == 0) {
      return(1)
    } else if (n == 1) {
      return(1)
    } else {
      fac_tbl <- c(1,rep(NA,n-1))
      fac_mem <- function(n) {
        stopifnot(n > 0)
  
        if(!is.na(fac_tbl[n])){
          fac_tbl[n]
        } else {
          fac_tbl[n - 1] <<- fac_mem(n - 1)
          n * fac_tbl[n - 1]
        }
      }
      fac_mem(n)
    }
  }
  else {
    return(NA)
  }
}

Factorial_mem_old <- function(n){
  fac_tbl <- c(1,rep(NA,n-1))
  
  if ((n%%1 == 0) && (n >= 0)) {
    if (n == 0) {
      return(1)
    } else if (n == 1) {
      return(1)
    } else {
      fac_tbl <- c(1,rep(NA,n-1))
      stopifnot(n > 0)
      paste(fac_tbl)
      
      if(!is.na(fac_tbl[n])){
        fac_tbl[n]
      } else {
        fac_tbl[n - 1] <<- Factorial_mem(n - 1)
        n * fac_tbl[n - 1]
      }
    }
  }
  else {
    return(NA)
  }
}

bench <- function(n=500,random = FALSE) {
  if (random == TRUE) {
    n <- sample(1:n, 1, replace=F)
  }
  message("Using n = ",n)
  microbenchmark(Factorial_loop(n),Factorial_reduce(n),
                 Factorial_func(n),Factorial_mem(n))
}