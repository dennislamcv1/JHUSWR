# factorial_code.R

# Load necessary libraries
library(purrr)
library(microbenchmark)

# Factorial using a loop
Factorial_loop <- function(n) {
  if (n == 0) return(1)
  result <- 1
  for (i in 1:n) {
    result <- result * i
  }
  return(result)
}

# Factorial using reduce
Factorial_reduce <- function(n) {
  if (n == 0) return(1)
  return(reduce(1:n, `*`))
}

# Factorial using recursion
Factorial_func <- function(n) {
  if (n == 0) return(1)
  return(n * Factorial_func(n - 1))
}

# Factorial using memoization
Factorial_mem <- local({
  memo <- c(1)
  function(n) {
    if (length(memo) >= n + 1) return(memo[[n + 1]])
    result <- if (n == 0) 1 else n * Recall(n - 1)
    memo <<- c(memo, result)
    return(result)
  }
})

# Benchmarking the four factorial functions
benchmark_results <- microbenchmark(
  loop = Factorial_loop(10),
  reduce = Factorial_reduce(10),
  recursive = Factorial_func(10),
  memoization = Factorial_mem(10)
)

# Print benchmark results
print(benchmark_results)

