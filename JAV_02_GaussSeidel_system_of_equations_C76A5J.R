

# Write a script to implement the Gauss-Seidel method of solving systems of equations!
# More description of the method is provided in the seminar_2_2025.pdf file.

gauss_seidel <- function(A, b, x0, iter) {
  n = nrow(A)
  x = x0
  
  # Checking for conditions
  # The condition of being non-negative
  if (any(A < 0)) {
    stop("Error: There are negative numbers in the matrix")
  }
  
  # The condition of the main diagonal should not contain 0
  if (any(diag(A) == 0)) {
    stop("Error: There are 0s in the main diagonal")
  }
  
  results <- matrix(0, nrow = iter, ncol = n)

  # For loop for iteration
  for (k in 1:iter) {
    for (i in 1:n) {
      if (i > 1) {
      s1 = sum(A[i, 1:(i-1)] * x[1:(i-1)])
      } else {
        s1 = 0
      }
      if (i < n) {
      s2 = sum(A[i, (i+1):n] * x[(i+1):n])
      } else {
        s2 = 0
      }
      x[i] = (b[i] - s1 - s2) / A[i,i]
    }
    results[k, ] = x
    cat("Iteration", k, ":", x, "\n")
  }
  return(x)
}

# The test:
A = matrix(c(3, 3, 4,
             3, 9, 3,
             4, 3, 17), nrow=3, byrow=TRUE)

b = c(60, 117, 105)
x0 = c(0, 0, 0)
iter = 25

test = gauss_seidel(A, b, x0, iter)
cat(test)



# Furthermore:

# 1. Write down the quadratic form (in matrix notations) for: a^2 + 2ab + b^2
# x vector for a and b variables
x = c("a", "b")

# A matrix 
A = matrix(c(1, 1,
             1, 1), nrow = 2, byrow = TRUE)

cat("The quadratic form of a^2 + 2ab + b^2 is: A\n")
print(A)

# The test:
a = 2
b = 3
x = c(2, 3)

polynom = a^2 + 2*a*b + b^2
cat(polynom)

quadrat = x %*% A %*% x
cat(quadrat)

# 2. Write down the quadratic form for: x'Ax  xwhere x is a column vector of x1, 
# x2 and x3 and A is the same 3x3 square matrix we used during the practical (see equation (4) in the seminar_2_2025.pdf document).
A = matrix(c(3, 3, 4,
             3, 9, 3,
             4, 3, 17), nrow = 3, byrow = TRUE)

cat("The quadratic form of x'Ax is: A\n")
print(A)

x1 = 1
x2 = 2
x3 = 3
x = c(x1, x2, x3)

polynom = 3*x1^2 + 9*x2^2 + 17*x3^2 + 6*x1*x2 + 8*x1*x3 + 6*x2*x3
cat(polynom)

quadrat = x %*% A %*% x
cat(quadrat)