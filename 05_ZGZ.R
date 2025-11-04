

# Parameters (q stands for 'square')
sigbq = 2 # Var(b)
sigabq = 1 # Var(ab) 
sigeq = 0.5 # Var(e) 

# Observations
# obs1: a1 b1
# obs2: a1 b2
# obs3: a2 b1
# obs4: a2 b2

data = 4

# Creating the Z matrix
Z = matrix(0, nrow = data, ncol = 6)
Z[1,] = c(1, 0, 1, 0, 0, 0)
Z[2,] = c(0, 1, 0, 1, 0, 0)
Z[3,] = c(1, 0, 0, 0, 1, 0) 
Z[4,] = c(0, 1, 0, 0, 0, 1) 

colnames(Z) = c("b1","b2","ab11","ab12","ab21","ab22")
rownames(Z) = paste0("obs",1:4)
cat("Z matrix:\n"); print(Z)

# Creating the diagonal G matrix
G = diag(c(rep(sigbq, 2), rep(sigabq, 4)))
cat("\nG matrix:\n"); print(G)

# R matrix
R = sigeq * diag(data)
cat("\nR matrix:\n"); print(R)


# Counting Var(Y) by Var(Y) = ZGZ' + R
VarY = (Z %*% G %*% t(Z)) + R
cat("\nVar(Y) = Z G Z' + R  (numerical):\n"); print(round(VarY, 6))

