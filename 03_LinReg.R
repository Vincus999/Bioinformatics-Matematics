

# Fit the following model to the simulated data in the lm_model.tsv file (also see the description in data_description.txt).

# Model to fit: 
#  z = beta0 + beta1x + beta2y + eps
# Use R's built-in lm() function to fit the data, but also calculate beta values with the matrix algebra 
# (important note: the equation below uses general linear regression model notation 
# (as in the LM_matrix_elmelet_2025 document) and 
# LaTeX: y corresponds to the dependent variable, which is z in our example above; X is the design matrix as we discussed during the seminar):
# beta = (X'X)^(-1) * X'y

model = read.table("/home/win/Bioinfo_SZ/lm_model.tsv", header = TRUE, sep = "\t")
head(model)

# Fitting the model
fit = lm(z ~ x + y, data = model)
summary(fit)

# Design matrix for the model
X = cbind(1, model$x, model$y)
y = data$z
beta = solve(t(X) %*% X) %*% t(X) %*% y
beta

coef(fit)


# Furthermore:
# 1. Write briefly about the coefficient of determination (R2)!
# Write down its lower and upper limits, how to calculate it and what is its significance.


