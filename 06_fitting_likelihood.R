
# Fit the linear mixed model as described in the data_description.txt file. 
# Linear regression model: zi = beta0 + beta1 * xi + beta2 * yi + beta3 * ui + epsiloni
# Linear mixed model: z = intercept + x + y + u + epsilon

# 1.) With u as a random effect! A function to fit a linear model in R: lm () 
# A function to fit linear mixed model in R (lme4 R package): lmer ()
# Read the LM and LMM data files
lm = read.table("/home/win/Egyetem/Bioinfo_SZ/lm_model.tsv", header = TRUE, sep = "\t")
lmm = read.table("/home/win/Egyetem/Bioinfo_SZ/lmm_data.tsv", header = TRUE, sep = "\t")

str(lm)
str(lmm)
head(lm)
head(lmm)

# Fit the linear model
lm_fit = lm(z ~ x + y, data = lm)
summary(lm_fit)

# Fit the linear mixed model
install.packages("lme4")
library(lme4)

lmm_fit = lmer(z ~ x + y + (1 | u), data = lmm)
summary(lmm_fit)

epsilon = resid(lmm_fit)
head(epsilon)

# Compare the models using ANOVA
anova(lmm_fit)

summary(lm_fit)$coefficients
summary(lmm_fit)$coefficients

# 2.) Calculate likelihood and log-likelihood for a binary data with n_success = 25 and n_failure = 75. 
# Also plot the data!
n_success = 25
n_failure = 75
n_total = n_success + n_failure
stepsize = 0.001
p_hat = n_success / n_total

# Calculating likelihood and log-likelihood for a set of p's.
pval_all = seq(0, 1, stepsize)
N = length(pval_all)
likelihood_est = numeric(N)
log_likelihood_est = numeric(N)

for (i in seq_along(pval_all)) {
  pval = pval_all[i]
  
  if (pval == 0 || pval == 1) {
		likelihood_est[i] = 0
		log_likelihood_est[i] = NA
		} else {
	    likelihood_est[i] = pval^n_success * (1 - pval)^n_failure
	    log_likelihood_est[i] = n_success * log(pval) + n_failure * log(1-pval)
		}
}

# Plotting the likelihood and log-likelihood functions
# Likelihood plot
plot(pval_all, likelihood_est, type = "l", col = "blue", lwd = 2,
     main = "Likelihood Function for Binomial Data",
     xlab = "Probability of Success (p)",
     ylab = "Likelihood")
abline(v = p_hat, col = "red", lwd = 2, lty = 2)

# Log-Likelihood plot
plot(pval_all, log_likelihood_est, type = "l", col = "green", lwd = 2, 
	 main = "Log-Likelihood Function for Binomial Data", 
	 xlab = "Probability of Success (p)", 
	 ylab = "Log-Likelihood")
abline(v = p_hat, col = "red", lwd = 2, lty = 2)

