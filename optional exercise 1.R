# Function for coefficient of variation
cv <- function(x) sd(x) / mean(x)

# Simulate data and compare CV vs SD of log(x)
set.seed(1)
n_iter <- 1000
results <- matrix(NA, ncol = 2, nrow = n_iter)
colnames(results) <- c("CV", "SD_log")

for (i in seq_len(n_iter)) {
  sd_i <- runif(1, 0.1, 5)
  x <- rnorm(50, mean = 10, sd = sd_i)
  results[i, "CV"] <- cv(x)
  results[i, "SD_log"] <- sd(log(x))
}

# Correlation between CV and SD(log)
cor(results[, "CV"], results[, "SD_log"])

# Plot relationship
plot(results[, "CV"], results[, "SD_log"],
     xlab = "CV(x)",
     ylab = "SD(log[x])",
     main = "Similarity of SD(log[x]) and CV(x)")
abline(0, 1, col = "red", lwd = 2)
