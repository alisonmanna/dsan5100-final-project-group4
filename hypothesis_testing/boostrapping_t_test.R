library(tidyverse)

df <- read.csv("data/cleaned/natality_cleaned.csv")

# Detailed birth weight 
outcome <- "dbwt"

# Creating chronic condition flag based on columns (true = chronic)
df$chronic_condition <- ifelse(
  df$rf_pdiab == 1 | df$rf_gdiab == 1 |
  df$rf_phype == 1 | df$rf_ghype == 1 |
  df$rf_ehype == 1,
  1, 0
)

# Splitting into chronic vs non-chronic groups 
group_chronic <- df %>%
  filter(chronic_condition == 1) %>%
  pull(outcome)

group_nonchronic <- df %>%
  filter(chronic_condition == 0) %>%
  pull(outcome)

# ===== PART 1: Original Data Histograms with Normal Overlay =====
# Save figure to file
png("plots/original_data_histograms.png", width = 1200, height = 600, res = 100)
par(mfrow = c(1, 2))

# Histogram for chronic group with normal overlay
hist(group_chronic, breaks = 50, freq = FALSE, 
     main = "Birth Weight - Chronic Conditions",
     xlab = "Birth Weight (grams)", 
     col = "coral", border = "white")
# Add normal curve overlay
mean_chronic <- mean(group_chronic, na.rm = TRUE)
sd_chronic <- sd(group_chronic, na.rm = TRUE)
curve(dnorm(x, mean = mean_chronic, sd = sd_chronic), 
      col = "darkred", lwd = 2, add = TRUE)
legend("topright", legend = "Normal Dist", col = "darkred", lwd = 2)

# Histogram for non-chronic group with normal overlay
hist(group_nonchronic, breaks = 50, freq = FALSE,
     main = "Birth Weight - No Chronic Conditions",
     xlab = "Birth Weight (grams)", 
     col = "steelblue", border = "white")
# Add normal curve overlay
mean_nonchronic <- mean(group_nonchronic, na.rm = TRUE)
sd_nonchronic <- sd(group_nonchronic, na.rm = TRUE)
curve(dnorm(x, mean = mean_nonchronic, sd = sd_nonchronic), 
      col = "darkblue", lwd = 2, add = TRUE)
legend("topright", legend = "Normal Dist", col = "darkblue", lwd = 2)
dev.off()  # Close the saved figure

# ===== PART 2: Bootstrap Sampling =====
set.seed(5100)
B <- 10000

# Bootstrap for chronic group
boot_mean_chronic <- numeric(B)
n_chronic <- length(group_chronic)

for(i in 1:B) {
  boot_sample <- sample(group_chronic, n_chronic, replace = TRUE)
  boot_mean_chronic[i] <- mean(boot_sample)
}

# Bootstrap for non-chronic group
boot_mean_nonchronic <- numeric(B)
n_nonchronic <- length(group_nonchronic)

for(i in 1:B) {
  boot_sample <- sample(group_nonchronic, n_nonchronic, replace = TRUE)
  boot_mean_nonchronic[i] <- mean(boot_sample)
}

# ===== PART 3: Bootstrap Distribution Histograms with Normal Overlay =====
# Save figure to file
png("plots/bootstrap_distributions.png", width = 1200, height = 600, res = 100)
par(mfrow = c(1, 2))

# Histogram for bootstrap chronic distribution with normal overlay
hist(boot_mean_chronic, breaks = 50, freq = FALSE,
     main = "Bootstrap Dist - Chronic Mean Birth Weight",
     xlab = "Mean Birth Weight (grams)", 
     col = "coral", border = "white")
# Add normal curve overlay
boot_mean_chronic_mean <- mean(boot_mean_chronic)
boot_mean_chronic_sd <- sd(boot_mean_chronic)
curve(dnorm(x, mean = boot_mean_chronic_mean, sd = boot_mean_chronic_sd), 
      col = "darkred", lwd = 2, add = TRUE)
legend("topright", legend = "Normal Dist", col = "darkred", lwd = 2)

# Histogram for bootstrap non-chronic distribution with normal overlay
hist(boot_mean_nonchronic, breaks = 50, freq = FALSE,
     main = "Bootstrap Dist - Non-Chronic Mean Birth Weight",
     xlab = "Mean Birth Weight (grams)", 
     col = "steelblue", border = "white")
# Add normal curve overlay
boot_mean_nonchronic_mean <- mean(boot_mean_nonchronic)
boot_mean_nonchronic_sd <- sd(boot_mean_nonchronic)
curve(dnorm(x, mean = boot_mean_nonchronic_mean, sd = boot_mean_nonchronic_sd), 
      col = "darkblue", lwd = 2, add = TRUE)
legend("topright", legend = "Normal Dist", col = "darkblue", lwd = 2)
dev.off()  # Close the saved figure

# ===== PART 4: T-Tests =====
# Original t-test
cat("\n===== ORIGINAL T-TEST =====\n")
t_test_result <- t.test(group_chronic, group_nonchronic)
print(t_test_result)

# Bootstrap t-test (comparing bootstrap distributions)
cat("\n\n===== BOOTSTRAP T-TEST =====\n")
boot_t_test_result <- t.test(boot_mean_chronic, boot_mean_nonchronic)
print(boot_t_test_result)

# Summary statistics
cat("\n\n===== SUMMARY STATISTICS =====\n")
cat("Original Data:\n")
cat(sprintf("  Chronic group: Mean = %.2f, SD = %.2f, n = %d\n", 
            mean_chronic, sd_chronic, n_chronic))
cat(sprintf("  Non-chronic group: Mean = %.2f, SD = %.2f, n = %d\n", 
            mean_nonchronic, sd_nonchronic, n_nonchronic))

cat("\nBootstrap Distributions:\n")
cat(sprintf("  Chronic bootstrap: Mean = %.2f, SD = %.2f\n", 
            boot_mean_chronic_mean, boot_mean_chronic_sd))
cat(sprintf("  Non-chronic bootstrap: Mean = %.2f, SD = %.2f\n", 
            boot_mean_nonchronic_mean, boot_mean_nonchronic_sd))