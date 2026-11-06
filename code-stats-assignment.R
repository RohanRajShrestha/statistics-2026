read_data <- function(file_name) {
  full_path <- file.path("C:/Users/User/Downloads/", file_name)
  read.csv(full_path, header = TRUE)
}

x_data <- read_data("X-c32ff126-a875-487a-865f-4376c19277d2.csv")
y_data <- read_data("y-59b0d90f-7113-41fc-a469-31996071f782.csv")
time_data <- read_data("time-ebe3ec9a-dd4e-41fc-8c70-c3d0e6451d3d.csv")

cat("\014")
cat("Dimenstion of x: (row | columns)", dim(x_data), "\n")
cat("Dimensions of y: (row | columns)", dim(y_data), "\n")
cat("Dimensions of time: (row | columns)", dim(time_data), "\n")

colnames(x_data)
colnames(y_data)
colnames(time_data)
# combine data
colnames(y_data) <- "total_power_consumption"
colnames(time_data) <- "time"

full_data <- cbind(time_data, x_data, y_data)

# view combined data
head(full_data)
colnames(full_data)

# Data Quality & Sign Breakdown Diagnostic Function
get_data_quality_report <- function(df) {
  sapply(df, function(x) c(
    Total_Rows    = length(x),
    NA_Count      = sum(is.na(x)),
    Empty_Count   = sum(x == "" | is.null(x), na.rm = TRUE),
    Zero_Count    = sum(x == 0, na.rm = TRUE),
    Negative_Count = sum(x < 0, na.rm = TRUE),
    Positive_Count = sum(x > 0, na.rm = TRUE)
  ))
}

# Run diagnostics on all variables 
quality_summary <- t(get_data_quality_report(full_data))

cat("=== DATA QUALITY DIAGNOSTIC SUMMARY ===\n")
print(quality_summary)


# Time-Series Plot
# It is generating a Time-Series Plot 
# (a line graph showing how a single variable changes sequentially over time $t$). 
# Set layout for 5 stacked rows

# Open a taller graphics window (height = 10 inches, width = 8 inches)
dev.new(width = 10, height = 10) 

# mar = c(bottom, left, top, right)
par(mfrow = c(5, 1), mar = c(2, 6, 1.5, 4))

plot(full_data$time, full_data$cpu_utilization, type = "l", col = "blue", 
     xlab = "Time (s)", ylab = "CPU", main = "CPU Utilization over Time")

plot(full_data$time, full_data$memory_usage, type = "l", col = "green", 
     xlab = "Time (s)", ylab = "Memory", main = "Memory Usage over Time")

plot(full_data$time, full_data$disk_io, type = "l", col = "orange", 
     xlab = "Time (s)", ylab = "Disk I/O", main = "Disk I/O over Time")

plot(full_data$time, full_data$network_traffic, type = "l", col = "purple", 
     xlab = "Time (s)", ylab = "Network", main = "Network Traffic over Time")

plot(full_data$time, full_data$total_power_consumption, type = "l", col = "red", 
     xlab = "Time (s)", ylab = "Power (W)", main = "Total Power Draw over Time")

# Reset plotting window back to single panel
par(mfrow = c(1, 1))


# ==============================================================================
# TASK 1: Visualizing Density Curves and Scatter Plots
# ==============================================================================

# Ensure full_data is loaded
# full_data <- cbind(time_data, x_data, y_data)
# ------------------------------------------------------------------------------
# Boxplot for all telemetry signals and target
# ------------------------------------------------------------------------------

dev.new(width = 10, height = 10)
par(mfrow = c(1, 5), mar = c(4, 4, 2, 1))
boxplot(full_data$cpu_utilization, main = "CPU", col = "lightblue")
boxplot(full_data$memory_usage, main = "Memory", col = "lightgreen")
boxplot(full_data$disk_io, main = "Disk I/O", col = "orange")
boxplot(full_data$network_traffic, main = "Network", col = "plum")
boxplot(full_data$total_power_consumption, main = "Power", col = "coral")
par(mfrow = c(1, 1))

# ------------------------------------------------------------------------------
# Histograms (Signal Distributions)
# ------------------------------------------------------------------------------
dev.new(width = 10, height = 10)
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))

hist(full_data$cpu_utilization, col = "lightblue", 
     main = "CPU Utilization", xlab = "CPU %")

hist(full_data$memory_usage, col = "lightgreen", 
     main = "Memory Usage", xlab = "Memory")

hist(full_data$disk_io, col = "orange", 
     main = "Disk I/O", xlab = "Disk I/O")

hist(full_data$network_traffic, col = "plum", 
     main = "Network Traffic", xlab = "Network")

hist(full_data$total_power_consumption, col = "coral", 
     main = "Power Consumption", xlab = "Power (W)")

par(mfrow = c(1, 1))

# ------------------------------------------------------------------------------
# Histograms Overlaid with Density Curves (PDF)
# ------------------------------------------------------------------------------
dev.new(width = 10, height = 10)
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

# Helper function to plot histogram + density curve
plot_density_hist <- function(data_vector, col_fill, title_name, x_label) {
  # freq = FALSE scales histogram to density so the curve aligns correctly
  hist(data_vector, freq = FALSE, col = col_fill, main = title_name, xlab = x_label)
  lines(density(data_vector), col = "darkred", lwd = 2)
}

plot_density_hist(full_data$cpu_utilization, "lightblue", "CPU Utilization", "CPU %")
plot_density_hist(full_data$memory_usage, "lightgreen", "Memory Usage", "Memory")
plot_density_hist(full_data$disk_io, "orange", "Disk I/O", "Disk I/O")
plot_density_hist(full_data$network_traffic, "plum", "Network Traffic", "Network")
plot_density_hist(full_data$total_power_consumption, "coral", "Power Consumption", "Power (W)")

par(mfrow = c(1, 1)) # Reset grid layout


# ------------------------------------------------------------------------------
# Scatter Plots: Telemetry Features vs Target Power (y)
# ------------------------------------------------------------------------------
dev.new(width = 10, height = 10)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Feature 1 vs y
plot(full_data$cpu_utilization, full_data$total_power_consumption,
     col = rgb(0, 0, 1, 0.4), pch = 16, xlab = "CPU Utilization", ylab = "Total Power (W)",
     main = "CPU vs Power")
abline(lm(total_power_consumption ~ cpu_utilization, data = full_data), col = "red", lwd = 2)

# Feature 2 vs y
plot(full_data$memory_usage, full_data$total_power_consumption,
     col = rgb(0, 0.8, 0, 0.4), pch = 16, xlab = "Memory Usage", ylab = "Total Power (W)",
     main = "Memory vs Power")
abline(lm(total_power_consumption ~ memory_usage, data = full_data), col = "red", lwd = 2)

# Feature 3 vs y
plot(full_data$disk_io, full_data$total_power_consumption,
     col = rgb(1, 0.5, 0, 0.4), pch = 16, xlab = "Disk I/O", ylab = "Total Power (W)",
     main = "Disk I/O vs Power")
abline(lm(total_power_consumption ~ disk_io, data = full_data), col = "red", lwd = 2)

# Feature 4 vs y
plot(full_data$network_traffic, full_data$total_power_consumption,
     col = rgb(0.5, 0, 0.5, 0.4), pch = 16, xlab = "Network Traffic", ylab = "Total Power (W)",
     main = "Network vs Power")
abline(lm(total_power_consumption ~ network_traffic, data = full_data), col = "red", lwd = 2)

par(mfrow = c(1, 1)) # Reset grid layout


# ------------------------------------------------------------------------------
# Summary Statistics
# ------------------------------------------------------------------------------
get_summary_stats <- function(df) {
  sapply(df, function(x) c(
    Mean     = mean(x, na.rm = TRUE),
    Median   = median(x, na.rm = TRUE),
    SD       = sd(x, na.rm = TRUE),
    IQR      = IQR(x, na.rm = TRUE),
    Min      = min(x, na.rm = TRUE),
    Max      = max(x, na.rm = TRUE)
  ))
}

summary_table <- t(get_summary_stats(full_data[, 2:6])) # Exclude time column
print(round(summary_table, 3))



# Scatterplot matrix across all features
dev.new(width = 10, height = 10)
pairs(full_data[, c("cpu_utilization", "memory_usage", "disk_io", "network_traffic")],
      col = "blue", main = "Scatterplot Matrix of Input Features")


# Shapiro-Wilk Test on sample (max 5000 rows for Shapiro test)
sample_data <- full_data[sample(nrow(full_data), min(500, nrow(full_data))), ]

shapiro_results <- sapply(sample_data[, 2:6], function(x) shapiro.test(x)$p.value)
cat("=== SHAPIRO-WILK TEST P-VALUES ===\n")
print(round(shapiro_results, 5))


# Fit baseline Multiple Linear Model
baseline_lm <- lm(total_power_consumption ~ cpu_utilization + memory_usage + disk_io + network_traffic, data = full_data)

# Print Summary (Coefficients, R-squared)
summary(baseline_lm)

# ------------------------------------------------------------------------------
# Correlation Matrix
# ------------------------------------------------------------------------------
cor_matrix <- cor(
  full_data[, c(
      "cpu_utilization",
      "memory_usage", 
      "disk_io",
      "network_traffic", 
      "total_power_consumption"
    )
  ], method="pearson"
)

cat("=== CORRELATION MATRIX ===\n")
print(cor_matrix)

# Select numerical variables
# signals <- full_data[, c(
#   "cpu_utilization",
#   "memory_usage", 
#   "disk_io",
#  "network_traffic", 
#  "total_power_consumption"
# )]

# Spearman
# spearman <- cor(signals, method = "spearman")

# Kendall
# kendall <- cor(signals, method = "kendall")

# Display results
# print(spearman)
# print(kendall)

# ------------------------------------------------------------------------------
# TASK 2: POLYNOMIAL REGRESSION MODELING & SELECTION
# ------------------------------------------------------------------------------

x1 <- full_data$cpu_utilization
x2 <- full_data$memory_usage
x3 <- full_data$disk_io
x4 <- full_data$network_traffic
y  <- full_data$total_power_consumption
n  <- length(y)
onesMatrix <- matrix(1, nrow = n, ncol = 1)

# Design Matrices (X) for Models 1 to 5
XData_model1 <- cbind(x4, x1^2, x1^3, x2^4, x1^4, onesMatrix)
XData_model2 <- cbind(x4, x1^3, x3^4, onesMatrix)
XData_model3 <- cbind(x3^3, x3^4, onesMatrix)
XData_model4 <- cbind(x2, x1^3, x3^4, onesMatrix)
XData_model5 <- cbind(x4, x1^2, x1^3, x3^4, onesMatrix)

# 2.1 Least Squares Estimation
model1_Thetahat <- solve(t(XData_model1) %*% XData_model1) %*% t(XData_model1) %*% y
model2_Thetahat <- solve(t(XData_model2) %*% XData_model2) %*% t(XData_model2) %*% y
model3_Thetahat <- solve(t(XData_model3) %*% XData_model3) %*% t(XData_model3) %*% y
model4_Thetahat <- solve(t(XData_model4) %*% XData_model4) %*% t(XData_model4) %*% y
model5_Thetahat <- solve(t(XData_model5) %*% XData_model5) %*% t(XData_model5) %*% y

# Construct Tabular Display of Parameter Estimates (Theta Hat)
theta_table <- data.frame(
  Parameter = c("Theta_1", "Theta_2", "Theta_3", "Theta_4", "Theta_5", "Theta_bias"),
  Model_1   = c(model1_Thetahat[1], model1_Thetahat[2], model1_Thetahat[3], model1_Thetahat[4], model1_Thetahat[5], model1_Thetahat[6]),
  Model_2   = c(model2_Thetahat[1], model2_Thetahat[2], model2_Thetahat[3], NA, NA, model2_Thetahat[4]),
  Model_3   = c(model3_Thetahat[1], model3_Thetahat[2], NA, NA, NA, model3_Thetahat[3]),
  Model_4   = c(model4_Thetahat[1], model4_Thetahat[2], model4_Thetahat[3], NA, NA, model4_Thetahat[4]),
  Model_5   = c(model5_Thetahat[1], model5_Thetahat[2], model5_Thetahat[3], model5_Thetahat[4], NA, model5_Thetahat[5])
)

# Apply rounding ONLY to the numeric columns (columns 2 through 6)
theta_table[, 2:6] <- round(theta_table[, 2:6], 5)

cat("\n=== TASK 2.1: PARAMETER ESTIMATES (THETA HAT) TABLE ===\n")
print(theta_table)

# ==============================================================================
# TASK 2.2: RESIDUAL SUM OF SQUARES (RSS)
# ==============================================================================

# Function to compute RSS for a given design matrix X and target y
compute_rss <- function(X_mat, y_vec) {
  theta_hat <- solve(t(X_mat) %*% X_mat) %*% t(X_mat) %*% y_vec
  y_hat     <- X_mat %*% theta_hat
  residuals <- y_vec - y_hat
  return(sum(residuals^2))
}

# Compute RSS for each model individually
rss_m1 <- compute_rss(XData_model1, y)
rss_m2 <- compute_rss(XData_model2, y)
rss_m3 <- compute_rss(XData_model3, y)
rss_m4 <- compute_rss(XData_model4, y)
rss_m5 <- compute_rss(XData_model5, y)

# Display Task 2.2 Output Table
task2_2_table <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  RSS   = c(rss_m1, rss_m2, rss_m3, rss_m4, rss_m5)
)

cat("=== TASK 2.2: RESIDUAL SUM OF SQUARES (RSS) ===\n")
print(task2_2_table)

# ==============================================================================
# TASK 2.3: ESTIMATED VARIANCE AND LOG-LIKELIHOOD
# ==============================================================================

# Function to compute Variance and Log-Likelihood
compute_log_likelihood <- function(X_mat, y_vec) {
  RSS <- compute_rss(X_mat, y_vec)
  
  # Estimated variance
  sigma_sq <- RSS / (n - 1)
  
  # Log-likelihood formula for Gaussian errors
  log_lik <- - (n / 2) * log(2 * pi) - (n / 2) * log(sigma_sq) - (RSS / (2 * sigma_sq))
  
  return(c(Sigma_Sq = sigma_sq, LogLikelihood = log_lik))
}

loglik_m1 <- compute_log_likelihood(XData_model1, y)
loglik_m2 <- compute_log_likelihood(XData_model2, y)
loglik_m3 <- compute_log_likelihood(XData_model3, y)
loglik_m4 <- compute_log_likelihood(XData_model4, y)
loglik_m5 <- compute_log_likelihood(XData_model5, y)

# Display Task 2.3 Output Table
task2_3_table <- data.frame(
  Model         = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  Sigma_Squared = c(loglik_m1[1], loglik_m2[1], loglik_m3[1], loglik_m4[1], loglik_m5[1]),
  Log_Likelihood = c(loglik_m1[2], loglik_m2[2], loglik_m3[2], loglik_m4[2], loglik_m5[2])
)

cat("=== TASK 2.3: ESTIMATED VARIANCE & LOG-LIKELIHOOD ===\n")
print(task2_3_table)

# ==============================================================================
# TASK 2.4: AIC AND BIC MODEL SELECTION METRICS
# ==============================================================================

# Function to compute AIC and BIC
compute_aic_bic <- function(X_mat, y_vec) {
  log_lik_res <- compute_log_likelihood(X_mat, y_vec)
  log_lik     <- log_lik_res["LogLikelihood"]
  k           <- ncol(X_mat) # Number of parameters including bias
  
  AIC <- 2 * k - 2 * log_lik
  BIC <- k * log(n) - 2 * log_lik
  
  return(c(Num_Params = k, AIC = AIC, BIC = BIC))
}

aic_bic_m1 <- compute_aic_bic(XData_model1, y)
aic_bic_m2 <- compute_aic_bic(XData_model2, y)
aic_bic_m3 <- compute_aic_bic(XData_model3, y)
aic_bic_m4 <- compute_aic_bic(XData_model4, y)
aic_bic_m5 <- compute_aic_bic(XData_model5, y)

# Display Task 2.4 Output Table
task2_4_table <- data.frame(
  Model      = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  Num_Params = c(aic_bic_m1[1], aic_bic_m2[1], aic_bic_m3[1], aic_bic_m4[1], aic_bic_m5[1]),
  AIC        = c(aic_bic_m1["AIC.LogLikelihood"], aic_bic_m2["AIC.LogLikelihood"], aic_bic_m3["AIC.LogLikelihood"], aic_bic_m4["AIC.LogLikelihood"], aic_bic_m5["AIC.LogLikelihood"]),
  BIC        = c(aic_bic_m1["BIC.LogLikelihood"], aic_bic_m2["BIC.LogLikelihood"], aic_bic_m3["BIC.LogLikelihood"], aic_bic_m4["BIC.LogLikelihood"], aic_bic_m5["BIC.LogLikelihood"])
)

cat("=== TASK 2.4: AIC AND BIC RESULTS ===\n")
print(task2_4_table)

# ==============================================================================
# TASK 2.5: RESIDUAL DIAGNOSTICS (Q-Q PLOTS FOR MODELS 1 TO 5)
# ==============================================================================

# 1. Helper function to extract residuals for any design matrix X
get_residuals <- function(X_mat, y_vec) {
  theta_hat <- solve(t(X_mat) %*% X_mat) %*% t(X_mat) %*% y_vec
  return(y_vec - (X_mat %*% theta_hat))
}

res_m1 <- get_residuals(XData_model1, y)
res_m2 <- get_residuals(XData_model2, y)
res_m3 <- get_residuals(XData_model3, y)
res_m4 <- get_residuals(XData_model4, y)
res_m5 <- get_residuals(XData_model5, y)

# 2. Plot 5 individual Q-Q plots in a 2x3 layout
dev.new(width = 10, height = 10)
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

qqnorm(res_m1, main = "Q-Q Plot: Model 1", col = "blue", pch = 16, cex = 0.5)
qqline(res_m1, col = "red", lwd = 2)

qqnorm(res_m2, main = "Q-Q Plot: Model 2", col = "blue", pch = 16, cex = 0.5)
qqline(res_m2, col = "red", lwd = 2)

qqnorm(res_m3, main = "Q-Q Plot: Model 3", col = "blue", pch = 16, cex = 0.5)
qqline(res_m3, col = "red", lwd = 2)

qqnorm(res_m4, main = "Q-Q Plot: Model 4", col = "blue", pch = 16, cex = 0.5)
qqline(res_m4, col = "red", lwd = 2)

qqnorm(res_m5, main = "Q-Q Plot: Model 5", col = "blue", pch = 16, cex = 0.5)
qqline(res_m5, col = "red", lwd = 2)

par(mfrow = c(1, 1)) # Reset plot window

# Residual histogram

# Arrange five histograms in a 2x3 layout
dev.new(width = 10, height = 10)
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

# Model 1
hist(res_m1, probability = TRUE,
     main = "Residual Distribution: Model 1",
     xlab = "Residuals", col = "lightblue", border = "white")
curve(dnorm(x, mean = mean(res_m1), sd = sd(res_m1)),
      add = TRUE, col = "red", lwd = 2)

# Model 2
hist(res_m2, probability = TRUE,
     main = "Residual Distribution: Model 2",
     xlab = "Residuals", col = "lightblue", border = "white")
curve(dnorm(x, mean = mean(res_m2), sd = sd(res_m2)),
      add = TRUE, col = "red", lwd = 2)

# Model 3
hist(res_m3, probability = TRUE,
     main = "Residual Distribution: Model 3",
     xlab = "Residuals", col = "lightblue", border = "white")
curve(dnorm(x, mean = mean(res_m3), sd = sd(res_m3)),
      add = TRUE, col = "red", lwd = 2)

# Model 4
hist(res_m4, probability = TRUE,
     main = "Residual Distribution: Model 4",
     xlab = "Residuals", col = "lightblue", border = "white")
curve(dnorm(x, mean = mean(res_m4), sd = sd(res_m4)),
      add = TRUE, col = "red", lwd = 2)

# Model 5
hist(res_m5, probability = TRUE,
     main = "Residual Distribution: Model 5",
     xlab = "Residuals", col = "lightblue", border = "white")
curve(dnorm(x, mean = mean(res_m5), sd = sd(res_m5)),
      add = TRUE, col = "red", lwd = 2)

# Reset plotting layout
par(mfrow = c(1, 1))


get_residual_stats <- function(residuals) {
  
  n <- length(residuals)
  
  m <- mean(residuals)
  s <- sd(residuals)
  
  skewness <- mean((residuals - m)^3) / s^3
  
  excess_kurtosis <- mean((residuals - m)^4) / s^4 - 3
  
  return(c(
    Mean = m,
    SD = s,
    Skewness = skewness,
    Excess_Kurtosis = excess_kurtosis
  ))
}

# Create comparison table
task2_5_table <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3",
            "Model 4", "Model 5"),
  
  rbind(
    get_residual_stats(res_m1),
    get_residual_stats(res_m2),
    get_residual_stats(res_m3),
    get_residual_stats(res_m4),
    get_residual_stats(res_m5)
  )
)

cat("=== TASK 2.5: RESIDUAL SUMMARY STATISTICS ===\n")
print(task2_5_table)
# ==============================================================================
# TASK 2.6: DYNAMIC AUTOMATED MODEL SELECTION
# ==============================================================================
selection_table <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  AIC   = c(task2_4_table$AIC),
  BIC   = c(task2_4_table$BIC),
  RSS   = c(task2_2_table$RSS)
)

cat("=== TASK 2.6: CANDIDATE MODEL SELECTION SUMMARY ===\n")
print(selection_table)

# 2. Dynamically identify winning model index based on lowest AIC
best_model_idx <- which.min(selection_table$AIC)
cat("\nWinning Model based on lowest AIC score: Model", best_model_idx, "\n")

# 3. Dynamic Assignment of Winning Design Matrix and Parameter Estimates
models_design_list <- list(XData_model1, XData_model2, XData_model3, XData_model4, XData_model5)
models_theta_list  <- list(model1_Thetahat, model2_Thetahat, model3_Thetahat, model4_Thetahat, model5_Thetahat)

winning_X     <- models_design_list[[best_model_idx]]
winning_theta <- models_theta_list[[best_model_idx]]




# ==============================================================================
# TASK 2.7: DYNAMIC TRAIN/TEST SPLIT (70/30)
#           95% PREDICTION INTERVALS AND OUTPUT DISTRIBUTION
# ==============================================================================

# 1. RANDOM 70% TRAINING / 30% TESTING SPLIT
n <- length(y)
set.seed(42)  # Ensure reproducibility

train_indices <- sample(1:n, size = floor(0.70 * n))

X_train <- winning_X[train_indices, , drop = FALSE]
y_train <- y[train_indices]

X_test <- winning_X[-train_indices, , drop = FALSE]
y_test <- y[-train_indices]

cat("=== TRAINING AND TESTING SPLIT ===\n")
cat("Total observations:", n, "\n")
cat("Training observations:", length(y_train), "\n")
cat("Testing observations:", length(y_test), "\n")

# 2. ESTIMATE MODEL PARAMETERS USING TRAINING DATA
theta_train <- solve(t(X_train) %*% X_train) %*% t(X_train) %*% y_train

cat("\n========================================\n")
cat("        TRAINING MODEL PARAMETERS\n")
cat("========================================\n")
for (i in seq_along(theta_train)) {
  cat(sprintf("Theta %-10d : %12.6f\n", i, theta_train[i, 1]))
}
cat("========================================\n")

# 3. CALCULATE TRAINING RESIDUALS AND ERROR VARIANCE
train_res <- y_train - X_train %*% theta_train
n_train <- length(y_train)
k <- ncol(X_train)

# Estimated error variance using training residuals
sigma_sq_t <- sum(train_res^2) / (n_train - k)

cat("\n=== ESTIMATED TRAINING ERROR VARIANCE ===\n")
cat("Estimated Variance:", sigma_sq_t, "\n")
cat("Estimated Standard Deviation:", sqrt(sigma_sq_t), "\n")

# 4. PREDICT POWER CONSUMPTION ON TESTING DATA
y_test_pred <- X_test %*% theta_train
test_res <- y_test - y_test_pred

cat("\n=== TESTING DATA PREDICTIONS ===\n")
prediction_results <- data.frame(
  Actual_Power = as.vector(y_test),
  Predicted_Power = as.vector(y_test_pred),
  Residual = as.vector(test_res)
)
print(head(prediction_results, 10), digits = 6)
cat("\nTotal Testing Predictions:", length(y_test_pred), "\n")

# 5. CALCULATE DYNAMIC 95% PREDICTION INTERVALS
prediction_variance <- sigma_sq_t * (
  1 + diag(
    X_test %*% solve(t(X_train) %*% X_train) %*% t(X_test)
  )
)
se_pred <- sqrt(prediction_variance)

# Critical t-value
t_critical <- qt(0.975, df = n_train - k)

# Lower and upper 95% prediction interval
pi_lower <- as.vector(y_test_pred) - t_critical * se_pred
pi_upper <- as.vector(y_test_pred) + t_critical * se_pred

# 6. CREATE TEST PREDICTION RESULTS TABLE
prediction_results <- data.frame(
  Actual_Power = as.vector(y_test),
  Predicted_Power = as.vector(y_test_pred),
  Lower_95_PI = pi_lower,
  Upper_95_PI = pi_upper,
  Test_Residual = as.vector(test_res)
)

cat("\n=== TASK 2.7: TEST PREDICTION RESULTS ===\n")
print(head(prediction_results, 10))

# 7. PLOT ACTUAL VS PREDICTED VALUES WITH 95% PREDICTION INTERVAL
invisible(dev.new(width = 12, height = 7))
plot_range <- 1:min(150, length(y_test))

plot(plot_range, y_test_pred[plot_range], type = "n", ylim = range(c(y_test[plot_range], pi_lower[plot_range], pi_upper[plot_range])), xlab = "Test Sample Index", ylab = "Power Consumption (W)", main = paste("Task 2.7: Model", best_model_idx, "Actual vs Predicted Power with 95% Prediction Intervals"))

arrows(x0 = plot_range, y0 = pi_lower[plot_range], 
       x1 = plot_range, y1 = pi_upper[plot_range], 
       angle = 90, code = 3, length = 0.035, col = "red")
lines(plot_range, y_test_pred[plot_range], col = "blue", lwd = 2)
points(plot_range, y_test[plot_range], col = "green", pch = 16, cex = 0.8)

legend("topleft", legend = c("Actual Test Values", "Predicted Values", "95% Prediction Intervals"), col = c("green", "blue", "red"), pch = c(16, NA, NA), lty = c(NA, 1, 1), lwd = c(NA, 2, 1.5), bty = "n")

# 8. PREDICTOR VS OUTPUT ERROR BAR PLOT (GGPLOT2)
library(ggplot2)

plot_range <- 1:min(150, length(y_test))
df_network <- data.frame(Predictor = "Network Traffic (X4)", X_Value = as.vector(X_test[plot_range, 1]), Actual_Y = as.vector(y_test[plot_range]), Pred_Y = as.vector(y_test_pred[plot_range]), Lower_PI = pi_lower[plot_range], Upper_PI = pi_upper[plot_range])
df_cpu <- data.frame(Predictor = "CPU Utilization (X1^3)", X_Value = as.vector(X_test[plot_range, 2]), Actual_Y = as.vector(y_test[plot_range]), Pred_Y = as.vector(y_test_pred[plot_range]), Lower_PI = pi_lower[plot_range], Upper_PI = pi_upper[plot_range])
df_disk <- data.frame(Predictor = "Disk I/O (X3^4)", X_Value = as.vector(X_test[plot_range, 3]), Actual_Y = as.vector(y_test[plot_range]), Pred_Y = as.vector(y_test_pred[plot_range]), Lower_PI = pi_lower[plot_range], Upper_PI = pi_upper[plot_range])
df_plot <- rbind(df_network, df_cpu, df_disk)
y_limits <- quantile(c(y_test, y_test_pred), probs = c(0.05, 0.95))
p <- ggplot(df_plot, aes(x = X_Value)) +
  geom_point(aes(y = Actual_Y), color = "green", shape = 4, size = 1.5) +
  geom_point(aes(y = Pred_Y), color = "blue", size = 1.5) +  geom_errorbar(aes(ymin = Lower_PI, ymax = Upper_PI), color = "red", width = 0.15, linewidth = 1) +
  facet_wrap(~ Predictor, scales = "free_x") +
  coord_cartesian(ylim = y_limits) +
  labs(title = "Model 2: Predictor vs Power Consumption", subtitle = "Actual and Predicted Output with 95% Prediction Intervals", x = "Transformed Predictor Values", y = "Power Consumption (W)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 10, hjust = 0.5), strip.text = element_text(size = 11, face = "bold"))
dev.new(width = 12, height = 8)
print(p)


# 9. CALCULATE 95% CONFIDENCE INTERVAL FOR TRAINING OUTPUT MEAN
# Compute mean_y explicitly before using it
mean_y <- mean(y_train) 
se_y <- sd(y_train) / sqrt(length(y_train))
ci_low <- mean_y - qt(0.975, df = length(y_train) - 1) * se_y
ci_upp <- mean_y + qt(0.975, df = length(y_train) - 1) * se_y

cat("\n=== TRAINING OUTPUT SIGNAL SUMMARY ===\n")
cat("Mean Power Consumption:", mean_y, "\n")
cat("Standard Deviation:", sd(y_train), "\n")
cat("Standard Error:", se_y, "\n")
cat("95% Confidence Interval Lower Bound:", ci_low, "\n")
cat("95% Confidence Interval Upper Bound:", ci_upp, "\n")

invisible(dev.new(width = 9, height = 6)) # Suppresses NULL output
dens <- density(y_train)
plot(dens, main = "Distribution of Output Signal Training Data", xlab = "Power Consumption (W)", ylab = "Density", col = "darkgreen", lwd = 2)

abline(v = mean_y, col = "grey40", lty = 2, lwd = 2)
abline(v = ci_low, col = "red", lty = 2, lwd = 1.5)
abline(v = ci_upp, col = "red", lty = 2, lwd = 1.5)

legend("topright", legend = c("Training Mean", "95% CI Bounds"), col = c("grey40", "red"), lty = 2, lwd = c(2, 1.5), bty = "n")

# 10. TESTING PERFORMANCE METRICS
test_RSS <- sum(test_res^2)
test_RMSE <- sqrt(mean(test_res^2))
test_MAE <- mean(abs(test_res))
coverage <- mean(y_test >= pi_lower & y_test <= pi_upper) * 100

task2_7_metrics <- data.frame(
  Metric = c("Test RSS", "Test RMSE", "Test MAE", "Prediction Interval Coverage (%)"),
  Value = c(test_RSS, test_RMSE, test_MAE, coverage)
)

cat("\n=== TASK 2.7: TESTING PERFORMANCE SUMMARY ===\n")
print(task2_7_metrics)

# ==============================================================================
# END OF TASK 2.7
# ==============================================================================


# ==============================================================================
# TASK 3: DYNAMIC REJECTION APPROXIMATE BAYESIAN COMPUTATION (ABC)
# ==============================================================================

# 1. Dynamically identify top 2 parameters with largest absolute magnitude (excluding bias)
num_params_nobias <- length(winning_theta) - 1
abs_theta_nobias  <- abs(winning_theta[1:num_params_nobias])

# Rank parameter positions by magnitude
top_param_indices <- order(abs_theta_nobias, decreasing = TRUE)[1:2]

idx_p1 <- top_param_indices[1]
idx_p2 <- top_param_indices[2]

theta_p1_hat <- winning_theta[idx_p1]
theta_p2_hat <- winning_theta[idx_p2]

cat("\n=== TASK 3: DYNAMICALLY SELECTED TOP PARAMETERS ===\n")
cat("Winning Model Index   :", best_model_idx, "\n")
cat("Top Parameter 1 Position [", idx_p1, "] Value:", theta_p1_hat, "\n")
cat("Top Parameter 2 Position [", idx_p2, "] Value:", theta_p2_hat, "\n")

# 2. Setup Monte Carlo Simulation Arrays
num_simulations <- 50000

# Draw Uniform Priors centered around selected OLS estimates (+/- 50% spread)
set.seed(42)
p1_prior <- runif(num_simulations, min = theta_p1_hat * 0.5, max = theta_p1_hat * 1.5)
p2_prior <- runif(num_simulations, min = theta_p2_hat * 0.5, max = theta_p2_hat * 1.5)

rss_simulated <- numeric(num_simulations)

# 3. Dynamic Vectorized Error Calculation Loop
for (i in 1:num_simulations) {
  theta_sim         <- winning_theta
  theta_sim[idx_p1] <- p1_prior[i]
  theta_sim[idx_p2] <- p2_prior[i]
  
  y_sim            <- winning_X %*% theta_sim
  rss_simulated[i] <- sum((y - y_sim)^2)
}

# 4. Dynamic Quantile Filtering (Retain Top 1% Best Performing Candidates)
acceptance_rate <- 0.01  # Retain top 1% (500 accepted samples)
tolerance_eps   <- quantile(rss_simulated, probs = acceptance_rate)

accepted_indices <- which(rss_simulated <= tolerance_eps)
accepted_p1      <- p1_prior[accepted_indices]
accepted_p2      <- p2_prior[accepted_indices]

cat("\n=== TASK 3: DYNAMIC ABC ACCEPTANCE DIAGNOSTICS ===\n")
cat("Total Simulations Run :", num_simulations, "\n")
cat("Tolerance Threshold   :", round(tolerance_eps, 2), "\n")
cat("Accepted Samples Count:", length(accepted_p1), "\n")

# ------------------------------------------------------------------------------
# 5. Visualizations: Dynamic Joint (2D) and Marginal (1D) Posteriors
# ------------------------------------------------------------------------------
dev.new(width = 10, height = 10)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

# A. Dynamic Joint Posterior Distribution Plot
plot(accepted_p1, accepted_p2, col = rgb(0, 0.5, 0, 0.4), pch = 16,
     xlab = substitute(theta[pos], list(pos = idx_p1)), 
     ylab = substitute(theta[pos], list(pos = idx_p2)),
     main = paste("Task 3: Model", best_model_idx, "Joint Posterior"))
points(theta_p1_hat, theta_p2_hat, col = "red", pch = 19, cex = 1.5) # OLS Point Estimate
legend("topright", legend = "OLS Estimate", col = "red", pch = 19)

# B. Marginal Posterior Histogram for Top Parameter 1
hist(accepted_p1, col = "lightblue", freq = FALSE,
     xlab = substitute(theta[pos], list(pos = idx_p1)), 
     main = substitute("Marginal Posterior " * theta[pos], list(pos = idx_p1)))
abline(v = theta_p1_hat, col = "red", lwd = 2)

# C. Marginal Posterior Histogram for Top Parameter 2
hist(accepted_p2, col = "lightgreen", freq = FALSE,
     xlab = substitute(theta[pos], list(pos = idx_p2)), 
     main = substitute("Marginal Posterior " * theta[pos], list(pos = idx_p2)))
abline(v = theta_p2_hat, col = "red", lwd = 2)

par(mfrow = c(1, 1)) # Reset graphics layout