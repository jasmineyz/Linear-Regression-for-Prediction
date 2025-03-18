# R Project: Evaluating RMSE of Linear Models under Varying Conditions

# Load required libraries
library(tidyverse)
library(caret)

# --------------------
# Generate synthetic data with specified covariance structure
# --------------------
set.seed(1)
n <- 100
Sigma <- 9 * matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# --------------------
# Function to calculate RMSE for linear models through repeated partitioning
# --------------------
calc_rmse <- function(dat, n_reps = 100) {
  replicate(n_reps, {
    test_index <- createDataPartition(dat$y, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)

    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)

    sqrt(mean((y_hat - test_set$y)^2))
  })
}

# --------------------
# Evaluate mean and standard deviation of RMSE (n = 100)
# --------------------
set.seed(1)
rmse_vals <- calc_rmse(dat)
mean(rmse_vals)
sd(rmse_vals)

# --------------------
# Analyze RMSE stability as dataset size increases
# --------------------
# Observation: RMSE variability decreases significantly with increasing dataset size, indicating more stable and reliable predictions.
set.seed(1)
n_sizes <- c(100, 500, 1000, 5000, 10000)

rmse_summary <- sapply(n_sizes, function(n) {
  dat_large <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))

  rmse_vals <- calc_rmse(dat_large)
  c(mean = mean(rmse_vals), sd = sd(rmse_vals))
})

rmse_summary

# --------------------
# Evaluate effect of higher correlation between predictors and outcomes
# --------------------
# Observation: Increasing correlation between predictors and outcomes significantly reduces RMSE, demonstrating improved predictive accuracy.
set.seed(1)
Sigma_high_corr <- 9 * matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat_high_corr <- MASS::mvrnorm(n, c(69, 69), Sigma_high_corr) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
rmse_high_corr <- calc_rmse(dat_high_corr)
mean(rmse_high_corr)
sd(rmse_high_corr)

# --------------------
# Evaluate RMSE with two independent predictors
# --------------------
# Observation: Including both independent predictors significantly reduces RMSE compared to using each predictor individually.
set.seed(1)
Sigma_two_preds <- matrix(c(1.0, 0.75, 0.75,
                            0.75, 1.0, 0.25,
                            0.75, 0.25, 1.0), 3, 3)

dat_two_preds <- MASS::mvrnorm(n, c(0, 0, 0), Sigma_two_preds) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat_two_preds$y, p = 0.5, list = FALSE)
train_set <- dat_two_preds %>% slice(-test_index)
test_set <- dat_two_preds %>% slice(test_index)

rmse_x1 <- sqrt(mean((predict(lm(y ~ x_1, train_set), test_set) - test_set$y)^2))
rmse_x2 <- sqrt(mean((predict(lm(y ~ x_2, train_set), test_set) - test_set$y)^2))
rmse_both <- sqrt(mean((predict(lm(y ~ x_1 + x_2, train_set), test_set) - test_set$y)^2))

rmse_results <- data.frame(Model = c("x_1", "x_2", "x_1 + x_2"),
                           RMSE = c(rmse_x1, rmse_x2, rmse_both))
rmse_results

# --------------------
# Evaluate RMSE with highly correlated predictors
# --------------------
# Observation: Adding highly correlated predictors does not significantly improve RMSE, indicating potential redundancy and multicollinearity issues.
set.seed(1)
Sigma_corr_preds <- matrix(c(1.0, 0.75, 0.75,
                             0.75, 1.0, 0.95,
                             0.75, 0.95, 1.0), 3, 3)

dat_corr_preds <- MASS::mvrnorm(n, c(0, 0, 0), Sigma_corr_preds) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat_corr_preds$y, p = 0.5, list = FALSE)
train_set <- dat_corr_preds %>% slice(-test_index)
test_set <- dat_corr_preds %>% slice(test_index)

rmse_x1_corr <- sqrt(mean((predict(lm(y ~ x_1, train_set), test_set) - test_set$y)^2))
rmse_x2_corr <- sqrt(mean((predict(lm(y ~ x_2, train_set), test_set) - test_set$y)^2))
rmse_both_corr <- sqrt(mean((predict(lm(y ~ x_1 + x_2, train_set), test_set) - test_set$y)^2))

rmse_results_corr <- data.frame(Model = c("x_1", "x_2", "x_1 + x_2"),
                                RMSE = c(rmse_x1_corr, rmse_x2_corr, rmse_both_corr))
rmse_results_corr
