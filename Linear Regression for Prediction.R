# Q1
# 加载必要的库
library(tidyverse)  # 用于数据操作
library(caret)      # 用于数据分区

# 设置随机种子，保证实验的可重复性
set.seed(1)

# 运行 100 次模型训练和测试，存储每次计算得到的 RMSE均方根误差Root Mean Squared Error
rmse <- replicate(100, {
  # 按照 y 的值进行数据分区，确保训练集和测试集分布相似
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  
  # 生成训练集，排除测试集中的索引
  train_set <- dat %>% slice(-test_index)
  
  # 生成测试集，仅包含分配到测试集的索引
  test_set <- dat %>% slice(test_index)
  
  # 在训练集上拟合线性回归模型 y ~ x
  fit <- lm(y ~ x, data = train_set)
  
  # 在测试集上进行预测
  y_hat <- predict(fit, newdata = test_set)
  
  # 计算 RMSE（均方根误差）：衡量预测值与真实值之间的平均偏差
  sqrt(mean((y_hat - test_set$y)^2))
})

# 计算 100 个模型 RMSE 的均值
mean(rmse)

# 计算 RMSE 的标准差
sd(rmse)





# Q2
# Set seed for reproducibility
set.seed(1)

# Define different dataset sizes
n <- c(100, 500, 1000, 5000, 10000)

# Apply the function to each dataset size using sapply()
res <- sapply(n, function(n) {
  
  # Define covariance matrix (controls the correlation between x and y)
  Sigma <- 9 * matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  
  # Generate dataset with 'n' observations from a bivariate normal distribution
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))  # Convert to dataframe and name columns
  
  # Run 100 simulations to compute RMSE
  rmse <- replicate(100, {
    
    # Split the data into training (50%) and testing (50%) sets
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    
    # Define training set (excluding test indices)
    train_set <- dat %>% slice(-test_index)
    
    # Define testing set (including test indices)
    test_set <- dat %>% slice(test_index)
    
    # Train a linear regression model using training data
    fit <- lm(y ~ x, data = train_set)
    
    # Make predictions on the test set
    y_hat <- predict(fit, newdata = test_set)
    
    # Compute Root Mean Squared Error (RMSE) for this model
    sqrt(mean((y_hat - test_set$y)^2))
  })
  
  # Return mean RMSE and standard deviation (SD) of RMSE across 100 iterations
  c(avg = mean(rmse), sd = sd(rmse))
})

# Display the final results stored in 'res'
res




# Q4

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})

mean(rmse)
sd(rmse)


# Q7
set.seed(1)

# Split the dataset into training (50%) and testing (50%) sets
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)  
# `createDataPartition()` creates indices for a stratified split based on `y`

# Define training set (all rows except those in `test_index`)
train_set <- dat %>% slice(-test_index)  

# Define testing set (only rows included in `test_index`)
test_set <- dat %>% slice(test_index)  

# ----------------- Model 1: Using only x_1 as predictor -----------------

# Train a linear regression model: predict y using x_1
fit <- lm(y ~ x_1, data = train_set)  

# Make predictions on the test set
y_hat <- predict(fit, newdata = test_set)  

# Compute RMSE (Root Mean Squared Error) to measure model accuracy
sqrt(mean((y_hat - test_set$y)^2))  

# ----------------- Model 2: Using only x_2 as predictor -----------------

# Train a linear regression model: predict y using x_2
fit <- lm(y ~ x_2, data = train_set)  

# Make predictions on the test set
y_hat <- predict(fit, newdata = test_set)  

# Compute RMSE for model using only x_2
sqrt(mean((y_hat - test_set$y)^2))  

# ----------------- Model 3: Using both x_1 and x_2 as predictors -----------------

# Train a linear regression model: predict y using both x_1 and x_2
fit <- lm(y ~ x_1 + x_2, data = train_set)  

# Make predictions on the test set
y_hat <- predict(fit, newdata = test_set)  

# Compute RMSE for model using both predictors
sqrt(mean((y_hat - test_set$y)^2))  


