# Delete everything in workspace

rm(list = ls())

# Libraries --------------------------------------------------------------------

library(tidyverse)
library(caret)
library(patchwork)
library(corrplot)
library(pROC)

# Modified OHLC data -----------------------------------------------------------

data <- read_csv("/data/daily_candles_btc.csv")

# Dependent variable -----------------------------------------------------------

data$final_day <- NA
n_days <- 3

data$fo <- lead(data$open, n_days)
data$fc <- lead(data$close, n_days)
data$fh <- lead(data$high, n_days)
data$fl <- lead(data$low, n_days)

data$final_day[data$open < data$fc] <- "Yes"
data$final_day[data$open >= data$fc] <- "No"

summary(as.factor(data$final_day))

# Independent variable ---------------------------------------------------------

data$returns <- (data$close - data$open)/data$open

# future returns
data$fo1 <- lead(data$open, 1)
data$fc1 <- lead(data$close, 1)
data$fh1 <- lead(data$high, 1)
data$fl1 <- lead(data$low, 1)

data$fo2 <- lead(data$open, 2)
data$fc2 <- lead(data$close, 2)
data$fh2 <- lead(data$high, 2)
data$fl2 <- lead(data$low, 2)

data$freturns_1 <- (data$fc1 - data$open)/data$open
data$freturns_2 <- (data$fc2 - data$open)/data$open

data$cth <- (data$high - data$open) / data$open
data$ctl <- (data$low - data$open) / data$open

data$cth1 <- (data$fh1 - data$fo1) / data$fo1
data$ctl1 <- (data$fl1 - data$fo1) / data$fo1

data$cth2 <- (data$fh2 - data$fo2) / data$fo2
data$ctl2 <- (data$fl2 - data$fo2) / data$fo2

# Final cleaning ---------------------------------------------------------------

idx_b <- (nrow(data)-n_days+1) : nrow(data) 
data <- data[-idx_b,]

data_riskcalc <- data %>% dplyr::select(c("date", "final_day","open","fo", "fh", "fl", "fc"))

data <- data %>% dplyr::select(c("final_day",
                                 "returns",
                                 "cth",
                                 "ctl",
                                 "cth1",
                                 "ctl1",
                                 "cth2",
                                 "ctl2",
                                 "freturns_1",
                                 "freturns_2"
))

which(!complete.cases(data))

data$final_day <- as.factor(data$final_day)
summary(data$final_day)

# Train/test split -------------------------------------------------------------

n_rows <- nrow(data)
all_idx <- 1:n_rows
num_folds <- 100
folds <- cut(all_idx, breaks = num_folds, labels = FALSE)

train_folds <- 1:80
test_folds <- 81:100

# Find indices for the training and testing sets
train_indices <- which(folds %in% train_folds)
test_indices <- which(folds %in% test_folds)

train_data <- data[train_indices,]
test_data <- data[test_indices,]

# EDA --------------------------------------------------------------------------

# train_data %>% ggplot(aes(y = returns, x = final_day, fill = final_day)) +
#   geom_boxplot(width=0.1, color="black",  outlier.color = "black")  +
#   geom_violin(alpha=0.2) +
#   theme(legend.position="none")
# 
# train_data %>% ggplot(aes(y = freturns_1, x = final_day, fill = final_day)) +
#   geom_boxplot(width=0.1, color="black",  outlier.color = "black")  +
#   geom_violin(alpha=0.2) +
#   theme(legend.position="none")
# 
# train_data %>% ggplot(aes(y = freturns_2, x = final_day, fill = final_day)) +
#   geom_boxplot(width=0.1, color="black",  outlier.color = "black")  +
#   geom_violin(alpha=0.2) +
#   theme(legend.position="none")
# 
# plot_cth <- train_data %>% 
#   ggplot(aes(y = cth, x = final_day, fill = final_day)) +
#   geom_boxplot(width=0.1, color="black", outlier.color = "black") +
#   geom_violin(alpha=0.2) +
#   theme(legend.position="none") +
#   labs(title = "CTH vs Final Day Outcome", x = "Final Day", y = "CTH")
# 
# plot_ctl <- train_data %>% 
#   ggplot(aes(y = ctl, x = final_day, fill = final_day)) +
#   geom_boxplot(width=0.1, color="black", outlier.color = "black") +
#   geom_violin(alpha=0.2) +
#   theme(legend.position="none") +
#   labs(title = "CTL vs Final Day Outcome", x = "Final Day", y = "CTL")
# 
# # Combine both plots using patchwork
# combined_plot <- plot_cth + plot_ctl
# combined_plot
# 
# plot_cth1 <- train_data %>% 
#   ggplot(aes(y = cth1, x = final_day, fill = final_day)) +
#   geom_boxplot(width=0.1, color="black", outlier.color = "black") +
#   geom_violin(alpha=0.2) +
#   theme(legend.position="none") +
#   labs(title = "CTH1 vs Final Day Outcome", x = "Final Day", y = "CTH1")
# 
# plot_ctl1 <- train_data %>% 
#   ggplot(aes(y = ctl1, x = final_day, fill = final_day)) +
#   geom_boxplot(width=0.1, color="black", outlier.color = "black") +
#   geom_violin(alpha=0.2) +
#   theme(legend.position="none") +
#   labs(title = "CTL1 vs Final Day Outcome", x = "Final Day", y = "CTL1")
# 
# plot_cth2 <- train_data %>% 
#   ggplot(aes(y = cth2, x = final_day, fill = final_day)) +
#   geom_boxplot(width=0.1, color="black", outlier.color = "black") +
#   geom_violin(alpha=0.2) +
#   theme(legend.position="none") +
#   labs(title = "CTH2 vs Final Day Outcome", x = "Final Day", y = "CTH2")
# 
# plot_ctl2 <- train_data %>% 
#   ggplot(aes(y = ctl2, x = final_day, fill = final_day)) +
#   geom_boxplot(width=0.1, color="black", outlier.color = "black") +
#   geom_violin(alpha=0.2) +
#   theme(legend.position="none") +
#   labs(title = "CTL2 vs Final Day Outcome", x = "Final Day", y = "CTL2")
# 
# # Combine both plots using patchwork
# combined_plot <- plot_cth1 + plot_ctl1 + plot_cth2 + plot_ctl2
# combined_plot
# 
# # Correlations of the numerical variables
# cors <- cor(train_data[,-1])
# corrplot(cors, method="color", type="upper",  tl.srt = 45)

# Standartization --------------------------------------------------------------

X.standardize <- function(X_train, X_test){
  
  # calculate the values for the mean and standard deviation
  X_mean <- apply(X_train, 2, mean)
  X_sd <- apply(X_train, 2, sd)
  
  # standardize the data
  X_train_std <- scale(X_train, center = X_mean, scale = X_sd)
  X_test_std <- scale(X_test, center = X_mean, scale = X_sd)
  
  return(list(X_train_std, X_test_std))
}

train_data_num <- train_data %>%
  select(where(is.numeric))
test_data_num <- test_data %>%
  select(where(is.numeric))

sdr <- X.standardize(train_data_num,test_data_num)

train_data_fac <- train_data %>%
  select(where(is.factor))
test_data_fac <- test_data %>%
  select(where(is.factor))

train_data <- cbind(sdr[[1]],train_data_fac)
test_data <- cbind(sdr[[2]],test_data_fac)

# Model training ---------------------------------------------------------------

set.seed(1234)
glmnet_control <- trainControl(method = "timeslice",
                               initialWindow = nrow(train_data)*0.60,
                               horizon = nrow(train_data)*0.1,
                               fixedWindow = FALSE,
                               allowParallel = TRUE,
                               classProbs = TRUE, 
                               summaryFunction = twoClassSummary, 
                               savePredictions = "all", 
                               verboseIter = TRUE)
# search = "grid")

weights_yes <- nrow(train_data)/(2*as.numeric(summary(train_data$final_day)[2]))
weights_no <- nrow(train_data)/(2*as.numeric(summary(train_data$final_day)[1]))

glmnet_weights <- ifelse(train_data$final_day == "Yes", weights_yes, weights_no)

grid_glmnet <- expand.grid(alpha = c(0.84,0.85,0.861,0.87,0.88),
                           lambda = c(0.08,0.09,0.1,0.11,0.12))

set.seed(1234)
glmnet_model <- caret::train(
  final_day ~ ., 
  data = train_data, 
  method = "glmnet",
  family = "binomial",
  metric = "ROC",
  trControl = glmnet_control,
  tuneGrid = grid_glmnet,
  # tuneLength = 10,
  weights = glmnet_weights
)

# Parameters -------------------------------------------------------------------

plot(glmnet_model)
plot(varImp(glmnet_model))

# Appication of the model on test dataset --------------------------------------

glmnet_pred <-
  predict(glmnet_model,
          newdata = test_data,
          type = "prob")

glmnet_roc <- roc(response = test_data$final_day,
                  predictor = glmnet_pred[, "Yes"])
glmnet_roc$auc

# saveRDS(glmnet_model, "glmnet_model_1day.rds")

# Threshold calculation for the stop loss --------------------------------------

risk <- data_riskcalc %>%
  filter(date < as.Date("2024-04-01"))

risk$drawdown <- ifelse(
  risk$final_day == "Yes", 
  (risk$fl - risk$open) / risk$open * 100,   # % difference from open to fl when second_day is "Yes"
  (risk$open - risk$fh) / risk$open * 100    # % difference from open to fh when second_day is "No"
)

drawdown <- risk %>% 
  filter(drawdown < 0)

summary(drawdown$drawdown)
lower_bound <- quantile(drawdown$drawdown, 0.05)

ggplot(drawdown, aes(x = drawdown)) +
  geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black", alpha = 0.7) +  # Histogram with bin width
  geom_vline(aes(xintercept = lower_bound), color = "red", linetype = "dashed", size = 1) +  # 5th percentile
  labs(
    title = "Distribution of Drawdowns",
    x = "Drawdown (%)",
    y = "Frequency"
  ) +
  theme_minimal() 

# Calculation of the benchmark ---------------------------------------------------------------------

options_prices <- read_csv("/data/option_premium.csv")
summary(options_prices$price_option)

benchmark <- data_riskcalc[test_indices,]
benchmark$final_day <- as.factor(benchmark$final_day)

benchmark <- merge(benchmark, options_prices, by = "date")

# Generate random predictions for put and call options

benchmark$prediction_put <- as.factor(ifelse(benchmark$open < benchmark$fo, "Yes", "No"))
benchmark$prediction_call <- as.factor(ifelse(benchmark$open >= benchmark$fo,"No", "Yes"))
# set.seed(1234)
# benchmark$prediction_put <- as.factor(sample(c("Yes", "No"), nrow(benchmark), replace = TRUE))
# benchmark$prediction_call <- as.factor(sample(c("Yes", "No"), nrow(benchmark), replace = TRUE))
benchmark$final_day <- as.factor(benchmark$final_day)

# PnL for puts
benchmark$pnl_puts <- ifelse(
  benchmark$prediction_put == "Yes" & benchmark$final_day == "Yes", 
  benchmark$price_option - benchmark$price_option * 0.2,  # Take value from price_option
  ifelse(
    benchmark$prediction_put == "Yes" & benchmark$final_day == "No", 
    benchmark$fc - benchmark$open + benchmark$price_option,  # Calculate fc - open + price_option
    0  # Set to 0 if prediction_put is No
  )
)

# PnL for calls
benchmark$pnl_call <- ifelse(
  benchmark$prediction_call == "No" & benchmark$final_day == "No", 
  benchmark$price_option - benchmark$price_option * 0.2,  # Take value from price_option
  ifelse(
    benchmark$prediction_call == "No" & benchmark$final_day == "Yes", 
    benchmark$open - benchmark$fc + benchmark$price_option,  # Calculate open - fc + price_option
    0  # Set to 0 if prediction_call is Yes
  )
)

benchmark$pnl <- benchmark$pnl_puts + benchmark$pnl_call

# Drawdown calculation
benchmark$drawdown_puts <- ifelse(
  benchmark$prediction_put == "Yes", 
  pmin(benchmark$fl - benchmark$open, 0),  # Calculate fl - open and ensure it’s <= 0
  0  # Set to 0 if prediction_put is No
)
benchmark$drawdown_call <- ifelse(
  benchmark$prediction_call == "No", 
  pmin(benchmark$open - benchmark$fh, 0),  # Calculate open - fh and ensure it’s <= 0
  0  # Set to 0 if prediction_call is Yes
)
benchmark$drawdown <- benchmark$drawdown_puts + benchmark$drawdown_call
benchmark$drawdown_treshold <- benchmark$open * -0.0532

benchmark$pnl <- ifelse(
  benchmark$drawdown < benchmark$drawdown_treshold, 
  benchmark$drawdown_treshold,  # Replace pnl with drawdown_treshold
  benchmark$pnl  # Keep existing pnl if condition not met
)
benchmark$cum_pnl <- cumsum(benchmark$pnl)

# Plot for pnl
pnl_plot <- ggplot(benchmark, aes(x = date, y = cum_pnl)) +
  geom_line(color = "black", size = 0.5) +
  labs(
    title = "Random Model: PnL Over Time",
    x = "Date",
    y = "PnL"
  ) +
  theme_minimal()

# Plot for drawdown with threshold line
drawdown_plot <- ggplot(benchmark, aes(x = date)) +
  geom_line(aes(y = drawdown), color = "darkorange", size = 0.5) +
  geom_line(aes(y = drawdown_treshold), linetype = "dotted", color = "red", size = 0.8) +  # Plot the threshold as a dotted line
  labs(
    title = "Random Model: Drawdown Over Time with Threshold",
    x = "Date",
    y = "Drawdown"
  ) +
  theme_minimal()

combined_plot <- pnl_plot / drawdown_plot
combined_plot

# Calculate metrics
total_pnl <- sum(benchmark$pnl)
total_trades <- nrow(benchmark)
taken_trades <- sum(benchmark$prediction_put == "Yes" | benchmark$prediction_call == "No")
skipped_trades <- total_trades - taken_trades
profitable_trades <- sum(benchmark$pnl > 0)
losing_trades <- sum(benchmark$pnl < 0)
max_profit <- max(benchmark$pnl)
max_loss <- min(benchmark$pnl)
average_profit <- mean(benchmark$pnl[benchmark$pnl > 0])
average_loss <- mean(benchmark$pnl[benchmark$pnl < 0])
sharpe_ratio <- mean(benchmark$pnl) / sd(benchmark$pnl)
total_profit <- sum(benchmark$pnl[benchmark$pnl > 0])
total_loss <- abs(sum(benchmark$pnl[benchmark$pnl < 0]))
profit_factor <- total_profit / total_loss
win_rate <- (profitable_trades / taken_trades) * 100

# Print the results
cat("Total PnL:", total_pnl, "\n")
cat("Total Number of Trading Days:", total_trades, "\n")
cat("Taken Trades:", taken_trades, "\n")
cat("Skipped Trades:", skipped_trades, "\n")
cat("Profitable Trades:", profitable_trades, "\n")
cat("Losing Trades:", losing_trades, "\n")
cat("Max Profit:", max_profit, "\n")
cat("Max Loss:", max_loss, "\n")
cat("Average Profit:", average_profit, "\n")
cat("Average Loss:", average_loss, "\n")
cat("Sharpe Ratio:", sharpe_ratio, "\n")
cat("Profit Factor:", profit_factor, "\n")
cat("Win Rate:", win_rate, "%\n")


# Selection of the optimal probability threshold -------------------------------

thresholds <- seq(0.5, 0.9, by = 0.01)

# Create a data frame to store results
results <- data.frame(put_threshold = numeric(),
                      call_threshold = numeric(),
                      cum_pnl = numeric())

# Loop through each put threshold value
for (thresh in thresholds) {
  # Calculate the corresponding call threshold
  other_thresh <- 1 - thresh
  
  # Apply the thresholds to make predictions
  prediction_put <- as.factor(ifelse(glmnet_pred$Yes > thresh, "Yes", "No"))
  prediction_call <- as.factor(ifelse(glmnet_pred$Yes > other_thresh, "Yes", "No"))
  
  # Backtest the PnL based on the current thresholds
  backtest <- data_riskcalc[test_indices,]
  backtest$prediction_put <- prediction_put
  backtest$prediction_call <- prediction_call
  backtest$final_day <- as.factor(backtest$final_day)
  
  # Merge with options prices
  backtest <- merge(backtest, options_prices, by = "date")
  
  # Calculate pnl for puts
  backtest$pnl_puts <- ifelse(
    backtest$prediction_put == "Yes" & backtest$final_day == "Yes", 
    backtest$price_option - backtest$price_option * 0.2,
    ifelse(
      backtest$prediction_put == "Yes" & backtest$final_day == "No", 
      backtest$fc - backtest$open + backtest$price_option,
      0
    )
  )
  
  # Calculate pnl for calls
  backtest$pnl_call <- ifelse(
    backtest$prediction_call == "No" & backtest$final_day == "No", 
    backtest$price_option - backtest$price_option * 0.2,
    ifelse(
      backtest$prediction_call == "No" & backtest$final_day == "Yes", 
      backtest$open - backtest$fc + backtest$price_option,
      0
    )
  )
  
  # Calculate total pnl and cumulative pnl
  backtest$pnl <- backtest$pnl_puts + backtest$pnl_call
  backtest$cum_pnl <- cumsum(backtest$pnl)
  
  # Calculate drawdowns for puts
  backtest$drawdown_puts <- ifelse(
    backtest$prediction_put == "Yes", 
    pmin(backtest$fl - backtest$open, 0),  
    0
  )
  
  # Calculate drawdowns for calls
  backtest$drawdown_call <- ifelse(
    backtest$prediction_call == "No", 
    pmin(backtest$open - backtest$fh, 0),  
    0
  )
  
  # Calculate total drawdown and drawdown threshold
  backtest$drawdown <- backtest$drawdown_puts + backtest$drawdown_call
  backtest$drawdown_treshold <- backtest$open * -0.0532
  
  # Calculate statistics: cumulative pnl and mean drawdown
  cum_pnl <- sum(backtest$pnl)
  
  # Store the results for this threshold combination
  results <- rbind(results, data.frame(
    put_threshold = thresh,
    call_threshold = other_thresh,
    cum_pnl = cum_pnl
  ))
}

# View the results to select the best thresholds
print(results)

ggplot(results, aes(x = put_threshold, y = cum_pnl)) +
  geom_line(color = "black", size = 1) +    # Line plot for PnL
  geom_point(color = "darkgray", size = 2) +    # Points on the line
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +  # Reference line at PnL = 0
  labs(
    x = "Threshold",
    y = "Cumulative PnL"
  ) +
  theme_minimal() 

# Backtesting  -----------------------------------------------------------------

prediction_put <- as.factor(ifelse(glmnet_pred$Yes > 0.52, "Yes", "No"))
prediction_call <- as.factor(ifelse(glmnet_pred$Yes > 0.48, "Yes", "No"))
actual <- test_data$final_day

# confusion_matrix <- confusionMatrix(prediction_put, actual)
# confusion_matrix
# confusion_matrix <- confusionMatrix(prediction_call, actual)
# confusion_matrix

backtest <- data_riskcalc[test_indices,]
backtest$prediction_put <- prediction_put
backtest$prediction_call <- prediction_call
backtest$final_day <- as.factor(backtest$final_day)

backtest <- merge(backtest, options_prices, by = "date")

# PnL
backtest$pnl_puts <- ifelse(
  backtest$prediction_put == "Yes" & backtest$final_day == "Yes", 
  backtest$price_option - backtest$price_option * 0.2,  # Take value from price_option
  ifelse(
    backtest$prediction_put == "Yes" & backtest$final_day == "No", 
    backtest$fc - backtest$open + backtest$price_option,  # Calculate fc - open + price_option
    0  # Set to 0 if prediction_put is No
  )
)

backtest$pnl_call <- ifelse(
  backtest$prediction_call == "No" & backtest$final_day == "No", 
  backtest$price_option - backtest$price_option * 0.2,  # Take value from price_option
  ifelse(
    backtest$prediction_call == "No" & backtest$final_day == "Yes", 
    backtest$open - backtest$fc + backtest$price_option,  # Calculate open - fc + price_option
    0  # Set to 0 if prediction_call is Yes
  )
)

backtest$pnl <- backtest$pnl_puts + backtest$pnl_call
backtest$cum_pnl <- cumsum(backtest$pnl)

# Drawdown
backtest$drawdown_puts <- ifelse(
  backtest$prediction_put == "Yes", 
  pmin(backtest$fl - backtest$open, 0),  # Calculate fl - open and ensure it’s <= 0
  0  # Set to 0 if prediction_put is No
)
backtest$drawdown_call <- ifelse(
  backtest$prediction_call == "No", 
  pmin(backtest$open - backtest$fh, 0),  # Calculate open - fh and ensure it’s <= 0
  0  # Set to 0 if prediction_call is Yes
)
backtest$drawdown <- backtest$drawdown_puts + backtest$drawdown_call
backtest$drawdown_treshold <- backtest$open * -0.0532

# Plot for pnl
pnl_plot <- ggplot(backtest, aes(x = date, y = cum_pnl)) +
  geom_line(color = "black", size = 0.5) +
  labs(
    title = "PnL Over Time",
    x = "Date",
    y = "PnL"
  ) +
  theme_minimal()

# Plot for drawdown with threshold line
drawdown_plot <- ggplot(backtest, aes(x = date)) +
  geom_line(aes(y = drawdown), color = "darkorange", size = 0.5) +
  geom_line(aes(y = drawdown_treshold), linetype = "dotted", color = "red", size = 0.8) +  # Plot the threshold as a dotted line
  labs(
    title = "Drawdown Over Time with Threshold",
    x = "Date",
    y = "Drawdown"
  ) +
  theme_minimal()

combined_plot <- pnl_plot / drawdown_plot
combined_plot

total_pnl <- sum(backtest$pnl)
total_trades <- nrow(backtest)
taken_trades <- sum(backtest$prediction_put == "Yes" | backtest$prediction_call == "No")
skipped_trades <- total_trades - taken_trades
profitable_trades <- sum(backtest$pnl > 0)
losing_trades <- sum(backtest$pnl < 0)
max_profit <- max(backtest$pnl)
max_loss <- min(backtest$pnl)
average_profit <- mean(backtest$pnl[backtest$pnl > 0])
average_loss <- mean(backtest$pnl[backtest$pnl < 0])
sharpe_ratio <- mean(backtest$pnl) / sd(backtest$pnl)
total_profit <- sum(backtest$pnl[backtest$pnl > 0])
total_loss <- abs(sum(backtest$pnl[backtest$pnl < 0]))
profit_factor <- total_profit / total_loss
win_rate <- (profitable_trades / taken_trades) * 100

# Print the results

cat("Total PnL:", total_pnl, "\n")
cat("Total Number of Trading Days:", total_trades, "\n")
cat("Taken Trades:", taken_trades, "\n")
cat("Skipped Trades:", skipped_trades, "\n")
cat("Profitable Trades:", profitable_trades, "\n")
cat("Losing Trades:", losing_trades, "\n")
cat("Max Profit:", max_profit, "\n")
cat("Max Loss:", max_loss, "\n")
cat("Average Profit:", average_profit, "\n")
cat("Average Loss:", average_loss, "\n")
cat("Sharpe Ratio:", sharpe_ratio, "\n")
cat("Profit Factor:", profit_factor, "\n")
cat("Win rate:", win_rate, "\n")
