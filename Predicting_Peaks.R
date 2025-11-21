#-----------------------------
# Load data 
#-----------------------------

# Clean environment
rm(list=ls())

# Load libraries 
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(moments)
library(zoo)  
library(corrplot)
library(flextable)
library(rpart.plot)
library(randomForest)
library(ggplot2)
library(caret)
library(glmnet)
library(purrr)
library(pROC)

# Load data
IPL_Player_Stats_2016_till_2019 <- read.csv("IPL Player Stats - 2016 till 2019.csv")
View(IPL_Player_Stats_2016_till_2019) # View data 
# Number of unique players
length(unique(IPL_Player_Stats_2016_till_2019$Player))

#-----------------------------
# Prepare data 
#-----------------------------

# Rename column 
IPL_batters <- IPL_Player_Stats_2016_till_2019 %>%
  rename(Tournament.Year = Tournament,
         Runs.Scored = Runds.Scored)

# Select columns we are interested in to predict batting performance 
IPL_batters <- IPL_batters %>%
  select(Player, Tournament.Year, Matches, Batting.Innings, Runs.Scored, Highest.Score, Batting.Average, Balls.Faced, Batting.Strike.Rate, X100, X50, X0, X4s, X6s)

# Inspect data 
str(IPL_batters)
summary(IPL_batters)
head(IPL_batters)

# Change '-' to 'NA'
IPL_batters <- IPL_batters %>%
  mutate(across(where(is.character), ~ na_if(., "-")))

# Flag not out for highest score so highest score can be treated as numeric  
IPL_batters <- IPL_batters %>%
  mutate(
    Highest.Score = as.numeric(gsub("\\*", "", Highest.Score)) # Remove "*" and convert to numeric
  )

# Extract the year and update the column
IPL_batters$Tournament.Year <- gsub("IPL ", "", IPL_batters$Tournament.Year)

# Change variable types 
IPL_batters <- IPL_batters %>%
  mutate(Player = as.factor(Player),
         Tournament.Year = as.numeric(Tournament.Year),
         Batting.Innings = as.numeric(Batting.Innings),
         Runs.Scored = as.numeric(Runs.Scored),
         Highest.Score = as.numeric(Highest.Score),
         Batting.Average = as.numeric(Batting.Average),
         Balls.Faced = as.numeric(Balls.Faced),
         Batting.Strike.Rate = as.numeric(Batting.Strike.Rate),
         X100 = as.numeric(X100),
         X50 = as.numeric(X50),
         X0 = as.numeric(X0), 
         X4s = as.numeric(X4s),
         X6s = as.numeric(X6s))

# Remove previous dataframe
rm(IPL_Player_Stats_2016_till_2019)


#-----------------------------
# Clean data 
#-----------------------------

# Check if any missing values 
missing_values <- colSums(is.na(IPL_batters))
print(missing_values)

# Remove those with missing values in batting innings as concerned only with those who have batted
# Removing those who are missing batting average as there is no valid imputation method 
IPL_batters <- IPL_batters %>%
  filter(!is.na(Batting.Innings),
         !is.na(Batting.Average))

# Check for duplicates 
sum(duplicated(IPL_batters[, c("Player", "Tournament.Year")]))

# Check for outliers visually
numeric_data <- IPL_batters[sapply(IPL_batters, is.numeric)]
boxplot(numeric_data, main="Boxplot of All Numerical Variables", (ncol(numeric_data)), las=2)

# Calculate skewness to set outlier threshold for z-scores
skew_value <- skewness(numeric_data)
print(skew_value)

# Use z-scores to see outliers 
z_scores <- scale(numeric_data) # scale data
threshold = 2.5 
outliers_z <- numeric_data[abs(z_scores) > threshold] 
print(outliers_z) 
# Outliers listed are real values so making the decision to keep them in the analysis


#-----------------------------
# Descriptive statistics 
#-----------------------------

# Basic dataset overview
cat("Number of players:", length(unique(IPL_batters$Player)), "\n")
cat("Number of player-seasons:", nrow(IPL_batters), "\n")
cat("Years covered:", range(IPL_batters$Tournament.Year), "\n\n")

# Summary statistics for key metrics
desc_stats <- IPL_batters %>%
  select(Runs.Scored, Batting.Average, Batting.Strike.Rate, X4s, X6s, X50) %>%
  psych::describe(quant = c(0.25, 0.75)) %>%  # Add IQR
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  select(-vars, -trimmed, -mad, -range, -n, -min, -max, -se)
print(desc_stats)

# Convert rownames to a column and modify row names
desc_stats <- desc_stats %>%
  tibble::rownames_to_column(var = "Metric") %>%
  mutate(Metric = case_when(
    Metric == "Runs.Scored" ~ "Runs Scored",
    Metric == "Batting.Average" ~ "Batting Average",
    Metric == "Batting.Strike.Rate" ~ "Strike Rate",
    Metric == "X4s" ~ "4s (Fours)",
    Metric == "X6s" ~ "6s (Sixes)",
    Metric == "X50" ~ "Fifties",
    TRUE ~ Metric  # Default to original if no match
  ))

# Table for report
Table1<-flextable(desc_stats)
Table1 <- set_header_labels(Table1, 
                            Statistic="", 
                            mean = "Average",
                            median = "Median",
                            sd = "Standard Deviation",
                            skew = "Skewness",
                            kurtosis = "Kurtosis",
                            Q0.25 = "Q1",
                            Q0.75 = "Q3")
Table1<-set_table_properties(Table1, layout = "autofit")
Table1 <- theme_vanilla(Table1)
Table1 <- align(Table1, j = c(1:4), align = "center", part = "all")
# Change font of the table
Table1 <- font(Table1, font = "Times New Roman", part = "all")
Table1

# Visualisations
# Histograms for key metrics
# Set up multi-panel plot (2x2 grid)
par(mfrow = c(2, 2))  # 2 rows, 2 columns
# Histograms for key metrics
hist(IPL_batters$Runs.Scored, main = "Distribution of Runs Scored", xlab = "Runs Scored", col = "dodgerblue4", breaks = 30)
hist(IPL_batters$Batting.Strike.Rate, main = "Distribution of Strike Rate", xlab = "Strike Rate", col = "dodgerblue4", breaks = 30)
hist(IPL_batters$X4s, main = "Distribution of the Number of 4s", xlab = "4s", col = "dodgerblue4", breaks = 30)
hist(IPL_batters$X50, main = "Distribution of the Number of 50s", xlab = "50s", col = "dodgerblue4", breaks = 30)
# Reset plot layout
par(mfrow = c(1, 1))  

# Boxplots by year
ggplot(IPL_batters, aes(x = factor(Tournament.Year), y = Batting.Strike.Rate)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Batting Strike Rate by Year", x = "Year") +
  theme_minimal()

# Correlation matrix 
cor_matrix <- cor(select(IPL_batters, where(is.numeric)), use = "complete.obs")
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black")


#-----------------------------
# Create peak data
#-----------------------------

# Create new 'IsPeak' column
IPL_batters <- IPL_batters %>%
  group_by(Player) %>%
  mutate(IsPeak = ifelse(Batting.Strike.Rate == max(Batting.Strike.Rate, na.rm = TRUE), 1, 0)) %>%
  ungroup()

# Identify each player's peak year
peak_seasons <- IPL_batters %>%
  filter(IsPeak == 1) %>%
  select(Player, Peak.Year = Tournament.Year)

# Join and filter only seasons before the peak
pre_peak_data <- IPL_batters %>%
  inner_join(peak_seasons, by = "Player") %>%
  filter(Tournament.Year < Peak.Year)

# Create test data 
peak_test_data <- IPL_batters %>%
  inner_join(peak_seasons, by = c("Player" = "Player"))
# Remove players who peaked in their first season
peak_test_data <- peak_test_data %>%
  group_by(Player) %>%
  filter(Peak.Year != 2016) %>%
  ungroup()


#-----------------------------
# Peak season analysis (player-specific peaks)
#-----------------------------

# Compare peak vs overall stats
strike_rate_comparison <- IPL_batters %>%
  summarise(
    Overall_Mean_SR = mean(Batting.Strike.Rate, na.rm = TRUE),
    Peak_Mean_SR = mean(Batting.Strike.Rate[IsPeak == 1], na.rm = TRUE),
    Improvement_Pct = (mean(Batting.Strike.Rate[IsPeak == 1], na.rm = TRUE) - 
                         mean(Batting.Strike.Rate, na.rm = TRUE)) / 
      mean(Batting.Strike.Rate, na.rm = TRUE) * 100,
    N_Peak_Seasons = sum(IsPeak == 1, na.rm = TRUE),
    N_NotPeak_Seasons = sum(IsPeak == 0, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))
print(strike_rate_comparison)

# Peak vs Pre-Peak Comparison
comparison_plot <- peak_test_data %>%
  mutate(Peak_Status = ifelse(IsPeak == 1, "Peak Season", "Non-Peak")) %>%
  select(Peak_Status, Batting.Strike.Rate, X4s, Batting.Average) %>%
  pivot_longer(-Peak_Status) %>%
  mutate(name = case_when(
    name == "Batting.Strike.Rate" ~ "Strike Rate",
    name == "X4s" ~ "4s",
    name == "Batting.Average" ~ "Average"
  )) %>%
  ggplot(aes(x = Peak_Status, y = value, fill = Peak_Status)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Peak Season" = "dodgerblue4", "Non-Peak" = "chocolate2")) +
  facet_wrap(~name, scales = "free_y") +
  labs(title = "Performance Peak vs Non-Peak Seasons",
       x = "", y = "Value", fill = "Season Type") +
  theme_minimal() +
  theme(legend.position = "top")
comparison_plot


#-----------------------------
# Predicting peak performance 
#-----------------------------
# Feature engineering
#-----------------------------

# Combine prepeak and testpeak for training data
train_data <- rbind(pre_peak_data, peak_test_data)

# Remove duplicates 
train_data <- distinct(train_data)

# Feature engineering function
create_features <- function(train_data) {
  # Sort data by Player and Tournament.Year
  train_data <- train_data %>% arrange(Player, Tournament.Year)
  
  # Create a list to store processed data for each player
  player_list <- list()
  
  # Get unique players
  players <- unique(train_data$Player)
  
  # Process each player separately
  for (player in players) {
    player_data <- train_data %>% filter(Player == player)
    
    # Features for current year
    current_features <- c(
      'Runs.Scored', 'Batting.Average', 'Batting.Strike.Rate',
      'X100', 'X50', 'X0', 'X4s', 'X6s'
    )
    
    # Add previous year features
    for (feature in current_features) {
      player_data[paste0('prev1_', feature)] <- lag(player_data[[feature]], 1)
      player_data[paste0('prev2_', feature)] <- lag(player_data[[feature]], 2)
      player_data[paste0('trend_', feature)] <- player_data[[feature]] - player_data[[paste0('prev1_', feature)]]
      
      # Fill NA values with 0 
      player_data[paste0('prev1_', feature)][is.na(player_data[paste0('prev1_', feature)])] <- 0
      player_data[paste0('prev2_', feature)][is.na(player_data[paste0('prev2_', feature)])] <- 0
      player_data[paste0('trend_', feature)][is.na(player_data[paste0('trend_', feature)])] <- 0
    }
    
    # Career statistics
    player_data <- player_data %>%
      mutate(
        career_runs = cumsum(Runs.Scored) - Runs.Scored,
        career_innings = cumsum(Batting.Innings) - Batting.Innings,
        career_avg = ifelse(career_innings > 0, career_runs / career_innings, NA)
      )
    
    # Fill NA values with 0 
    player_data[paste0('career_avg')][is.na(player_data[paste0('career_avg')])] <- 0
    
    # Experience in years
    player_data$experience <- seq_len(nrow(player_data)) - 1
    
    # Add to list
    player_list[[player]] <- player_data
  }
  
  # Combine all players back into a single dataframe
  result <- do.call(rbind, player_list)
  return(result)
}

# Apply feature engineering
train_data <- create_features(train_data)


#-----------------------------
# Model training preparation
#-----------------------------

# Prepare training data
# Remove specified columns
cols_to_drop <- c('Player', 'Tournament.Year', 'IsPeak', 'PeakYear')
X <- train_data %>% 
  select(-one_of(intersect(names(train_data), cols_to_drop)))
y <- train_data$IsPeak

# Handle missing values
X[is.na(X)] <- 0

# Split data
set.seed(42)  # For reproducibility
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]


#-----------------------------
# Logistic Regression Implementation
#-----------------------------

# Prepare data for logistic regression
# Scale numeric features 
preProc <- preProcess(X_train, method = c("center", "scale"))
X_train_scaled <- predict(preProc, X_train)
X_test_scaled <- predict(preProc, X_test)

# Convert to dataframe 
train_df <- as.data.frame(X_train_scaled)
train_df$IsPeak <- y_train

# Train logistic regression model
logit_model <- glm(IsPeak ~ ., 
                   data = train_df, 
                   family = binomial(link = "logit"))

# Summary of model
summary(logit_model)

exp(coef(logit_model))  # Convert log-odds to odds ratios

# Predictions
logit_probs <- predict(logit_model, 
                       newdata = as.data.frame(X_test_scaled), 
                       type = "response")
logit_preds <- ifelse(logit_probs > 0.5, 1, 0)

# Evaluate
logit_confMatrix <- confusionMatrix(as.factor(logit_preds), as.factor(y_test))
print(logit_confMatrix)

# ROC/AUC analysis
logit_roc <- roc(response = y_test, 
                 predictor = logit_probs,
                 levels = c(0, 1))
plot(logit_roc, main = "Logistic Regression ROC Curve", print.auc = TRUE)
logit_auc <- auc(logit_roc)


#-----------------------------
# Train random forest model
#-----------------------------

# Train model
model <- randomForest(x = X_train, 
                      y = as.factor(y_train),
                      ntree = 100,            # Number of trees 
                      mtry = 5,               # Number of features at each split
                      importance = TRUE)

# Evaluate
y_pred <- predict(model, X_test)
confMatrix <- confusionMatrix(as.factor(y_pred), as.factor(y_test))
print(confMatrix)

# Print detailed metrics 
print("Detailed Metrics:")
print(confMatrix$byClass)

# Visualising individual trees within the model 
single_tree <- randomForest::getTree(model, k=1, labelVar=TRUE)
single_tree


#-----------------------------
# Recursive Feature Elimination 
#-----------------------------

# RFE with Random Forest
control <- rfeControl(functions = rfFuncs,  
                      method = "cv",        # Cross-validation
                      number = 5)           # Number of folds

# Run RFE
set.seed(42)
rfe_results <- rfe(x = X_train, 
                   y = as.factor(y_train),
                   sizes = c(1:40),        # Range of feature subset sizes to test
                   rfeControl = control)

# Print results
print(rfe_results)
# Plot results
plot(rfe_results, type = c("g", "o"))

# Get optimal features
optimal_features <- predictors(rfe_results)
X_train_rfe <- X_train[, optimal_features]
X_test_rfe <- X_test[, optimal_features]

# Retrain model with selected features
model_rfe <- randomForest(x = X_train_rfe, 
                          y = as.factor(y_train),
                          ntree = 100,
                          mtry = 5,
                          importance = TRUE)


#-----------------------------
# Evaluating RFE
#-----------------------------
# Classification metrics 
#-----------------------------

# Predict on test set
y_pred <- predict(model_rfe, X_test_rfe)
y_prob <- predict(model_rfe, X_test_rfe, type = "prob")[,2]  # Probability estimates

# Confusion matrix
conf_matrix <- confusionMatrix(y_pred, as.factor(y_test))
print(conf_matrix)

# Extended classification report
print("Detailed Performance Metrics:")
print(conf_matrix$byClass)  

# Extract the confusion matrix table
cm_table <- as.table(conf_matrix$table)

# Convert to data frame for ggplot
cm_df <- as.data.frame(cm_table)
colnames(cm_df) <- c("Predicted", "Actual", "Freq")

# Plot using ggplot2
ggplot(data = cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "lightblue", high = "dodgerblue4") +
  theme_minimal() +
  labs(title = "Confusion Matrix", x = "Actual Label", y = "Predicted Label")


#-----------------------------
# ROC and AUC analysis
#-----------------------------

# Calculate ROC curve
roc_obj <- roc(response = y_test, 
               predictor = y_prob,
               levels = c(0, 1))  

# Plot ROC curve
plot(roc_obj, main = "ROC Curve", print.auc = TRUE)

# AUC value
auc_value <- auc(roc_obj)
print(paste("AUC:", auc_value))


#-----------------------------
# Feature importance analysis
#-----------------------------

# Get importance scores
importance_scores <- importance(model_rfe)
varImpPlot(model_rfe, main = "Feature Importance")

# Convert to data frame for better visualisation
importance_df <- data.frame(
  Feature = rownames(importance_scores),
  MeanDecreaseGini = importance_scores[, "MeanDecreaseGini"]
) %>% arrange(desc(MeanDecreaseGini))

# Plot using ggplot2
ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "dodgerblue4") +
  coord_flip() +
  labs(title = "Feature Importance (RFE-Selected Model)", 
       x = "Features", 
       y = "Mean Decrease in Gini") + 
  theme_minimal()


#-----------------------------
# Compare RFE to baseline
#-----------------------------

# Train baseline model with all features
model_baseline <- randomForest(x = X_train, 
                               y = as.factor(y_train),
                               ntree = 100,
                               mtry = 5)

# Predict with baseline
y_pred_base <- predict(model_baseline, X_test)
conf_matrix_base <- confusionMatrix(y_pred_base, as.factor(y_test))

# Compare AUC
y_prob_base <- predict(model_baseline, X_test, type = "prob")[,2]
auc_base <- auc(roc(response = y_test, predictor = y_prob_base))

# Create comparison table
comparison <- data.frame(
  Model = c("Baseline", "RFE-Optimised"),
  Accuracy = c(conf_matrix_base$overall["Accuracy"], conf_matrix$overall["Accuracy"]),
  AUC = c(auc_base, auc_value),
  Sensitivity = c(conf_matrix_base$byClass["Sensitivity"], conf_matrix$byClass["Sensitivity"]),
  Specificity = c(conf_matrix_base$byClass["Specificity"], conf_matrix$byClass["Specificity"])
)
print(comparison)


#-----------------------------
# Error rate across trees
#-----------------------------

# Define custom colors for the plot and legend
custom_colors <- c("dodgerblue4", "chocolate2", "black")  

# Plot error rate vs number of trees 
plot(model_rfe, main = "Out-of-Bag (OOB) Error Rate vs Number of Trees", 
     col = custom_colors)  

# Add legend with custom colors
legend("topright", 
       legend = colnames(model_rfe$err.rate),
       col = custom_colors,  
       lty = 1, cex = 0.8)

#-----------------------------
# Class probability distribution
#-----------------------------

# Create probability density plot
prob_df <- data.frame(
  Probability = y_prob,
  Actual = factor(y_test, levels = c(0, 1), labels = c("No Peak", "Peak"))
)

ggplot(prob_df, aes(x = Probability, fill = Actual)) +
  geom_density(alpha = 0.5) +
  labs(title = "Predicted Probability Distribution",
       x = "Predicted Probability of Peak",
       y = "Density") +
  theme_minimal()

# T-test for comparing probabilities between the two classes
t_test_result <- t.test(Probability ~ Actual, data = prob_df)
print(t_test_result)


#-----------------------------
# Hyperparameter tuning
#-----------------------------

# Define tuning grid
rf_grid <- expand.grid(
  mtry = c(3, 5, 7, 9),        
  splitrule = "gini",          
  min.node.size = c(1, 3, 5)   
)

# Set up train control
ctrl <- trainControl(
  method = "cv",               
  number = 5,                 
  classProbs = TRUE,           # For probability estimates
  summaryFunction = twoClassSummary  # For binary classification metrics
)

# Convert y to factor with proper level names 
y_train_factor <- factor(y_train, levels = c(0, 1), labels = c("No", "Yes"))

# Train model with tuning
set.seed(42)
rf_tuned <- train(
  x = X_train,
  y = y_train_factor,
  method = "ranger",          # Faster implementation of Random Forest
  trControl = ctrl,
  tuneGrid = rf_grid,
  metric = "ROC",             # Optimise for AUC-ROC
  importance = "permutation", # Calculate variable importance
  num.trees = 500             # Increase number of trees
)

# View results
print(rf_tuned)
plot(rf_tuned)

# Best model
best_rf <- rf_tuned$finalModel


#-----------------------------
# Model Comparison
#-----------------------------

# Create comparison table
model_comparison <- data.frame(
  Model = c("Random Forest", "Logistic Regression"),
  Accuracy = c(confMatrix$overall["Accuracy"], logit_confMatrix$overall["Accuracy"]),
  AUC = c(auc_value, logit_auc),
  Sensitivity = c(confMatrix$byClass["Sensitivity"], logit_confMatrix$byClass["Sensitivity"]),
  Specificity = c(confMatrix$byClass["Specificity"], logit_confMatrix$byClass["Specificity"]),
  Precision = c(confMatrix$byClass["Precision"], logit_confMatrix$byClass["Precision"])
)

print(model_comparison)

# Visual comparison of ROC curves
plot(roc_obj, col = "dodgerblue4", main = "ROC Curve Comparison")
plot(logit_roc, col = "chocolate2", add = TRUE)
legend("bottomright", 
       legend = c(paste("Random Forest (AUC =", round(auc_value, 3), ")"), 
                  paste("Logistic Regression (AUC =", round(logit_auc, 3), ")")),
       col = c("dodgerblue4", "chocolate2"), lwd = 2)


#-----------------------------
# Predicting peaks for current players
#-----------------------------

# Prepare current player data for prediction
current_players <- train_data  
cols_to_drop <- c('Player', 'Tournament.Year')
X_current <- current_players %>%
  select(-one_of(intersect(names(current_players), cols_to_drop)))

# Handle missing values
X_current[is.na(X_current)] <- 0

# Predict probabilities
predictions <- predict(model_rfe, X_current, type = "prob")
current_players$peak_probability <- predictions[, "1"]  

# Get predictions for the most recent year
latest_year <- max(current_players$Tournament.Year)
current_predictions <- current_players %>%
  filter(Tournament.Year == latest_year)

# Players likely to peak next season
players_to_watch <- current_predictions %>%
  arrange(desc(peak_probability)) %>%
  slice_head(n = 10)

# Print results
print(players_to_watch %>% 
        select(Player, peak_probability, Runs.Scored, Batting.Average, Batting.Strike.Rate))

# Subset columns for table 
players_table_data <- players_to_watch %>%
  select(Player, peak_probability, Runs.Scored, Batting.Average, Batting.Strike.Rate)

# Table for report
Table2<-flextable(players_table_data) 
Table2 <- set_header_labels(Table2, 
                            Statistic="", 
                            Player = "Player",
                            peak_probability = "Peak Probability",
                            Runs.Scored = "Runs Scored",
                            Batting.Average = "Batting Average",
                            Batting.Strike.Rate = "Strike Rate")
Table2<-set_table_properties(Table2, layout = "autofit")
Table2 <- theme_vanilla(Table2)
Table2 <- align(Table2, j = c(1:4), align = "center", part = "all")
# Change font of the table
Table2 <- font(Table2, font = "Times New Roman", part = "all")
Table2

