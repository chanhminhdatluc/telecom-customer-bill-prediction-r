getwd()
#Install Packages 
install.packages('tidyverse')
install.packages('skimr')
install.packages('janitor')
install.packages('naniar')
install.packages('DataExplorer')
install.packages('corrplot')
install.packages('caret')
install.packages('rpart')
install.packages('rpart.plot')
install.packages("rpart.rules")
install.packages('VIM')
install.packages('fastDummies')
install.packages('psych')
install.packages('e1071')
install.packages('car')
install.packages('Metrics')
install.packages("lm.beta")

library(tidyverse)
library(skimr)
library(janitor)
library(naniar)
library(DataExplorer)
library(corrplot)
library(caret)
library(rpart)
library(rpart.plot)
library(VIM)
library(ggplot2)
library(dplyr)
library(fastDummies)
library(psych)
library(e1071)
library(car)
library(Metrics)
library(lm.beta)

#Part A
#Q1. Understanding Data Types and Transformations 
#Load dataset and inspect structure
connecttel <- read_csv("ConnectTel_customer_billing_dataset.csv")
connecttel <- clean_names(connecttel)
str(connecttel)
head(connecttel)
glimpse(connecttel)

#Separate variables by type
#Continuous numerical variables
continuous_vars <- connecttel %>%
  select(where(is.numeric))

names(continuous_vars)

#Categorical variables
categorical_vars <- connecttel %>%
  select(where(is.character))

names(categorical_vars)

#Check unique values (to classify nominal vs ordinal)
lapply(connecttel[categorical_vars %>% names()], unique)

#Identify useless variables
n_distinct(connecttel$customer_id)
nrow(connecttel)

numeric_data <- connecttel %>%
  select(where(is.numeric))

cor_matrix <- cor(numeric_data)

corrplot(cor_matrix,
         method="color",
         type="upper",
         tl.cex=0.6)

#Convert categorical variables for modelling
connecttel <- connecttel %>%
  mutate(
    device_type = as.factor(device_type),
    device_category = as.factor(device_category),
    gender = as.factor(gender),
    district_name = as.factor(district_name),
    age_group = factor(age_group,
                       ordered = TRUE)
  )

str(connecttel)

#Example encoding
connecttel_dummy <- dummy_cols(connecttel,
                               remove_first_dummy = TRUE,
                               remove_selected_columns = TRUE)

head(connecttel_dummy)

#Remove identifier variable
connecttel <- connecttel %>%
  select(-customer_id)

connecttel_scaled <- connecttel %>%
  mutate(across(where(is.numeric),
                scale))


#Q2. Exploratory Data Analysis
#Data preparation workflow
#Check missing values
colSums(is.na(connecttel))

missing_percent <- colSums(is.na(connecttel))/nrow(connecttel)*100

missing_percent

missing_table <- data.frame(
  Variable = names(connecttel),
  Missing_Count = colSums(is.na(connecttel)),
  Missing_Percent = colSums(is.na(connecttel))/nrow(connecttel)*100
)

missing_table

#Check duplicates
sum(duplicated(connecttel))
summary(connecttel)

#Distribution visualisations
numeric_data <- connecttel %>%
  select(where(is.numeric))

#Histograms for all continuous variables
numeric_data %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(value)) +
  geom_histogram(bins=30, fill="steelblue", color="black") +
  facet_wrap(~name, scales="free") +
  theme_minimal()

#Boxplots for outlier detection\
numeric_data %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x="", y=value)) +
  geom_boxplot(fill="orange") +
  facet_wrap(~name, scales="free") +
  theme_minimal()

#Descriptive statistics table
describe(numeric_data)

#Variability analysis Find highest standard deviation:
variability <- numeric_data %>%
  summarise(across(everything(), sd, na.rm=TRUE))

variability
variability_long <- variability %>%
  pivot_longer(cols = everything(),
               names_to="Variable",
               values_to="SD") %>%
  arrange(desc(SD))

variability_long

#Skewness analysis
skewness_values <- numeric_data %>%
  summarise(across(everything(),
                   ~skewness(., na.rm=TRUE)))

skewness_values
skew_long <- skewness_values %>%
  pivot_longer(cols=everything(),
               names_to="Variable",
               values_to="Skewness") %>%
  arrange(desc(abs(Skewness)))

skew_long

#Outlier detection (IQR method)
outlier_check <- function(x){
  
  Q1 <- quantile(x,0.25,na.rm=TRUE)
  
  Q3 <- quantile(x,0.75,na.rm=TRUE)
  
  IQR <- Q3-Q1
  
  lower <- Q1-1.5*IQR
  
  upper <- Q3+1.5*IQR
  
  sum(x<lower | x>upper, na.rm=TRUE)
  
}

outliers <- numeric_data %>%
  summarise(across(everything(), outlier_check))

outliers

outlier_long <- outliers %>%
  pivot_longer(cols=everything(),
               names_to="Variable",
               values_to="Outlier_Count") %>%
  arrange(desc(Outlier_Count))

outlier_long

#Visualising extreme variables
ggplot(connecttel,
       aes(x=avg_monthly_bill_amount)) +
  geom_histogram(fill="red", bins=30)

ggplot(connecttel,
       aes(y=avg_monthly_bill_amount)) +
  geom_boxplot(fill="yellow")

cor_matrix <- cor(numeric_data,
                  use="complete.obs")

corrplot(cor_matrix,
         method="color",
         type="upper",
         tl.cex=0.6)

#Q3. Handling Missing Data 
#Identify variables with missing values
missing_table <- data.frame(
  Variable = names(connecttel),
  Missing_Count = colSums(is.na(connecttel)),
  Missing_Percent = colSums(is.na(connecttel))/nrow(connecttel)*100
)

missing_table %>%
  arrange(desc(Missing_Percent))

vis_miss(connecttel)
gg_miss_var(connecttel)

#Select variables with missing values
missing_vars <- names(connecttel)[colSums(is.na(connecttel)) > 0]

missing_vars

#Remove missing values (listwise deletion)
#Create dataset without NA rows
data_remove <- na.omit(connecttel)

dim(connecttel)

dim(data_remove)
#Compare summary
summary(data_remove$avg_monthly_bill_amount)

summary(connecttel$avg_monthly_bill_amount)

#Plot comparison
ggplot(data_remove,
       aes(avg_monthly_bill_amount)) +
  geom_histogram(fill="red", bins=30) +
  ggtitle("After removing missing values")

#Mean/Median Imputation
data_mean <- connecttel
data_mean <- data_mean %>%
  mutate(across(where(is.numeric),
                ~ifelse(is.na(.),
                        median(.,na.rm=TRUE),
                        .)))
colSums(is.na(data_mean))

summary(data_mean$avg_monthly_bill_amount)

summary(connecttel$avg_monthly_bill_amount)

ggplot(data_mean,
       aes(avg_monthly_bill_amount)) +
  geom_histogram(fill="blue", bins=30) +
  ggtitle("Median imputation")

#KNN Imputation
data_knn <- kNN(connecttel,k=5,imp_var=FALSE)
colSums(is.na(data_knn))
summary(data_knn$avg_monthly_bill_amount)

ggplot(data_knn,
       aes(avg_monthly_bill_amount)) +
  geom_histogram(fill="green", bins=30) +
  ggtitle("KNN imputation")

#Compare the 3 methods
comparison <- data.frame(
  
  Method = c("Original",
             "Remove NA",
             "Median Impute",
             "KNN Impute"),
  
  Mean = c(
    mean(connecttel$avg_monthly_bill_amount,na.rm=TRUE),
    mean(data_remove$avg_monthly_bill_amount),
    mean(data_mean$avg_monthly_bill_amount),
    mean(data_knn$avg_monthly_bill_amount)
  ),
  
  SD = c(
    sd(connecttel$avg_monthly_bill_amount,na.rm=TRUE),
    sd(data_remove$avg_monthly_bill_amount),
    sd(data_mean$avg_monthly_bill_amount),
    sd(data_knn$avg_monthly_bill_amount)
  )
  
)

comparison

#Compare visually
connecttel$Method <- "Original"

data_remove$Method <- "Remove NA"

data_mean$Method <- "Median"

data_knn$Method <- "KNN"

combined <- bind_rows(
  connecttel,
  data_remove,
  data_mean,
  data_knn
)

ggplot(combined,
       aes(avg_monthly_bill_amount,
           fill=Method)) +
  
  geom_density(alpha=0.4)

#Check effect on variance
variance_compare <- data.frame(
  
  Method=c("Original",
           "Remove",
           "Median",
           "KNN"),
  
  Variance=c(
    
    var(connecttel$avg_monthly_bill_amount,
        na.rm=TRUE),
    
    var(data_remove$avg_monthly_bill_amount),
    
    var(data_mean$avg_monthly_bill_amount),
    
    var(data_knn$avg_monthly_bill_amount)
    
  ))

variance_compare

final_data <- data_knn

#Q4. Feature Selection and Relationships 
#Correlation analysis
numeric_data <- connecttel %>%
  select(where(is.numeric))
cor_matrix <- cor(numeric_data,
                  use="complete.obs")

round(cor_matrix,2)

corrplot(cor_matrix,
         method="color",
         type="upper",
         tl.cex=0.6,
         number.cex=0.5)

#Find strong correlations 
cor_matrix <- cor(numeric_data,
                  use = "pairwise.complete.obs")
cor_matrix[is.na(cor_matrix)] <- 0
high_cor <- findCorrelation(cor_matrix, cutoff = 0.85, names = TRUE)
high_cor

#Remove highly correlated variables
data_reduced <- numeric_data %>%
  select(-all_of(high_cor))

ncol(numeric_data)

ncol(data_reduced)

#Correlation with target variable
target_cor <- cor_matrix[,"avg_monthly_bill_amount"]

target_cor_sorted <- sort(target_cor,
                          decreasing = TRUE)

target_cor_sorted

target_cor_sorted <- target_cor_sorted[-1]

target_cor_sorted

target_table <- data.frame(
  
  Variable = names(target_cor_sorted),
  
  Correlation = target_cor_sorted
  
)

target_table

#Visualise top relationships
top_features <- names(target_cor_sorted)[1:5]

top_features

#Scatterplots
ggplot(connecttel,
       aes(x=total_data_activity,
           y=avg_monthly_bill_amount)) +
  
  geom_point(alpha=0.3,
             color="blue") +
  
  geom_smooth(method="lm",
              color="red")

#Scatterplot 2 
ggplot(connecttel,
       aes(x=dusage_sum,
           y=avg_monthly_bill_amount)) +
  
  geom_point(alpha=0.3,
             color="green") +
  
  geom_smooth(method="lm",
              color="red")
#Scatterplot 3
ggplot(connecttel,
       aes(x=total_voice_usage,
           y=avg_monthly_bill_amount)) +
  
  geom_point(alpha=0.3,
             color="purple") +
  
  geom_smooth(method="lm",
              color="red")

#Combined feature importance view
target_table %>%
  top_n(10,Correlation) %>%
  
  ggplot(aes(reorder(Variable,
                     Correlation),
             Correlation)) +
  
  geom_col(fill="steelblue") +
  
  coord_flip() +
  
  ggtitle("Top predictors of monthly bill")

#Check multicollinearity

model_check <- lm(avg_monthly_bill_amount ~
                    
                    total_data_activity +
                    
                    total_voice_usage +
                    
                    dusage_sum +
                    
                    network_stay,
                  
                  data=connecttel)

vif(model_check)

#Variable selection logic
selected_features <- connecttel %>%
  select(
    
    avg_monthly_bill_amount,
    
    total_data_activity,
    
    total_voice_usage,
    
    dusage_sum,
    
    network_stay,
    
    add_on_tot_rental,
    
    digital_engagement_score
    
  )

#feature engineering
connecttel <- connecttel %>%
  
  mutate(
    
    usage_intensity = total_data_activity +
      
      total_voice_usage
    
  )
cor(connecttel$usage_intensity,
    connecttel$avg_monthly_bill_amount,
    use = "complete.obs")


#Assignment Part 2_Building Predictive Models
#Q1. Regression Modelling for Customer Bill Prediction
#Create modelling dataset
data_reg <- connecttel %>%
  mutate(
    usage_intensity = rowSums(
      cbind(total_data_activity, total_voice_usage),
      na.rm = TRUE
    )
  )

#Select final variables:
reg_data <- data_reg %>%
  select(
    avg_monthly_bill_amount,
    total_data_activity,
    add_on_tot_rental,
    usage_intensity,
    network_stay,
    vusage_offnet_avg
  )

#Handle missing values before modelling
reg_data <- reg_data %>%
  mutate(across(
    where(is.numeric),
    ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
  ))

colSums(is.na(reg_data))

#Split into training and testing sets
set.seed(123)

train_index <- createDataPartition(
  reg_data$avg_monthly_bill_amount,
  p = 0.7,
  list = FALSE
)

train_data <- reg_data[train_index, ]
test_data  <- reg_data[-train_index, ]

dim(train_data)
dim(test_data)

#Build the linear regression model
lm_model <- lm(
  avg_monthly_bill_amount ~
    total_data_activity +
    add_on_tot_rental +
    usage_intensity +
    network_stay +
    vusage_offnet_avg,
  data = train_data
)

summary(lm_model)

#Present the regression formula
coef(lm_model)
formula(lm_model)

#Predict on test data
pred_lm <- predict(lm_model, newdata = test_data)

#Evaluate model performance
rmse_lm <- rmse(test_data$avg_monthly_bill_amount, pred_lm)
mae_lm  <- mae(test_data$avg_monthly_bill_amount, pred_lm)

rss <- sum((test_data$avg_monthly_bill_amount - pred_lm)^2)
tss <- sum((test_data$avg_monthly_bill_amount - mean(test_data$avg_monthly_bill_amount))^2)
r2_lm <- 1 - rss/tss

performance_lm <- data.frame(
  Metric = c("RMSE", "MAE", "R-squared"),
  Value = c(rmse_lm, mae_lm, r2_lm)
)

performance_lm

#Compare actual vs predicted values
comparison_lm <- data.frame(
  Actual = test_data$avg_monthly_bill_amount,
  Predicted = pred_lm
)

head(comparison_lm)

#Scatterplot
ggplot(comparison_lm, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.4, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Actual vs Predicted Monthly Bill (Linear Regression)")

#Residual diagnostics
par(mfrow = c(2, 2))
plot(lm_model)

#Check multicollinearity
vif(lm_model)

#Check significance of predictors
summary(lm_model)$coefficients

#Confidence intervals for coefficients
confint(lm_model)

#Standardised coefficients
lm_model_beta <- lm.beta(lm_model)
summary(lm_model_beta)

#Q2. Decision Tree Modelling for Customer Bill Prediction
#Prepare the modelling dataset
data_tree <- connecttel %>%
  mutate(
    usage_intensity = rowSums(
      cbind(total_data_activity, total_voice_usage),
      na.rm = TRUE
    )
  ) %>%
  select(
    avg_monthly_bill_amount,
    total_data_activity,
    add_on_tot_rental,
    usage_intensity,
    network_stay,
    vusage_offnet_avg
  )

#Impute missing numeric values with median
data_tree <- data_tree %>%
  mutate(across(
    where(is.numeric),
    ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
  ))

colSums(is.na(data_tree))

#Train/test split
set.seed(123)

train_index_tree <- createDataPartition(
  data_tree$avg_monthly_bill_amount,
  p = 0.7,
  list = FALSE
)

train_tree <- data_tree[train_index_tree, ]
test_tree  <- data_tree[-train_index_tree, ]

#Build the baseline decision tree
tree_model <- rpart(
  avg_monthly_bill_amount ~
    total_data_activity +
    add_on_tot_rental +
    usage_intensity +
    network_stay +
    vusage_offnet_avg,
  data = train_tree,
  method = "anova"
)

print(tree_model)
summary(tree_model)

#Plot the tree
rpart.plot(
  tree_model,
  type = 2,
  extra = 101,
  fallen.leaves = TRUE,
  cex = 0.7
)

#Predict and evaluate baseline tree
pred_tree <- predict(tree_model, newdata = test_tree)

#Performance metrics
rmse_tree <- rmse(test_tree$avg_monthly_bill_amount, pred_tree)
mae_tree  <- mae(test_tree$avg_monthly_bill_amount, pred_tree)

rss_tree <- sum((test_tree$avg_monthly_bill_amount - pred_tree)^2)
tss_tree <- sum((test_tree$avg_monthly_bill_amount - mean(test_tree$avg_monthly_bill_amount))^2)
r2_tree <- 1 - rss_tree/tss_tree

performance_tree <- data.frame(
  Metric = c("RMSE", "MAE", "R-squared"),
  Value = c(rmse_tree, mae_tree, r2_tree)
)

performance_tree

#Actual vs predicted plot
comparison_tree <- data.frame(
  Actual = test_tree$avg_monthly_bill_amount,
  Predicted = pred_tree
)

ggplot(comparison_tree, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.4, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  ggtitle("Actual vs Predicted Monthly Bill (Decision Tree)")

#Examine the tree structure
tree_model$frame
path.rpart(tree_model, nodes = row.names(tree_model$frame[tree_model$frame$var == "<leaf>", ]))

#Feature importance
tree_model$variable.importance
importance_tree <- data.frame(
  Variable = names(tree_model$variable.importance),
  Importance = as.numeric(tree_model$variable.importance)
) %>%
  arrange(desc(Importance))

importance_tree

ggplot(importance_tree, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  ggtitle("Decision Tree Feature Importance")

#Hyperparameter tuning
#Shallow tree
tree_model_a <- rpart(
  avg_monthly_bill_amount ~
    total_data_activity +
    add_on_tot_rental +
    usage_intensity +
    network_stay +
    vusage_offnet_avg,
  data = train_tree,
  method = "anova",
  control = rpart.control(
    maxdepth = 3,
    minsplit = 30,
    cp = 0.01
  )
)

#Medium tree
tree_model_b <- rpart(
  avg_monthly_bill_amount ~
    total_data_activity +
    add_on_tot_rental +
    usage_intensity +
    network_stay +
    vusage_offnet_avg,
  data = train_tree,
  method = "anova",
  control = rpart.control(
    maxdepth = 5,
    minsplit = 20,
    cp = 0.005
  )
)

#Deeper tree
tree_model_c <- rpart(
  avg_monthly_bill_amount ~
    total_data_activity +
    add_on_tot_rental +
    usage_intensity +
    network_stay +
    vusage_offnet_avg,
  data = train_tree,
  method = "anova",
  control = rpart.control(
    maxdepth = 8,
    minsplit = 10,
    cp = 0.001
  )
)

#Compare tuned trees
evaluate_tree <- function(model, test_data) {
  pred <- predict(model, newdata = test_data)
  rmse_val <- rmse(test_data$avg_monthly_bill_amount, pred)
  mae_val  <- mae(test_data$avg_monthly_bill_amount, pred)
  rss <- sum((test_data$avg_monthly_bill_amount - pred)^2)
  tss <- sum((test_data$avg_monthly_bill_amount - mean(test_data$avg_monthly_bill_amount))^2)
  r2_val <- 1 - rss/tss
  
  data.frame(
    RMSE = rmse_val,
    MAE = mae_val,
    R_squared = r2_val
  )
}

perf_a <- evaluate_tree(tree_model_a, test_tree)
perf_b <- evaluate_tree(tree_model_b, test_tree)
perf_c <- evaluate_tree(tree_model_c, test_tree)

tree_comparison <- bind_rows(
  cbind(Model = "Shallow Tree", perf_a),
  cbind(Model = "Medium Tree", perf_b),
  cbind(Model = "Deep Tree", perf_c)
)

tree_comparison

#Plot the best tree  (Medium tree)
rpart.plot(
  tree_model_b,
  type = 2,
  extra = 101,
  fallen.leaves = TRUE,
  cex = 0.75
)

#Complexity parameter table and pruning
printcp(tree_model)
plotcp(tree_model)

best_cp <- tree_model$cptable[which.min(tree_model$cptable[, "xerror"]), "CP"]
pruned_tree <- prune(tree_model, cp = best_cp)

#Plot pruned tree
rpart.plot(
  pruned_tree,
  type = 2,
  extra = 101,
  fallen.leaves = TRUE,
  cex = 0.75
)

perf_pruned <- evaluate_tree(pruned_tree, test_tree)
perf_pruned

#Human-readable decision rules
rpart.rules(pruned_tree)

#Trace one specific observation through the tree
obs1 <- test_tree[1, ]
obs1

predict(pruned_tree, newdata = obs1)

pred_where <- predict(pruned_tree, newdata = obs1, type = "vector")
pred_where

data.frame(obs1)

#store final decision tree predictions
final_tree_predictions <- data.frame(
  Actual = test_tree$avg_monthly_bill_amount,
  Predicted = predict(pruned_tree, newdata = test_tree)
)

head(final_tree_predictions)

#Q3. Model Comparison
model_comparison <- data.frame(
  Model = c("Linear Regression", "Decision Tree"),
  RMSE = c(1220.03,1255.34),
  MAE = c(852.30,867.12),
  R_squared = c(0.312,0.272)
)

model_comparison
