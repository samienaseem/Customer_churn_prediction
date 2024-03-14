## clear the Global environment and console area
rm(list=ls())

cat("\014")

#load library
library(dplyr)
library(corrplot)
library(ROSE)
library(ggplot2)

#load train and test data
data_train <- read.csv("train_churn.csv") 
data_test <- read.csv("test_churn.csv") 

print(data_train)
head(data_train)

print(data_test)
head(data_test)

#checking dataset summary
summary(data_train)
summary(data_test)

#Checking data types
str(data_train)
str(data_test)

#Dimension of dataset
dim(data_train)
dim(data_test)

#checking null values
colSums(is.na(data_train))
colSums(is.na(data_test))

#checking Duplicate values
sum(duplicated(data_train))
sum(duplicated(data_test))

#checking Unique values
sapply(data_train, function(x) length(unique(x))) #Churn=3 because of empty space 
sapply(data_test, function(x) length(unique(x)))  #Churn=2 True/False

#Checking for empty and blank spaces
count_values <- function(column) {
  na_count <- sum(is.na(column))
  blank_count <- sum(column == "")
  empty_space_count <- sum(column == " ")
  return(c(`NA` = na_count, `Blank` = blank_count, `EmptySpace` = empty_space_count))
}
value_counts <- sapply(data_train, count_values) #Calling the function to each column
print(value_counts)     #114 blank spaces 
 
#Replace empty value with na
replace_empty_value <- function(dataset,value="") {
  for (col_name in colnames(dataset)) {
    indices <- dataset[[col_name]] == value
    dataset[[col_name]][indices] <- NA
  }
  return(dataset)
}

data_train<-replace_empty_value(data_train,"")  #calling function to replace empty value with NA

value_counts <- sapply(data_train, count_values) #Calling the function 
print(value_counts)     #114 blank spaces converted to NA

#Removing unnecessary columns
columns_drop <- c("State","Area.code")
data_train <- data_train[, !(names(data_train) %in%columns_drop)]
data_test <- data_test[, !(names(data_test) %in%columns_drop)]


#identifying numeric and categorical data
numerical_cols <- sapply(data_train, is.numeric)
print(names(data_train)[numerical_cols])

categorical_cols <- sapply(data_train, function(x) is.factor(x) | is.character(x))
print(names(data_train)[categorical_cols])

numerical_cols <- sapply(data_test, is.numeric)
print(names(data_test)[numerical_cols])

categorical_cols <- sapply(data_test, function(x) is.factor(x) | is.character(x))
print(names(data_test)[categorical_cols])


#function to fill NA with mean value for numeric data     
ReplaceWithMean<-function(dataset,columns){
  for(col_name in columns){
    #Find the mean of this column to be replace instead of the null values.
    mean_value<-mean(dataset[[col_name]], na.rm = TRUE)
    dataset[[col_name]][is.na(dataset[[col_name]])] <- mean_value
  }
  return( dataset)
}

Columns<-c("Account.length","Area.code","Number.vmail.messages","Total.day.minutes","Total.day.calls","Total.day.charge","Total.eve.minutes","Total.eve.calls","Total.eve.charge","Total.night.minutes","Total.night.calls","Total.night.charge","Total.intl.minutes","Total.intl.calls","Total.intl.charge","Customer.service.calls")
data_train<-ReplaceWithMean(data_train,Columns) 

colSums(is.na(data_train))


#Label Encoder for binary columns
data_train$Churn <- ifelse(data_train$Churn == 'True', 1,
                     ifelse(data_train$Churn == 'False', 0, data_train$Churn))

data_test$Churn <- ifelse(data_test$Churn == 'True', 1,
                           ifelse(data_test$Churn == 'False', 0, data_test$Churn))

data_train$International.plan <- recode(data_train$International.plan, "Yes" = 1, "No" = 0)
data_test$International.plan <- recode(data_test$International.plan, "Yes" = 1, "No" = 0)


data_train$Voice.mail.plan <- recode(data_train$Voice.mail.plan, "Yes" = 1, "No" = 0)
data_test$Voice.mail.plan <- recode(data_test$Voice.mail.plan, "Yes" = 1, "No" = 0)



print(data_train)
print(data_test)

#function to fill NA with mode value for cat data     
ReplaceWithMode<-function(dataset,columns){
  
  for(col_name in columns){
    #Find the mean of this column to be replace instead of the null values.
    #mode_value<-mode(dataset[[col_name]])
    dataset[[col_name]][is.na(dataset[[col_name]])] <- 1
  }
  return( dataset)
}

Target<-c("Churn")
data_train<-ReplaceWithMode(data_train,Target)  

colSums(is.na(data_train)) #again checking if there is any null values remaining or not

#histogram to identify continuous, ordinal or discrete 
par(mfrow=c(4,4))                                                     #on train data
for (i in names(data_train)[numerical_cols]) {
  hist(data_train[[i]], main=paste("Histogram of", i), xlab=i)
}

par(mfrow=c(4,4))                                                      #on train data
for (i in names(data_test)[numerical_cols]) {
  hist(data_test[[i]], main=paste("Histogram of", i), xlab=i)
}

#Correlation_matrix
correlation_matrix <- cor(data_train[numerical_cols])
    # View the correlation matrix
print(correlation_matrix)

par(mar=c(4,4,2,2))


# plotting correlation matrix
corrplot(correlation_matrix,method = "circle",t1.cex=0.6,c1.cex=0.6)

#feature Engineering removing high correlated features
columns_drop_corr <- c("Total.day.charge","Total.eve.charge","Total.night.charge","Total.intl.charge")
data_train_corr <- data_temp[, !(names(data_train) %in%columns_drop_corr)]
data_test_corr <- data_test[, !(names(data_test) %in%columns_drop_corr)]

print(data_train_corr)


#Outlier:
find_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  return(column < lower_bound | column > upper_bound)
}
        # Apply the function to each numeric column
outliers_train<- sapply(data_train[numerical_cols], find_outliers)
outliers_test<- sapply(data_test[numerical_cols], find_outliers)
        # Optionally, sum the outliers in each column to get a count
outlier_counts_train<- colSums(outliers_train, na.rm = TRUE)
outlier_counts_test<- colSums(outliers_test, na.rm = TRUE)

print(outliers_train)
print(outliers_test)

print(outlier_counts_train)
print(outlier_counts_test)

#Removing Numeric Outliers:
replace_outliers_with_mean <- function(x) {
  if(is.numeric(x)) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    x[x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)] <- mean(x, na.rm = TRUE)
  }
  return(x)
}
  #for train    
  # Apply the function to each column
data_train[numerical_cols] <- data.frame(lapply(data_train[numerical_cols], replace_outliers_with_mean))
outliers_train<- sapply(data_train[numerical_cols], find_outliers)
outlier_counts_train<- colSums(outliers_train, na.rm = TRUE)
print(outliers_train)
print(outlier_counts_train)

  #for test
data_test[numerical_cols] <- data.frame(lapply(data_test[numerical_cols], replace_outliers_with_mean))
outliers_test<- sapply(data_test[numerical_cols], find_outliers)
outlier_counts_test<- colSums(outliers_test, na.rm = TRUE)
print(outliers_test)
print(outlier_counts_test)


# identifying the percentage of TRUE amd False in Churn
target_col <- 'Churn'

# Calculate the count of each category in the target column
target_count <- table(data_balanced[[target_col]])

# Calculate the percentage of each category
target_percentage <- prop.table(target_count) * 100

# Print the percentages
print(target_count)
print(target_percentage) #14-> 1 and 85->0

data_temp<-data_train
print(data_temp)

#saving the clean dataset
write.csv(data_temp, "Cleaned_Train1.csv", row.names = FALSE)
write.csv(data_test, "Cleaned_Test1.csv", row.names = FALSE)

#converting data format
data_temp$Account.length <- as.integer(data_temp$Account.length)
data_temp$Number.vmail.messages <- as.integer(data_temp$Number.vmail.messages)
data_temp$Total.day.calls <- as.integer(data_temp$Total.day.calls)
data_temp$Total.eve.calls <- as.integer(data_temp$Total.eve.calls)
data_temp$Total.night.calls <- as.integer(data_temp$Total.night.calls)
data_temp$Total.intl.calls <- as.integer(data_temp$Total.intl.calls)
data_temp$Customer.service.calls <- round(data_temp$Customer.service.calls)

#test Data

data_test$Account.length <- as.integer(data_test$Account.length)
data_test$Number.vmail.messages <- as.integer(data_test$Number.vmail.messages)
data_test$Total.day.calls <- as.integer(data_test$Total.day.calls)
data_test$Total.eve.calls <- as.integer(data_test$Total.eve.calls)
data_test$Total.night.calls <- as.integer(data_test$Total.night.calls)
data_test$Total.intl.calls <- as.integer(data_test$Total.intl.calls)
data_test$Customer.service.calls <- round(data_test$Customer.service.calls)


#balancing
# Separating the majority and minority classes
data_majority <- data_temp[data_temp$Churn == 0, ]
data_minority <- data_temp[data_temp$Churn == 1, ]

print(data_minority)
#undersampling
data_majority_undersampled <- data_majority[sample(nrow(data_majority), nrow(data_minority), replace = FALSE), ]

# Combining the undersampled majority class with the minority class
balanced_data <- rbind(data_majority_undersampled, data_minority)
dim(balanced_data)


#saving the clean dataset
write.csv(data_temp, "Cleaned_Train1.csv", row.names = FALSE)
write.csv(data_test, "Cleaned_Test1.csv", row.names = FALSE)

#saving the Balanced dataset
write.csv(balanced_data, "Balanced_Train1.csv", row.names = FALSE)

#saving the data after removing corelated feature
write.csv(data_train_corr, "Remove_corr_Train1.csv", row.names = FALSE)
write.csv(data_test_corr, "Remove_corr_Test1.csv", row.names = FALSE)


#----------------------------------------------------
#Loading the dataset train and test
clean_data_train= read.csv('Cleaned_Train1.csv')
head(clean_data_train)

#reading test data
clean_data_test = read.csv('Cleaned_Test1.csv')
print(head(clean_data_test))

print(clean_data_train)

#reading the dataset with no corelated feature
no_corr_train= read.csv('Remove_corr_Train1.csv')
head(no_corr_train)
no_corr_test = read.csv('Remove_corr_Test1.csv')
print(head(no_corr_test))

# Feature Scaling
scaled_train<-clean_data_train
scaled_test<-clean_data_test

no_corr_scaled_train<-no_corr_train
no_corr_scaled_test<-no_corr_test

scaled_train[, 1:4:5:6:7:8:9:10:11:12:13:14:15:16:17] = scale(clean_data_train[, 1:4:5:6:7:8:9:10:11:12:13:14:15:16:17])
scaled_test[, 1:4:5:6:7:8:9:10:11:12:13:14:15:16:17] = scale(clean_data_test[, 1:4:5:6:7:8:9:10:11:12:13:14:15:16:17])

print(scaled_train)
# Feature Scaling on No Corr features
no_corr_scaled_train[, 1:4:5:6:7:8:9:11:12:13] = scale(no_corr_train[,1:4:5:6:7:8:9:11:12:13])
no_corr_scaled_test[, 1:4:5:6:7:8:9:11:12:13] = scale(no_corr_test[, 1:4:5:6:7:8:9:11:12:13])
warnings()

# Fitting Logistic Regression to the Training set
classifier = glm(formula = Churn ~ .,
                 family = binomial,
                 data = scaled_train)

# Fitting Logistic Regression to the  No corr Training set 
classifier_nocorr = glm(formula = Churn ~ .,
                 family = binomial,
                 data = no_corr_scaled_train)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = scaled_test[-18])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Predicting the Test set results on no corr
prob_pred = predict(classifier_nocorr, type = 'response', newdata = no_corr_scaled_test[-14])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

print(y_pred)

library(pROC)
library(ggplot2)

# Making the Confusion Matrix
cm = table(scaled_test[, 18], y_pred)

# Making the Confusion Matrix for no corr
cm = table(no_corr_scaled_test[, 14], y_pred)

print(cm)

# Convert the confusion matrix to a data frame for ggplot
cm_df <- as.data.frame(as.table(cm))

# Plot the Confusion Matrix using ggplot2
cm_plot <- ggplot(cm_df, aes(x = Var1, y = y_pred, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title = "Confusion Matrix", x = "Predicted", y = "Actual")

print(cm_plot)

accuracy <- sum(diag(cm)) / sum(cm)
cat("Accuracy:", accuracy, "\n")#85.75

accuracy <- sum(diag(cm)) / sum(cm)
cat("Accuracy:", accuracy, "\n")#85.45



# ROC Curve
roc_obj <- roc(response = test_data$Churn, predictor = as.numeric(prob_pred))
plot(roc_obj, main="ROC Curve")
auc(roc_obj)#74.2

#-------------------------------------------
#Model Random Forest

library(randomForest)
library(caret)

# Splitting the dataset
train_x <- clean_data_train[, -which(names(clean_data_train) == "Churn")]
train_y <- clean_data_train$Churn
test_x <- clean_data_test[, -which(names(clean_data_test) == "Churn")]
test_y <- clean_data_test$Churn

head(train_x)

rf_model <- randomForest(x = train_x, y = train_y, ntree = 500)

predictions <- predict(rf_model, test_x)

print(predictions)

conf_matrix <- confusionMatrix(predictions, test_y)
print(conf_matrix)


#----------------------------------------------------

#Model 3 Descision Tree

library(rpart)
library(rpart.plot)
classifier = rpart(formula = Churn ~ .,
                   data = clean_data_train)

# Predicting the Test set results
y_pred = predict(classifier, newdata = clean_data_test[,-18], type = 'class')

# Making the Confusion Matrix
cm_dt = table(clean_data_test[, 18], y_pred)

print(cm_dt)

accuracy <- sum(diag(cm_dt)) / sum(cm_dt)
cat("Accuracy:", accuracy, "\n")


# Visualizing the Decision Tree
prp(classifier, type = 2, extra = 1, main = "Decision Tree Visualization")

#Checking with balannced target varaible

classifier_c = rpart(formula = Churn ~ .,
                   data = data_balanced)

y_pred_c = predict(classifier_c, newdata = clean_data_test[,-18], type = 'class')

print(y_pred_c)

# Making the Confusion Matrix
cm_dt_c = table(clean_data_test[, 18], y_pred_c)

print(cm_dt_c)
accuracy <- sum(diag(cm_dt_c)) / sum(cm_dt_c)
cat("Accuracy:", accuracy, "\n")


#------------------------------------------------------------
# model 4 XGBoost


library(xgboost)
library(readr)
library(dplyr)
library(caret)
library(e1071)
library(pROC)
library(ggplot2)

# Prepare matrices for xgboost original data
train_matrix <- xgb.DMatrix(data.matrix(clean_data_train[,-ncol(clean_data_train)]), label = clean_data_train$Churn)
test_matrix <- xgb.DMatrix(data.matrix(clean_data_test[,-ncol(clean_data_test)]), label = clean_data_test$Churn)

#Training on Balance Data 

train_matrix_b <- xgb.DMatrix(data.matrix(data_balanced[,-ncol(data_balanced)]), label = data_balanced$Churn)
test_matrix_b <- xgb.DMatrix(data.matrix(clean_data_test[,-ncol(clean_data_test)]), label = clean_data_test$Churn)

# Define parameters for XGBoost
params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eta = 0.3,
  gamma = 0,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 1,
  colsample_bytree = 1
)

# Train the modelTrain the model Accuracy92, precision 92, AUC 84.21
nrounds <- 100
model <- xgboost(params = params, data = train_matrix, nrounds = nrounds)

# Train the model with 500 rounds Train the model Accuracy92.65, precision 93.08, AUC 82.69
nrounds <- 500
model <- xgboost(params = params, data = train_matrix, nrounds = nrounds)

# Predictions with XGBoost
xgb_predictions <- predict(model, test_matrix)
xgb_predicted_labels <- ifelse(xgb_predictions > 0.5, 1, 0)

# Evaluate the XGBoost model
conf_matrix_xgb <- confusionMatrix(factor(xgb_predicted_labels), factor(test_data$Churn))
print(conf_matrix_xgb)

# Calculate Accuracy for XGBoost
accuracy_xgb <- sum(xgb_predicted_labels == test_data$Churn) / length(test_data$Churn)
print(paste("XGBoost Accuracy:", accuracy_xgb))

# ROC Curve and AUC
roc_curve <- roc(factor(test_data$Churn), xgb_predictions)
auc_value <- auc(roc_curve)
plot(roc_curve, main = "ROC Curve")
cat("AUC: ", auc_value, "\n")

# Precision
precision_value <- conf_matrix_xgb$byClass['Pos Pred Value']
cat("Precision: ", precision_value, "\n")

# Plotting Confusion Matrix using ggplot2
conf_matrix_plot <- as.data.frame(conf_matrix_xgb$table)
colnames(conf_matrix_plot) <- c("Reference", "Prediction", "Frequency")
ggplot(conf_matrix_plot, aes(x = Reference, y = Prediction, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%1.0f", Frequency)), vjust = 1) +
  theme_minimal() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion Matrix", x = "Actual label", y = "Predicted label")


#--------------------------------------------------------------------------
#Model 5 Gradient Boosting
library(gbm)
library(readr)
library(caret)

# Splitting the dataset
train_x <- clean_data_train[, -which(names(clean_data_train) == "Churn")]
train_y <- clean_data_train$Churn

test_x <- clean_data_test[, -which(names(clean_data_test) == "Churn")]
test_y <- clean_data_test$Churn

# Training the Gradient Boosting model
gbm_model <- gbm(Churn ~ ., data = clean_data_train, distribution = "bernoulli", n.trees = 100, interaction.depth = 1)

# Making predictions Accuracy=88.45, precision=89.09, Roc=81.02, balanced accuracy=62.98%
predictions_gb <- predict(gbm_model, test_x, n.trees = 100, type = "response")
predicted_labels_gb <- ifelse(predictions > 0.5, 1, 0)

# Confusion Matrix
conf_matrix_gb <- confusionMatrix(factor(predicted_labels_gb), factor(test_y))
print(conf_matrix_gb)

# Calculate Accuracy
accuracy <- sum(predicted_labels == test_y) / length(test_y)
print(paste("Accuracy:", accuracy))

# Precision Calculation
precision_value <- conf_matrix$byClass['Pos Pred Value']
print(paste("Precision:", precision_value))

# ROC Curve and AUC
roc_curve <- roc(factor(test_y), predictions)
auc_value <- auc(roc_curve)
plot(roc_curve, main = "ROC Curve")
cat("AUC: ", auc_value, "\n")

# Plotting Confusion Matrix using ggplot2
conf_matrix_melted <- as.data.frame(as.table(conf_matrix$table))
colnames(conf_matrix_melted) <- c("Reference", "Prediction", "Frequency")
ggplot(data = conf_matrix_melted, aes(x = Reference, y = Prediction, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix for GBM Model")


#-----------------------------------------------

#model 6
library(class)
library(readr)
# Load the datasets
clean_data_train <- read_csv("Cleaned_Train1.csv")
clean_data_test <- read_csv("Cleaned_Test1.csv")

#loading the balance data
balanced_train <- read_csv("Balanced_Train1.csv")


# For KNN, we often need to scale/normalize the data
# Assuming the data is already in a suitable format based on the summary

#min max scaling
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Applying normalization to each column in your data frame acc=87.0 7,precision=88.5
normalized_data <- as.data.frame(lapply(clean_data_train, normalize))
normalized_data_test <- as.data.frame(lapply(clean_data_test, normalize))

#Normalizing balanced dataset Acc=70.01, precision=0.93
normalized_data_balanced <- as.data.frame(lapply(balanced_train, normalize))

# Splitting the dataset into features and target variable
train_x <- normalized_data[, -which(names(normalized_data) == "Churn")]
train_y <- normalized_data$Churn
test_x <- normalized_data_test[, -which(names(normalized_data_test) == "Churn")]
test_y <- normalized_data_test$Churn

# Splitting the balanced dataset into features and target variable
train_x_knnb <- normalized_data_balanced[, -which(names(normalized_data_balanced) == "Churn")]
train_y_knnb <- normalized_data_balanced$Churn

# Running the KNN model
# Choose a value for k (number of neighbors)
k <- 5
knn_pred <- knn(train = train_x, test = test_x, cl = train_y, k = k)

# Running the KNN model for balanced dataset
# Choose a value for k (number of neighbors)
k <- 5
knn_pred_balance <- knn(train = train_x_knnb, test = test_x, cl = train_y_knnb, k = k)

# Confusion Matrix
conf_matrix <- table(test_y, knn_pred)
print(conf_matrix)

#accuracy
accuracy <- sum(knn_pred == test_y) / length(test_y)
print(paste("Accuracy:", accuracy))

#accuracy for balance
accuracy <- sum(knn_pred_balance == test_y) / length(test_y)
print(paste("Accuracy:", accuracy))


# Plotting Confusion Matrix using ggplot2
conf_matrix_melted <- as.data.frame(as.table(conf_matrix))
colnames(conf_matrix_melted) <- c("Reference", "Prediction", "Frequency")
ggplot(data = conf_matrix_melted, aes(x = Reference, y = Prediction, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix for KNN Model")

# Precision Calculation
precision_value <- confusionMatrix(as.factor(knn_pred), as.factor(test_y))$byClass['Pos Pred Value']
print(paste("Precision:", precision_value))


#-------------------------------------------------------------















