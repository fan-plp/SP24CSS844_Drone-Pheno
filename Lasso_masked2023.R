#load packages needed
library(readxl)
library(randomForest)
library(datasets)
library(caret)
library(dplyr)
library(glmnet)
library(ggplot2)
library(car)
#load file

masked2023<-read_xlsx("2023_phenos_and_maskedNDVI.xlsx")
#NA not permitted in predictors, so drop a few column, which contains NA and others 
masked2023<- select(masked2023, -1,-2, -5, -6, -9, -10, -12, -13, -14, -15, -18, -21, -24, -27, -30, -33, -36, -39, -42, -45 )
#a few rows contains NA value, if discard we can use code below

summary(masked2023)
masked2023$Anthesis_DAP <- as.numeric(masked2023$Anthesis_DAP)
masked2023$Silking_DAP <- as.numeric(masked2023$Silking_DAP)
masked2023$spadavg <- as.numeric(masked2023$spadavg)
#seperating training and testing data set
set.seed(123)
ind <- sample(2, nrow(masked2023), replace = TRUE, prob = c(0.75, 0.25))
train <- masked2023[ind==1,]
test <- masked2023[ind==2,]
# 598 data points for training and 202 data points for testing

# try LASSO #

#remove rows containing NA as glmnet does not deal with missing data
train <- train %>% na.omit()
test <- test %>%  na.omit()

#creating data matrix
y <- data.matrix(train$Yield)
x.sum <- data.matrix(train[, c( "Anthesis_DAP", "Silking_DAP","StandCount1", "spadavg", "062123_sum","062823_sum", "070723_sum", "071223_sum","072123_sum","080823_sum","082223_sum", "083123_sum", "091423_sum", "092523_sum")])

#remove "Anthesis_DAP", "Silking_DAP", "spadavg",
#x.sum <- data.matrix(train[, c("StandCount1",  "062123_sum","062823_sum", "070723_sum", "071223_sum","072123_sum","080823_sum","082223_sum", "083123_sum", "091423_sum", "092523_sum")])

#####not really sure if these are needed
#testing multicolinearity of the predictors
correlation <- cor(x.sum)
correlation.df <- print(correlation) #High correlations (close to -1 or 1) might indicate multicollinearity.
x.sum.df <- as.data.frame(x.sum)
y.df <- as.data.frame(y)
x.sum.df <- merge(x.sum.df, y.df)
lm.model<- lm(V1 ~., x.sum.df)
vif_values <- vif(lm.model)
print(vif_values) #a VIF greater than 5 or 10 indicates significant multicollinearity.
results <- prcomp(x.sum, scale = TRUE) # calculate rinciple component 
results$rotation <- -1*results$rotation
results$rotation
results$x <- -1*results$x
#ggplot2::autoplot(stats::prcomp(x.sum, scale=TRUE), label = FALSE, loadings.label = TRUE)
results$sdev^2 / sum(results$sdev^2)# Find Variance Explained by Each Principal Component
var_explained = results$sdev^2 / sum(results$sdev^2)
#if remove flowering and spad, change 14 to 11
qplot(c(1:14), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


#try scale the predictor
x.sum <- scale(x = x.sum, center = T, scale = T)
sum_model <- cv.glmnet(y = y, x = x.sum, alpha = 1)
best_lambda_sum <- sum_model$lambda.min
plot(sum_model)

best_model_sum <- glmnet(x.sum, y, alpha = 1, lamda = best_lambda_sum)
coef(best_model_sum)

#use fitted model to make preictions
#REMOVE "Anthesis_DAP", "Silking_DAP","spadavg",
#test.x <- data.matrix(test[, c( "StandCount1",  "062123_sum","062823_sum", "070723_sum", "071223_sum","072123_sum","080823_sum","082223_sum", "083123_sum", "091423_sum", "092523_sum")])
test.x <- data.matrix(test[, c( "Anthesis_DAP", "Silking_DAP","StandCount1", "spadavg", "062123_sum","062823_sum", "070723_sum", "071223_sum","072123_sum","080823_sum","082223_sum", "083123_sum", "091423_sum", "092523_sum")])
test.x <- scale(x = test.x, center = T, scale = T)
test.y <- data.matrix(test$Yield)
y_predicted <- predict(best_model_sum, s = best_lambda_sum, newx =test.x)
test.df <- cbind(test.y,y_predicted)
test.df <- as.data.frame(test.df)
rmse <- sqrt(mean((y_predicted - test.y)^2))
print(paste("RMSE:", rmse)) # removal of flowering and spad RMSE = 32.7500137827302
# "RMSE: 31.8946027549686"

masked2023.sum.plot<- ggplot(data = test.df, aes(x = test.y, y = y_predicted)) +
  geom_point() + theme_bw() +
  geom_smooth(method = 'lm', col = 'blue') +
  labs(title = "Masked 2023 Actual vs Predicted Yields", x = "Actual Yield", y = "Predicted Yield")
masked2023.sum.plot + geom_label(label="RMSE = 31.8946", x=50,  y=205, label.size = 0.35, color = "red")

####################for mean masked 2023
y <- data.matrix(train$Yield)
x.mean<- data.matrix(train[, c("Anthesis_DAP", "Silking_DAP", "StandCount1", "spadavg", "062123_mean","062823_mean", "070723_mean", "071223_mean","072123_mean","080823_mean","082223_mean", "083123_mean", "091423_mean", "092523_mean")])


#####not really sure if these are needed
#testing multicolinearity of the predictors
correlation.mean <- cor(x.mean)
correlation.mean.df <- print(correlation.mean) #High correlations (close to -1 or 1) might indicate multicollinearity.
x.mean.df <- as.data.frame(x.mean)
y.mean.df <- as.data.frame(y)
x.mean.df <- merge(x.mean.df, y.mean.df)
mean.lm.model<- lm(V1 ~., x.mean.df)
mean_vif_values <- vif(mean.lm.model)
print(mean_vif_values) #a VIF greater than 5 or 10 indicates significant multicollinearity.
mean.results <- prcomp(x.mean, scale = TRUE) # calculate rinciple component 
mean.results$rotation <- -1*mean.results$rotation
mean.results$rotation
mean.results$x <- -1*mean.results$x
#ggplot2::autoplot(stats::prcomp(x.mean, scale=TRUE), label = FALSE, loadings.label = TRUE)
mean.results$sdev^2 / sum(mean.results$sdev^2)# Find Variance Explained by Each Principal Component
mean.var_explained = mean.results$sdev^2 / sum(mean.results$sdev^2)
qplot(c(1:14), mean.var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


#try scale the predictor
x.mean <- scale(x = x.mean, center = T, scale = T)
mean_model <- cv.glmnet(y = y, x = x.mean, alpha = 1)
best_lambda_mean <- mean_model$lambda.min
plot(mean_model)

best_model_mean <- glmnet(x.mean, y, alpha = 1, lamda = best_lambda_mean)
coef(best_model_mean)

#use fitted model to make preictions
test.x <- data.matrix(test[, c("Anthesis_DAP", "Silking_DAP", "StandCount1", "spadavg", "062123_mean","062823_mean", "070723_mean", "071223_mean","072123_mean","080823_mean","082223_mean", "083123_mean", "091423_mean", "092523_mean")])
test.x <- scale(x = test.x, center = T, scale = T)
test.y <- data.matrix(test$Yield)
y_predicted <- predict(best_model_mean, s = best_lambda_mean, newx =test.x)
test.df <- cbind(test.y,y_predicted)
test.df <- as.data.frame(test.df)
rmse <- sqrt(mean((y_predicted - test.y)^2))
print(paste("RMSE:", rmse)) # 

masked2023.mean.plot<-ggplot(data = test.df, aes(x = test.y, y = y_predicted)) +
  geom_point() + theme_bw() +
  geom_smooth(method = 'lm', col = 'blue') +
  labs(title = "Masked 2023 Actual vs Predicted Yields", x = "Actual Yield", y = "Predicted Yield")

masked2023.mean.plot + geom_label(label="RMSE = 30.8307", x=50,  y=180, label.size = 0.35, color = "red")



#######other fun code 
# try random forest #

classifier_RF = randomForest(x = train[-5], 
                             y = train$Yield, 
                             ntree = 500)
classifier_RF

# Predicting the Test set results 
y_pred = predict(classifier_RF, newdata = test[-5]) 

# Confusion Matrix 
confusion_mtx = table(test[, 5], y_pred) 
confusion_mtx 

# Plotting model 
plot(classifier_RF) 

# Importance plot 
importance(classifier_RF) 

# Variable importance plot 
varImpPlot(classifier_RF) 
