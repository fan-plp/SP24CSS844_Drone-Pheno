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

unmasked2022<-read_xlsx("2022_phenos_and_unmaskedNDVI.xlsx")
#NA not permitted in predictors, so drop a few column, which contains NA and others 
unmasked2022<- select(unmasked2022, -1,-2, -5, -6, -9, -10, -13, -14, -15, -18, -21, -24, -27, -30, -33, -36, -39, -42, -45, -48 )
#a few rows contains NA value, if discard we can use code below

summary(unmasked2022)
unmasked2022$Anthesis_DAP <- as.numeric(unmasked2022$Anthesis_DAP)
unmasked2022$Silking_DAP <- as.numeric(unmasked2022$Silking_DAP)
unmasked2022$spadavg <- as.numeric(unmasked2022$spadavg)
#seperating training and testing data set
set.seed(123)
ind <- sample(2, nrow(unmasked2022), replace = TRUE, prob = c(0.75, 0.25))
train <- unmasked2022[ind==1,]
test <- unmasked2022[ind==2,]
# 598 data points for training and 202 data points for testing

# try LASSO #

#remove rows containing NA as glmnet does not deal with missing data
train <- train %>% na.omit()
test <- test %>%  na.omit()

#creating data matrix
y <- data.matrix(train$Yield)
x.sum <- data.matrix(train[, c( "Anthesis_DAP", "Silking_DAP","StandCount1", "StandCount2", "spadavg", "060922_sum","061522_sum","062222_sum", "062922_sum","071422_sum", "071922_sum","072922_sum","080522_sum","081122_sum", "081722_sum", "083122_sum")])

#remove "Anthesis_DAP", "Silking_DAP", "spadavg",
#x.sum <- data.matrix(train[, c("StandCount1", "StandCount2", "060922_sum","061522_sum","062222_sum", "062922_sum","071422_sum", "071922_sum","072922_sum","080522_sum","081122_sum", "081722_sum", "083122_sum")])

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
#if remove flowering and spad, change 16 to 13
qplot(c(1:16), var_explained) + 
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
#test.x <- data.matrix(test[, c( "StandCount1", "StandCount2", "060922_sum","061522_sum","062222_sum", "062922_sum","071422_sum", "071922_sum","072922_sum","080522_sum","081122_sum", "081722_sum", "083122_sum")])
test.x <- data.matrix(test[, c( "Anthesis_DAP", "Silking_DAP","StandCount1", "StandCount2", "spadavg", "060922_sum","061522_sum","062222_sum", "062922_sum","071422_sum", "071922_sum","072922_sum","080522_sum","081122_sum", "081722_sum", "083122_sum")])
test.x <- scale(x = test.x, center = T, scale = T)
test.y <- data.matrix(test$Yield)
y_predicted <- predict(best_model_sum, s = best_lambda_sum, newx =test.x)
test.df <- cbind(test.y,y_predicted)
test.df <- as.data.frame(test.df)
rmse <- sqrt(mean((y_predicted - test.y)^2))
print(paste("RMSE:", rmse)) # removal of flowering and spad RMSE = ?
# "RMSE: 33.3063650088517"

unmasked2022.sum.plot<- ggplot(data = test.df, aes(x = test.y, y = y_predicted)) +
  geom_point() + theme_bw() +
  geom_smooth(method = 'lm', col = 'blue') +
  labs(title = "Unmasked 2022 Actual vs Predicted Yields", x = "Actual Yield", y = "Predicted Yield")
unmasked2022.sum.plot + geom_label(label="RMSE = 33.3064", x=75,  y=175, label.size = 0.35, color = "red")+ylim(50, 200)

####################for mean unmasked 2022
y <- data.matrix(train$Yield)
x.mean<- data.matrix(train[, c("Anthesis_DAP", "Silking_DAP","StandCount1", "StandCount2", "spadavg", "060922_mean","061522_mean","062222_mean", "062922_mean","071422_mean", "071922_mean","072922_mean","080522_mean","081122_mean", "081722_mean", "083122_mean")])


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
qplot(c(1:16), mean.var_explained) + 
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
test.x <- data.matrix(test[, c("Anthesis_DAP", "Silking_DAP","StandCount1", "StandCount2", "spadavg", "060922_mean","061522_mean","062222_mean", "062922_mean","071422_mean", "071922_mean","072922_mean","080522_mean","081122_mean", "081722_mean", "083122_mean")])
test.x <- scale(x = test.x, center = T, scale = T)
test.y <- data.matrix(test$Yield)
y_predicted <- predict(best_model_mean, s = best_lambda_mean, newx =test.x)
test.df <- cbind(test.y,y_predicted)
test.df <- as.data.frame(test.df)
rmse <- sqrt(mean((y_predicted - test.y)^2))
print(paste("RMSE:", rmse)) # RMSE: 33.0298

unmasked2022.mean.plot<-ggplot(data = test.df, aes(x = test.y, y = y_predicted)) +
  geom_point() + theme_bw() +
  geom_smooth(method = 'lm', col = 'blue') +
  labs(title = "unmasked 2022 Actual vs Predicted Yields", x = "Actual Yield", y = "Predicted Yield")

unmasked2022.mean.plot + geom_label(label="RMSE = 33.0298", x=75,  y=180, label.size = 0.35, color = "red")+ylim(50,200)



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
