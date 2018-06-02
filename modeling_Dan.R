### Build models to fit the data and make predictions.
### 
### Models:
###   Linear Regression
###   Linear Regression with principal component analysis
###   Ridge Regression
###   Foward stepwise regression
###   Lasso Regression
###   Random Forest
###   Gradient Boosting Trees
###

rm(list = ls())

 
library(Metrics) 
library(caret)
library(ranger) 
library(gbm)
library(ada)
library(ggplot2)
library(som) 
library(e1071)

############### load data and create trainset/testset ###############
dataset <- read.csv( '/Users/victorjs/desktop/insight/boston.csv')
dataset <- subset(dataset, 
                  select = -c( Apt.or.House, Utilities.included))


set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
data(dataset)

############### calculate correlation matrix##################
correlationMatrix <- cor(dataset)
# summarize the correlation matrix
hc = findCorrelation(correlationMatrix, cutoff=0.9) # putt any value as a "cutoff" 
hc = sort(hc)
reduced_Data = dataset[,-c(hc)]
dataset <- reduced_Data

###################split data to train and test##############
set.seed(10)
index <- sample(1:dim(dataset)[1], floor(0.6 * dim(dataset)[1]))
dataset <- as.data.frame(normalize(dataset, byrow=FALSE))
train <- dataset[index, ]
test <- dataset[-index, ]


############### use forward selection to find important features ###############
null <- lm(Rent ~ 1, data = train)
full <- lm(Rent ~ ., data = train)
fit_lm_forward <- step(null, 
                       scope     = list(lower = null, upper = full), 
                       direction = 'forward')
summary(fit_lm_forward)
pred_lm_forward <- predict(fit_lm_forward, newdata = test)

coe <- fit_lm_forward$coefficients


############### baseline linear regression ############### 
dataset <- read.csv( '/Users/victorjs/desktop/insight/boston.csv')
dataset <- subset(dataset, 
                  select = -c( Apt.or.House, Utilities.included))
set.seed(10)
index <- sample(1:dim(dataset)[1], floor(0.6 * dim(dataset)[1]))
train <- dataset[index, ]
test <- dataset[-index, ]

fit_lm <- lm(formula = Rent ~ SqFt + middlesex_euc + house + pop + median_family_in + 
               lowell_euc + Longitude + outlet_euc + quincy_euc + utility_no + 
               bc_euc + lynn_euc, data = train)
pred_lm <- predict(fit_lm, newdata = test)

rmse(pred_lm, test$Rent)




############### tree based methods ###############
dataset <- read.csv( '/Users/victorjs/desktop/insight/boston.csv')
dataset <- subset(dataset, 
                  select = -c( Apt.or.House, Utilities.included))
set.seed(10)
index <- sample(1:dim(dataset)[1], floor(0.6 * dim(dataset)[1]))
train <- dataset[index, ]
test <- dataset[-index, ]


############### random forest ###############
# use 5-fold cv to find best hyperparameters for the model
# ctrl = trainControl(method = "cv", 
#                     number = 5)
# grid_rf = expand.grid(mtry = c(50, 60, 70), splitrule = 'variance')
# fit_rf = train(Rent ~ .,
#                data = train,
#                method = "ranger",
#                # tuneGrid = grid_rf,
#                tuneLength = 5,
#                trControl = ctrl,
#                metric = 'RMSE')

# fit the model with best parameters
fit_rf_best <- ranger(Rent ~., 
                      data         = train, 
                      mtry         = 13, 
                      splitrule    = 'variance',
                      num.trees    = 500,
                      write.forest = TRUE, 
                      importance   = 'impurity')

pred_rf <- predict(fit_rf_best, data = test)
rmse(pred_rf$predictions, test$Rent)

# calculate feature importance
# calculate feature importance
feature_im <- importance(fit_rf_best)
feature_im <- feature_im[order(feature_im, decreasing = TRUE)]
barplot(height = feature_im[1:6] / 10^8,
        names.arg = names(feature_im)[1:6],las=2,main="Feature importance (Random Forest)",ylab="Importance(/10^8)")
################bagging#######################
fit_bagging <- bagging(Rent ~., 
                      data         = train, 
                      mtry         = 13, 
                      splitrule    = 'variance',
                      num.trees    = 500,
                      write.forest = TRUE, 
                      importance   = 'impurity')

pred_bagging <- predict(fit_bagging, data = test)
rmse(pred_bagging, test$Rent)


############### gradient boosting trees ###############
# use 5-fold cv to find best hyperparameters for the model
# ctrl = trainControl(method = "cv", 
#                     number = 5)
# grid_gbm = expand.grid(n.trees = 700, 
#                        interaction.depth = 7, 
#                        shrinkage = 0.1,
#                        n.minobsinnode = 5)
# fit_gbm = train(Rent ~ .,
#                 data = train,
#                 method = "gbm",
#                 tuneGrid = grid_gbm,
#                 # tuneLength = 5,
#                 trControl = ctrl,
#                 metric = 'RMSE')

# fit the model with best parameters
fit_gbm <- gbm(Rent ~., data     = train,
               distribution      = 'gaussian',
               n.trees           = 700, 
               interaction.depth = 7, 
               shrinkage         = 0.1,
               n.minobsinnode    = 5)

pred_gbm <- predict(fit_gbm, 
                    newdata = test, 
                    n.trees = 700)

rmse(test$Rent, pred_gbm)

feature_im <- varImp(fit_gbm, numTrees = 700)
fea <- data.frame(names = row.names(feature_im), vals = feature_im$Overall)
fea <- fea[order(fea$vals, decreasing = TRUE), ]
barplot(height = fea[1:6,2] / 10^8,
        names.arg = fea[1:6,1],las=2,main="Feature importance (Boosting)",ylab="Importance(/10^8)")

tree_methods <- c(rmse(test$Rent, pred_rf$predictions), rmse(test$Rent, pred_gbm))


############### SVM ##################
fit_svm <- svm(Rent ~., data= train,
               kernel = "polynomial", 
               degree = 3, gamma = 0.1
               )

pred_svm <- predict(fit_svm, 
                    newdata = test
                    )

rmse(test$Rent, pred_svm)

##############save prediction###########
test$pred.lm <- pred_lm_forward 
test$Actual.rent <- test$Rent
test$pred.rf <- pred_rf
test$pred.B <- pred_bagging
test$pred.boost <- pred_gbm 
test$pred.svm <- pred_svm 
saveRDS(test,"cdata.rds")
############### visualize results for different models ###############
#data visulization
library(ggplot2)
cdata <- readRDS("/Users/victorjs/desktop/insight/cdata.rds")
#First we plot a basic plot for rent by Square Footage
p <- ggplot(data=cdata, aes(y=Actual.rent, x=pred.RT))
p + geom_point() +
  xlab("Actual Rent") +
  ylab("Predict Rent") +
  xlim(0,10000) +
  ylim(0,15000) +
  ggtitle("Prediction(Regression Tree)")+
  geom_abline(intercept = 0, slope = 1) +
  theme(plot.title=element_text(size=16,hjust = 0.5),
        axis.text=element_text(size=10))

p <- ggplot(data=cdata, aes(x=pred.RT, y=-pred.RT+Actual.rent))
p + geom_point() +
  xlab("Predicted Rent") +
  ylab("Residual") +ylim(-5000,10000)+xlim(0,8000)+
  ggtitle("Regression Tree")+
  geom_abline(intercept = 0, slope = 0) +
  theme(plot.title=element_text(size=16,hjust = 0.5),
        axis.text=element_text(size=10))


p <- ggplot(data=cdata, aes(y=Actual.rent, x=pred.B))
p + geom_point() +
  xlab("Actual Rent") +
  ylab("Predict Rent") +
  xlim(0,10000) +
  ylim(0,15000) +
  ggtitle("Prediction(Bagging)")+
  geom_abline(intercept = 0, slope = 1) +
  theme(plot.title=element_text(size=16,hjust = 0.5), 
        axis.text=element_text(size=14))

p <- ggplot(data=cdata, aes(x=pred.B, y=-pred.B+Actual.rent))
p + geom_point() +
  xlab("Predicted Rent") +
  ylab("Residual") +ylim(-5000,10000)+xlim(0,8000)+
  ggtitle("Bagging")+
  geom_abline(intercept = 0, slope = 0) +
  theme(plot.title=element_text(size=16,hjust = 0.5),
        axis.text=element_text(size=10))



p <- ggplot(data=cdata, aes(y=Actual.rent, x=pred.rf))
p + geom_point() +
  xlab("Actual Rent") +
  ylab("Predict Rent") +
  xlim(0,10000) +
  ylim(0,15000) +ggtitle("Prediction(Random Forest)")+
  geom_abline(intercept = 0, slope = 1) +
  theme(plot.title=element_text(size=16,hjust = 0.5),
        axis.text=element_text(size=14))
p <- ggplot(data=cdata, aes(x=pred.rf, y=-pred.rf+Actual.rent))
p + geom_point() +
  xlab("Predicted Rent") +
  ylab("Residual") +ylim(-5000,10000)+xlim(0,8000) +ggtitle("Random Forest")+
  geom_abline(intercept = 0, slope = 0) +
  theme(plot.title=element_text(size=16,hjust = 0.5),
        axis.text=element_text(size=10))



p <- ggplot(data=cdata, aes(y=Actual.rent, x=pred.boost))
p + geom_point() +
  xlab("Actual Rent") +
  ylab("Predict Rent") +
  xlim(0,10000) +
  ylim(0,15000) +ggtitle("Prediction(Boosting)")+
  geom_abline(intercept = 0, slope = 1) +
  theme(plot.title=element_text(size=16,hjust = 0.5),
        axis.text=element_text(size=14))
p <- ggplot(data=cdata, aes(x=pred.boost, y=-pred.boost+Actual.rent))
p + geom_point() +
  xlab("Predicted Rent") +
  ylab("Residual") +ylim(-5000,10000)+xlim(0,8000) +ggtitle("Boosting")+
  geom_abline(intercept = 0, slope = 0) +
  theme(plot.title=element_text(size=16,hjust = 0.5),
        axis.text=element_text(size=10))




p <- ggplot(data=cdata, aes(y=Actual.rent, x=pred.lm))
p + geom_point() +
  xlab("Actual Rent") +
  ylab("Predict Rent") +ggtitle("   Prediction(Linear Regression)")+
  xlim(0,10000) +
  ylim(0,15000) +
  geom_abline(intercept = 0, slope = 1) +
  theme(plot.title=element_text(size=16,hjust = 0.5),
        axis.text=element_text(size=14))
p <- ggplot(data=cdata, aes(x=pred.lm, y=-pred.lm+Actual.rent))
p + geom_point() +
  xlab("Predicted Rent") +
  ylab("Residual") +ylim(-5000,10000)+xlim(0,8000) +ggtitle("   Linear Regression")+
  geom_abline(intercept = 0, slope = 0) +
  theme(plot.title=element_text(size=16,hjust = 0.5),
        axis.text=element_text(size=10))

p <- ggplot(data=test, aes(y=Actual.rent, x=pred.svm))
p + geom_point() +
  xlab("Actual Rent") +
  ylab("Predict Rent") +
  xlim(0,10000) +
  ylim(0,15000) +ggtitle("   Prediction(SVM)")+
  geom_abline(intercept = 0, slope = 1) +
  theme(plot.title=element_text(size=16,hjust = 0.5),
        axis.text=element_text(size=14))
p <- ggplot(data=test, aes(x=pred.svm, y=-pred.svm+Actual.rent))
p + geom_point() +
  xlab("Predicted Rent") +
  ylab("Residual") +ylim(-5000,10000)+xlim(0,8000) +
  geom_abline(intercept = 0, slope = 0) +ggtitle("   SVM")+
  theme(plot.title=element_text(size=16,hjust = 0.5),
        axis.text=element_text(size=10))

