#April 14th
#wilder data (more variable)
#SVM
#regression tree
#text informative? word in decription
#increasing covar...
#introduction to statical learning, james
#cross validation

#na.omit

#sqrt>0

#str

#package(caret)

#square root 

#residual >>0

 

library (tree)
library (MASS)
data<-Boston.data[,c(2:9,11,12)]
cdata<-na.omit(Boston.data)
cdata <- cdata[cdata$SqFt >0,]
split=sample(1: nrow(cdata), nrow(cdata))

# mean=c(0,0,0,0,0)
# for (i in 1:5)
# {
#   train=cdata[-split[578*(i-1)+1:(578*i)],]
#   
#   yhat=predict (   ,newdata =cdata [split[578*(i-1)+1:(578*i)],])
#   data.test=cdata [split[578*(i-1)+1:(578*i)] ,"Rent"]
#   mean[i]=((yhat -data.test)^2)
#   }

set.seed (1)
#train = sample (1: nrow(cdata ), nrow(cdata)/2)
tree.data =tree(Rent ~SqFt+Apt.or.House+Utilities.included+Latitude+Longitude+num.bedrooms+state,cdata)
summary (tree.data )
plot(tree.data)
text(tree.data,pretty =0)
cv.data =cv.tree(tree.data)
plot(cv.data$size ,cv.data$dev ,type='b')
# yhat=predict (tree.data, newdata =cdata)
# cdata$pred.RT<-yhat


set.seed (1)
m1=c(0,0,0,0,0)
for (i in 1:5)
{
  train=cdata[-split[(578*(i-1)+1):(578*i)],]
  tree.data =tree(Rent ~SqFt+Apt.or.House+Utilities.included+Latitude+Longitude+num.bedrooms+state,data=train)
  yhat=predict (tree.data, newdata =cdata [split[(578*(i-1)+1):(578*i)],])
  cdata[split[(578*(i-1)+1):(578*i)],]$pred.RT<-yhat
  data.test=cdata [split[(578*(i-1)+1):(578*i)] ,"Rent"]
  m1[i]=mean((yhat -data.test)^2)
  }
mean(sqrt(m1))

# yhat=predict (tree.boston ,newdata =Boston [-train ,])
# boston .test=Boston [-train ," medv"]
# plot(yhat ,boston .test)
# abline (0,1)
# mean((yhat -boston .test)^2)

# library(party)
# TreeModel=ctree(Rent ~SqFt+Apt.or.House+Utilities.included+Latitude+Longitude+num.bedrooms,data=cdata)
# plot(TreeModel,type = "simple")

# library(rpart)
# data.tree = rpart(Rent ~ SqFt+Apt.or.House+Utilities.included+Latitude+Longitude+num.bedrooms,data=cdata, cp = 10^(-6))
# data.tree$cptable[1:10, ]
# data.tree$cptable[dim(data.tree$cptable)[1] - 9:0, ]
# cp9 = which(data.tree$cptable[, 2] == 9)
# data.tree9 = prune(data.tree, data.tree$cptable[cp9, 1])
# print(data.tree9)
# summary(data.tree9)
# png("datatree9.png", width = 1200, height = 800)
# post(data.tree9, file = "", title. = "Test, 9 splits",
#      bp = 18)
# dev.off()
# which.min(data.tree$cptable[, 4])
# cpstat = dim(cdata)[1] * data.tree$cptable[, 3] + 2 * (data.tree$cptable[,2] + 1)
# round(data.tree$cptable[which.min(cpstat), ], 3)


library (randomForest)
set.seed (1)
bag.data =randomForest(Rent ~ SqFt+Apt.or.House+Utilities.included+Latitude+Longitude+num.bedrooms+state,data=cdata, mtry=7, importance =TRUE)
bag.data
importance(bag.data)
# yhat=predict (bag.data, newdata =cdata)
# cdata$pred.B<-yhat


set.seed (1)
m2=c(0,0,0,0,0)
for (i in 1:5)
{
  train=cdata[-split[(578*(i-1)+1):(578*i)],]
  bag.data =randomForest(Rent ~ SqFt+Apt.or.House+Utilities.included+Latitude+Longitude+num.bedrooms+state,data=train, mtry=7, importance =TRUE)
  yhat=predict (bag.data, newdata =cdata [split[(578*(i-1)+1):(578*i)],])
  cdata[split[(578*(i-1)+1):(578*i)],]$pred.B<-yhat
  data.test=cdata [split[(578*(i-1)+1):(578*i)] ,"Rent"]
  m2[i]=mean((yhat -data.test)^2)
}
mean(sqrt(m2))

set.seed (1)
rf.data =randomForest(Rent ~ SqFt+Apt.or.House+Utilities.included+Latitude+Longitude+num.bedrooms+state,data=cdata, mtry=4, importance =TRUE)
rf.data
importance(rf.data)
# yhat=predict (rf.data, newdata =cdata)
# cdata$pred.rf<-yhat

set.seed (1)
m3=c(0,0,0,0,0)
for (i in 1:5)
{
  train=cdata[-split[(578*(i-1)+1):(578*i)],]
  rf.data =randomForest(Rent ~ SqFt+Apt.or.House+Utilities.included+Latitude+Longitude+num.bedrooms+state,data=train, mtry=4, importance =TRUE)
  yhat=predict (rf.data, newdata =cdata [split[(578*(i-1)+1):(578*i)],])
  cdata[split[(578*(i-1)+1):(578*i)],]$pred.rf<-yhat
  data.test=cdata [split[(578*(i-1)+1):(578*i)] ,"Rent"]
  m3[i]=mean((yhat -data.test)^2)
}
mean(sqrt(m3))


library (gbm)
set.seed (1)
boost.data =gbm(Rent ~ SqFt+Apt.or.House+Utilities.included+Latitude+Longitude+num.bedrooms+state,data=cdata, distribution="gaussian", n.trees =5000, interaction.depth =5, shrinkage =0.1, verbose =F)
summary (boost.data)
par(mfrow =c(1,3))
plot(boost.data ,i="Latitude")
plot(boost.data ,i="SqFt")
plot(boost.data ,i="Longitude")
# yhat=predict (boost.data, newdata =cdata, n.trees =5000)
# cdata$pred.boost<-yhat

set.seed (1)
m4=c(0,0,0,0,0)
for (i in 1:5)
{
  train=cdata[-split[(578*(i-1)+1):(578*i)],]
  boost.data =gbm(Rent ~ SqFt+Apt.or.House+Utilities.included+Latitude+Longitude+num.bedrooms+state,data=train, distribution="gaussian", n.trees =5000, interaction.depth =5, shrinkage =0.1, verbose =F)
  yhat=predict (boost.data, newdata =cdata [split[(578*(i-1)+1):(578*i)],], n.trees =5000)
  cdata[split[(578*(i-1)+1):(578*i)],]$pred.boost<-yhat
  data.test=cdata [split[(578*(i-1)+1):(578*i)] ,"Rent"]
  m4[i]=mean((yhat -data.test)^2)
}
mean(sqrt(m4))


# yhat.boost=predict (boost.data ,newdata =cdata[-train ,],
#                     n.trees =5000)
# mean(( yhat.boost -data .test)^2)

# boost.boston =gbm(Rent ~ SqFt+Apt.or.House+Utilities.included+Latitude+Longitude+num.bedrooms,data=cdata, distribution=
#                     "gaussian ",n.trees =5000 , interaction .depth =4, shrinkage =0.2,
#                   verbose =F)
# yhat.boost=predict (boost.data ,newdata =cdata[-train ,],
#                       n.trees =5000)
# mean(( yhat.boost -data.test)^2)

library(ggplot2)

#First we plot a basic plot for rent by Square Footage
p <- ggplot(data=cdata, aes(y=Actual.rent, x=pred.RT))
p + geom_point() +
  xlab("Actual Rent") +
  ylab("Predict Rent") +
  xlim(0,15000) +
  ylim(0,15000) +
  geom_abline(intercept = 0, slope = 1) +
  theme(title=element_text(size=16),
        axis.text=element_text(size=14))

p <- ggplot(data=cdata, aes(x=pred.RT, y=-pred.RT+Actual.rent))
p + geom_point() +
  xlab("Predict Rent") +
  ylab("Rent Residual") +
  geom_abline(intercept = 0, slope = 0) +
  theme(title=element_text(size=16),
        axis.text=element_text(size=14))

p <- ggplot(data=cdata, aes(x=Latitude, y=Longitude, color=pred.RT))
p + geom_point() +
  scale_colour_gradientn(name="Predict Rent", colours = terrain.colors(5))+
  theme(title=element_text(size=16),
        axis.text=element_text(size=14))

p <- ggplot(data=cdata, aes(y=Actual.rent, x=pred.B))
p + geom_point() +
  xlab("Actual Rent") +
  ylab("Predict Rent") +
  xlim(0,15000) +
  ylim(0,15000) +
  geom_abline(intercept = 0, slope = 1) +
  theme(title=element_text(size=16),
        axis.text=element_text(size=14))

p <- ggplot(data=cdata, aes(x=pred.B, y=-pred.B+Actual.rent))
p + geom_point() +
  xlab("Predict Rent") +
  ylab("Rent Residual") +
  geom_abline(intercept = 0, slope = 0) +
  theme(title=element_text(size=16),
        axis.text=element_text(size=14))

p <- ggplot(data=cdata, aes(x=Latitude, y=Longitude, color=pred.B))
p + geom_point() +
  scale_colour_gradientn(name="Predict Rent", colours = terrain.colors(5))+
  theme(title=element_text(size=16),
        axis.text=element_text(size=14))

p <- ggplot(data=cdata, aes(y=Actual.rent, x=pred.rf))
p + geom_point() +
  xlab("Actual Rent") +
  ylab("Predict Rent") +
  xlim(0,15000) +
  ylim(0,15000) +
  geom_abline(intercept = 0, slope = 1) +
  theme(title=element_text(size=16),
        axis.text=element_text(size=14))
p <- ggplot(data=cdata, aes(x=pred.rf, y=-pred.rf+Actual.rent))
p + geom_point() +
  xlab("Predict Rent") +
  ylab("Rent Residual") +
  geom_abline(intercept = 0, slope = 0) +
  theme(title=element_text(size=16),
        axis.text=element_text(size=14))

p <- ggplot(data=cdata, aes(x=Latitude, y=Longitude, color=pred.rf))
p + geom_point() +
  scale_colour_gradientn(name="Predict Rent", colours = terrain.colors(5))+
  theme(title=element_text(size=16),
        axis.text=element_text(size=14))

p <- ggplot(data=cdata, aes(y=Actual.rent, x=pred.boost))
p + geom_point() +
  xlab("Actual Rent") +
  ylab("Predict Rent") +
  xlim(0,15000) +
  ylim(0,15000) +
  geom_abline(intercept = 0, slope = 1) +
  theme(title=element_text(size=16),
        axis.text=element_text(size=14))
p <- ggplot(data=cdata, aes(x=pred.boost, y=-pred.boost+Actual.rent))
p + geom_point() +
  xlab("Predict Rent") +
  ylab("Rent Residual") +
  geom_abline(intercept = 0, slope = 0) +
  theme(title=element_text(size=16),
        axis.text=element_text(size=14))

p <- ggplot(data=cdata, aes(x=Latitude, y=Longitude, color=pred.boost))
p + geom_point() +
  scale_colour_gradientn(name="Predict Rent", colours = terrain.colors(5))+
  theme(title=element_text(size=16),
        axis.text=element_text(size=14))


p <- ggplot(data=cdata, aes(y=Actual.rent, x=pred.lm))
p + geom_point() +
  xlab("Actual Rent") +
  ylab("Predict Rent") +
  xlim(0,15000) +
  ylim(0,15000) +
  geom_abline(intercept = 0, slope = 1) +
  theme(title=element_text(size=16),
        axis.text=element_text(size=14))
p <- ggplot(data=cdata, aes(x=pred.lm, y=-pred.lm+Actual.rent))
p + geom_point() +
  xlab("Predict Rent") +
  ylab("Rent Residual") +
  geom_abline(intercept = 0, slope = 0) +
  theme(title=element_text(size=16),
        axis.text=element_text(size=14))

p <- ggplot(data=cdata, aes(y=Actual.rent, x=pred.svm))
p + geom_point() +
  xlab("Actual Rent") +
  ylab("Predict Rent") +
  xlim(0,15000) +
  ylim(0,15000) +
  geom_abline(intercept = 0, slope = 1) +
  theme(title=element_text(size=16),
        axis.text=element_text(size=14))
p <- ggplot(data=cdata, aes(x=pred.svm, y=-pred.svm+Actual.rent))
p + geom_point() +
  xlab("Predict Rent") +
  ylab("Rent Residual") +
  geom_abline(intercept = 0, slope = 0) +
  theme(title=element_text(size=16),
        axis.text=element_text(size=14))



