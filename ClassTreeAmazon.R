Amazon <- read.csv("~/Data_Science/MarketingAnalytics/Amazon.csv", comment.char="#")
library(caret)
smp_size <- floor(0.7*nrow(Amazon))
set.seed(143)
train_ind <- sample(seq_len(nrow(Amazon)), size = smp_size)
train <- Amazon[train_ind,]
test <- Amazon[-train_ind,]
#Classification and Regression Tree
library(rpart)
amazon.rt <- rpart(Customer.Segment.n~Product.Container.n+Ship.Mode.n+Order.Priority.n+Product.Category.n+Product.Sub.Category.n+Province+Region+PRIN1+PRIN2+PRIN3+PRIN4, data=train, method="class", control=rpart.control(minsplit=10, cp=.00001) )
printcp(amazon.rt)
summary(amazon.rt, cp=)
pred <- predict(amazon.rt, type="class")
table(pred, train$Customer.Segment.n)
library(rpart.plot)
rpart.plot(amazon.rt, type = 1)
# library(randomForest)
# amazon.rf <- randomForest(Customer.Segment.n~Product.Container.n+Ship.Mode.n+Order.Priority.n+Product.Category.n+Product.Sub.Category.n+PRIN1+PRIN2+PRIN3+PRIN4, data=Amazon, mtry= 6, ntree=1000, importance = TRUE, na.action=na.omit)
# print(amazon.rf)
# importance(amazon.rf)


####Calculate performance of CART
library(ROCR)
actual <- as.numeric(train$Customer.Segment.n)
pred1 <- as.numeric(pred)
ROCR_pred <- prediction(pred1, actual)
#Recall and Precision
RP.perf <- performance(ROCR_pred, "prec", "rec")
plot(RP.perf)
#ROC Curve
ROC.perf <- performance(ROCR_pred, "tpr", "fpr")
plot(ROC.perf)
#ROC under curve
auc.tmp <- performance(ROCR_pred, "auc")
auc <- as.numeric(auc.tmp@y.values)
auc
