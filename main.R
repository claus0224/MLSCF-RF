library(randomForest)
library(openxlsx)
library(Metrics)
rm(list = ls())

# 处理数据
matrixtrain <- read.xlsx('./matrixtrain.xlsx')
matrixtest <- read.xlsx('./matrixtest.xlsx')

trainX <- matrixtrain[,-8]
trainY <- matrixtrain[,-7]

test <- matrixtest[,-7]
test <- test[,-7]

label_train_x = matrixtrain[,7]
label_train_y = matrixtrain[,8]
label_test_x = matrixtest[,7]
label_test_y = matrixtest[,8]

## CFDVx
# 选择mtry
err <- as.numeric()

for(i in 1:6){
  rfmodel.x <- randomForest(CFDVx~., data = trainX, mtry = i, ntree = 100)
  predict_label <- predict(rfmodel.x, trainX)
  err[i] <- rmse(predict_label, label_train_x)
  print(err)
}

mtry <- which.min(err)

## 选择Ntree
err <- as.numeric()
ntree_num = seq(100,500,100)
for(i in ntree_num){
  rfmodel.x <- randomForest(CFDVx~., data = trainX, mtry = mtry, ntree = i)
  predict_label <- predict(rfmodel.x, trainX)
  err[i/100] <- rmse(predict_label, label_train_x)
  print(err)
}

# 十折交叉 mtry=6 ntree=200
err <- as.numeric()
list_num <- seq(1,30000,3000)
for(i in list_num){
  valiadtion_cv <- trainX[i:(i+2999),]
  train_cv <- trainX[-(i:(i+2999)),]
  label_cv <- label_train_x[i:(i+2999)]
  rfmodel.x <- randomForest(CFDVx~., data = train_cv, mtry = 6, ntree = 200)
  predict_label <- predict(rfmodel.x, valiadtion_cv)
  err <- rmse(predict_label, label_cv)
  print(err)
}


# 特征重要性
rfmodel.x <- randomForest(CFDVx~., data = trainX, mtry = 6, ntree = 200, importance = T)
importance.x <- importance(x=rfmodel.x)
head(importance.x)
varImpPlot(rfmodel.x, n.var = nrow(rfmodel.x$importance),
           main = 'variable importance')
# 预测
predict_label_x <- predict(rfmodel.x, test)
err <- rmse(predict_label_x, label_test_x)
print(err)

# 导出
write.table (predict_label_x, file ="predict_label_x.csv", sep ="", row.names =FALSE, col.names =TRUE, quote =TRUE)
write.table (label_test_x, file ="label_test_x.csv", sep ="", row.names =FALSE, col.names =TRUE, quote =TRUE)

## CFDVy
# 十折交叉 mtry=6 ntree=200
err <- as.numeric()
list_num <- seq(1,30000,3000)
for(i in list_num){
  valiadtion_cv <- trainY[i:(i+2999),]
  train_cv <- trainY[-(i:(i+2999)),]
  label_cv <- label_train_y[i:(i+2999)]
  rfmodel.y <- randomForest(CFDVy~., data = train_cv, mtry = 6, ntree = 200)
  predict_label <- predict(rfmodel.y, valiadtion_cv)
  err <- rmse(predict_label, label_cv)
  print(err)
}


# 特征重要性
rfmodel.y <- randomForest(CFDVy~., data = trainY, mtry = 6, ntree = 200, importance = T)
importance.y <- importance(x=rfmodel.y)
head(importance.y)
varImpPlot(rfmodel.y, n.var = nrow(rfmodel.x$importance),
           main = 'variable importance')
# 预测
predict_label_y <- predict(rfmodel.y, test)
err <- rmse(predict_label_y, label_test_y)
print(err)

# 导出
write.table (predict_label_y, file ="predict_label_y.csv", sep ="", row.names =FALSE, col.names =TRUE, quote =TRUE)
write.table (label_test_y, file ="label_test_y.csv", sep ="", row.names =FALSE, col.names =TRUE, quote =TRUE)
