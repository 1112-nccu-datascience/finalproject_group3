# 讀取數據
library(caret)
library(pROC)
data <- read.csv("../data/P39-Financial-Data.csv", header = TRUE, na.strings = "")

unique_contents <- unique(data$pay_schedule)
print(unique_contents)

has_na <- anyNA(data)
print(has_na)

# 整理數據：One-Hot Encoding
data$bi_weekly <- ifelse(data$pay_schedule == "bi-weekly", 1, 0)
data$weekly <- ifelse(data$pay_schedule == "weekly", 1, 0)
data$semi_monthly <- ifelse(data$pay_schedule == "semi-monthly", 1, 0)
data$monthly <- ifelse(data$pay_schedule == "monthly", 1, 0)

# 洗牌資料框
shuffled_df <- data[sample(nrow(data)), ]
# print(shuffled_df)

# 去掉entry_id\pay_schedule
df <- shuffled_df[, !(colnames(shuffled_df) %in% c("entry_id", "pay_schedule"))]

# 列出DataFrame中的所有列名
column_names <- colnames(df)
print(column_names)

# 選擇需要標準化的co;lumn
columns_to_scale <- c(
    "age", "income", "months_employed", "years_employed",
    "current_address_year", "personal_account_m", "personal_account_y",
    "amount_requested", "risk_score", "risk_score_2", "risk_score_3",
    "risk_score_4", "risk_score_5", "ext_quality_score", "ext_quality_score_2",
    "inquiries_last_month"
)

# 對選定的column進行標準化
scaled_columns <- scale(df[, columns_to_scale])
df_scaled <- cbind(df[, !(colnames(df) %in% columns_to_scale)], scaled_columns)



# 劃分訓練集(80%)以及測試集(20%)
n <- 0.2 * nrow(df_scaled)
index <- sample(1:nrow(df_scaled), n)
train <- df_scaled[-index, ]
test <- df_scaled[index, ]
test_y <- factor(test$e_signed, levels = c(0,1))

#計算混淆矩陣
#回傳一個列表，包含confusion_matrix、accuracy、specificity、sensitivity、recall、f_score
cal_confusion_matrix <- function(predict, test){
  if ( !(is.factor(predict))){
    predict <- factor(predict, levels = c(0,1))
  }
  result <- list()
  confusion <- confusionMatrix(predict, test)
  result[["confusion_matrix"]] <- confusion$table
  result[["accuracy"]] <- confusion$overall["Accuracy"]
  result[["specificity"]] <- confusion$byClass["Specificity"]
  result[["sensitivity"]] <- confusion$byClass["Sensitivity"]
  result[["recall"]] <- confusion$byClass["Recall"]
  result[["f_score"]] <- confusion$byClass["F1"]
  return(result)
}

##################################################################################
#決策樹
library(rpart)
library(rpart.plot)
# build rpart model
model <- rpart(train$e_signed ~ .,
               data = train, 
               control = rpart.control(maxdepth=6, cp=0),
               method = "class")
# model
# summary(model)
prp(model,              # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    # shadow.col="black", # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2)

predictions <- predict(model, newdata=test, type="class")


for ( i in cal_confusion_matrix(predictions,test_y)){
  print(i)
}

# 繪製 ROC 曲線
predict_prob <- predict(model, newdata=test, type="prob")[, 2]
roc_obj <- roc(test_y, predict_prob)
plot(roc_obj, main = "ROC Curve", print.auc = TRUE, auc.polygon = TRUE, grid = TRUE,
     col = "red", lwd = 2, auc.polygon.col = "snow2",xlim = c(1,0))
auc <- auc(roc_obj)

# prune decision tree
pmodel <- prune(model,
                cp = model$cptable[which.min(model$cptable[, "xerror"]),"CP"])
# pmodel
# summary(pmodel)
prp(pmodel,             # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    # shadow.col="black", # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2)
prediction <- predict(pmodel, newdata=test, type="class")

#打印混淆矩陣
for ( i in cal_confusion_matrix(prediction,test_y)){
  print(i)
}

# 繪製 ROC 曲線
predict_prob <- predict(pmodel, newdata=test, type="prob")[, 2]
roc_obj <- roc(test_y, predict_prob)
plot(roc_obj, main = "ROC Curve", print.auc = TRUE, auc.polygon = TRUE, grid = TRUE,
     col = "red", lwd = 2, auc.polygon.col = "snow2",xlim = c(1,0))
auc <- auc(roc_obj)

##################################################################################

#random_forest
library(randomForest)
# random forest
rf <- randomForest (as.factor(train$e_signed) ~ ., data = train,
                    mtry=ncol(train)/3,
                    method="classes")
rf
plot(rf)

# tune random forest: mtry
tuneRF(x=train[, -3], y=train[, 3], 
       ntreeTry= 200)
tune_rf <- randomForest(as.factor(train$e_signed) ~ ., data = train,
                        mtry = 8, 
                        ntree = 200,
                        method = "classes")
tune_rf
plot(tune_rf)

# tune random forest: important variables
# importance(tune_rf)
# importance(tune_rf, type=1)
varImpPlot(tune_rf) # choose: risk_score, amount_requested, income

# predict
predictions <- predict(tune_rf, test, type="class")
# head(predictions)

#打印混淆矩陣
for ( i in cal_confusion_matrix(predictions,test_y)){
  print(i)
}

# 繪製 ROC 曲線
predict_prob <- predict(tune_rf, newdata=test, type="prob")[, 2]
roc_obj <- roc(test_y, predict_prob)
plot(roc_obj, main = "ROC Curve", print.auc = TRUE, auc.polygon = TRUE, grid = TRUE,
     col = "red", lwd = 2, auc.polygon.col = "snow2",xlim = c(1,0))
auc <- auc(roc_obj)

##################################################################################
#bagging
library(ipred)

# reproducibility
set.seed(123)

# # find how many trees we should use
# ntree <- seq(10,60,2)
# 
# # create empty vector to store OOB RMSE values
# rmse <- vector(mode = "numeric", length = length(ntree))
# 
# #不同 ntree 數值建立出的 Bagging Trees
# for (i in seq_along(ntree)) {
# 
#   # perform bagged model
#   model <- bagging(train$e_signed ~ .,
#                    data    = train,
#                    coob    = TRUE,
#                    nbagg   = ntree[i])
#   rmse[i] <- model$err # get OOB error
# }
# # plot
# plot(ntree, rmse, type = 'l', lwd = 2, ylim = c(0, 1))
# abline(v = 65, col = "red", lty = "dashed")

# it turns out that the number of trees is not matter

# bagging
model <- bagging(as.factor(train$e_signed) ~ .,
                 data = train,
                 coob = TRUE, # Use the OOB sample to estimate the test error
                 nbagg = 10)
model

# predict
predictions <- predict(model, test)
# head(predictions)

#打印混淆矩陣
for ( i in cal_confusion_matrix(predictions,test_y)){
  print(i)
}

# 繪製 ROC 曲線
predict_prob <- predict(model, newdata=test, type="prob")[, 2]
roc_obj <- roc(test_y, predict_prob)
plot(roc_obj, main = "ROC Curve", print.auc = TRUE, auc.polygon = TRUE, grid = TRUE,
     col = "red", lwd = 2, auc.polygon.col = "snow2",xlim = c(1,0))
auc <- auc(roc_obj)

##################################################################################
# Adaboost
library(ada)
model_adaboost <- ada(e_signed ~ ., data = train)

predict_adaboost <- predict(model_adaboost, test)

#打印混淆矩陣
for (i in cal_confusion_matrix(predict_adaboost, test_y)){
  print(i)
}

# 繪製 ROC 曲線
predict_prob <- predict(model_adaboost, test, type = "prob")[, 2]
roc_obj <- roc(test_y, predict_prob)

plot(roc_obj, main = "ROC Curve", print.auc = TRUE, auc.polygon = TRUE, grid = TRUE,
     col = "red", lwd = 2, auc.polygon.col = "snow2",xlim = c(1,0))
auc <- auc(roc_obj)

##################################################################################

# Logistics Regression
library(e1071)
model_logistics <- glm(e_signed ~ .,
    family = binomial(link = "logit"), data = train
)
predict_logistics_prob <- predict(model_logistics, test, type = "response")
predict_logistics <- ifelse(predict_logistics_prob > 0.5, 1, 0)

#打印混淆矩陣
for (i in cal_confusion_matrix(predict_logistics, test_y)){
  print(i)
}

# 繪製 ROC 曲線
roc_obj_logistics <- roc(test_y, predict_logistics_prob)
plot(roc_obj_logistics, main = "ROC Curve", print.auc = TRUE, auc.polygon = TRUE, grid = TRUE,
     col = "red", lwd = 2, auc.polygon.col = "snow2",xlim = c(1,0))
auc_logistics <- auc(roc_obj_logistics)

##################################################################################

# 加载所需套件keras
library(keras)

# 創建模型
model <- keras_model_sequential()
model %>%
    layer_dense(units = 8, input_shape = ncol(train) - 1, activation = "relu") %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dense(units = 32, activation = "relu") %>%
    # layer_dense(units = 64, activation = "relu") %>%
    # layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 16, activation = "relu") %>%
    layer_dense(units = 8, activation = "relu") %>%
    layer_dense(units = 4, activation = "relu") %>%
    layer_dense(units = 2, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")

# 編譯模型
model %>% compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_adam(),
    metrics = c("accuracy")
)

# 擬和
history <- model %>% fit(
    x = as.matrix(train[, -3]),
    y = train[, 3],
    epochs = 50,
    batch_size = 100,
    validation_split = 0.1
)

# 預測
predictions_prob <- predict(model, as.matrix(test[, -3]))
predictions <- ifelse(predictions_prob > 0.5, 1, 0)

#打印混淆矩陣
for (i in cal_confusion_matrix(predictions, test_y)){
  print(i)
}

# 繪製 ROC 曲線
roc_obj_nn <- roc(test_y, predictions_prob[,1])
plot(roc_obj_nn, main = "ROC Curve", print.auc = TRUE, auc.polygon = TRUE, grid = TRUE,
     col = "red", lwd = 2, auc.polygon.col = "snow2",xlim = c(1,0))
auc_nn <- auc(roc_obj_nn)