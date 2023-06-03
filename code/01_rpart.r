library(rpart)
library(rpart.plot)

# 讀取數據
data <- read.csv("../data/P39-Financial-Data.csv", header = TRUE, na.strings = "")

unique_contents <- unique(data$pay_schedule)
print(unique_contents) # "bi-weekly" "weekly" "semi-monthly" "monthly"

has_na <- anyNA(data)
print(has_na) # no need to deal with na values

# 整理數據：One-Hot Encoding
data$bi_weekly <- ifelse(data$pay_schedule == "bi-weekly", 1, 0)
data$weekly <- ifelse(data$pay_schedule == "weekly", 1, 0)
data$semi_monthly <- ifelse(data$pay_schedule == "semi-monthly", 1, 0)
data$monthly <- ifelse(data$pay_schedule == "monthly", 1, 0)

# 洗牌資料框
shuffled_df <- data[sample(nrow(data)), ]
print(head(shuffled_df))

# 去掉entry_id\pay_schedule
df <- shuffled_df[, !(colnames(shuffled_df) %in% c("entry_id", "pay_schedule"))]

# 列出DataFrame中的所有列名
column_names <- colnames(df)
print(column_names)

# 選擇需要標準化的column
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
print(head(df_scaled))



# 劃分訓練集(80%)以及測試集(20%)
n <- 0.2 * nrow(df_scaled)
index <- sample(1:nrow(df_scaled), n)
train <- df_scaled[-index, ]
test <- df_scaled[index, ]



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
cm <- table(real=test$e_signed, predict=predictions)
accuracy_rpart <- sum(diag(cm)) / sum(cm)
accuracy_rpart

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
predictions <- predict(pmodel, newdata=test, type="class")
cm <- table(real=test$e_signed, predict=predictions)
accuracy_rpart <- sum(diag(cm)) / sum(cm)
accuracy_rpart

