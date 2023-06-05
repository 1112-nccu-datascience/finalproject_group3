# 讀取數據
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

# 劃分訓練集(80%)以及測試集(20%)
n <- 0.2 * nrow(df_scaled)
index <- sample(1:nrow(df_scaled), n)
train <- df_scaled[-index, ]
test <- df_scaled[index, ]

##################################################################################

# Adaboost
library(ada)
model_adaboost <- ada(e_signed ~ ., data = train)

predict_adaboost <- predict(model_adaboost, test)

accuracy_adaboost <- mean(predict_adaboost == test$e_signed)
accuracy_adaboost

table(true = test$e_signed, predict = predict_adaboost)