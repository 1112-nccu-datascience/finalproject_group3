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

# Logistics Regression
library(e1071)
model_logistics <- glm(e_signed ~ .,
    family = binomial(link = "logit"), data = train
)
predict_logistics <- predict(model_logistics, test, type = "response")
predict_logistics <- ifelse(predict_logistics > 0.5, 1, 0)
predict_logistics
Accuracy_logistics <- mean(predict_logistics == test$e_signed)
Accuracy_logistics
table(true = test$e_signed, predict = predict_logistics)