library(kernlab)
library(caret)
library(ROCit)
library(tidyverse)
library(magrittr)
library(tidymodels)

# 載入資料集
esigned <- read_csv("../data/P39-Financial-Data.csv")

# 設定變數型別
esigned %<>% mutate(entry_id = as.character(entry_id), home_owner = as.factor(home_owner), has_debt = as.factor(has_debt), e_signed = as.factor(e_signed))
esigned %<>% select(-entry_id)

# 拆分訓練集與測試集
esigned_split <- initial_split(esigned, prop = 0.80, strata = e_signed)
esigned_train <- training(esigned_split)
esigned_test <- testing(esigned_split)

# 支持向量機（SVM）
svm_rbf_kernlab_spec <-
  svm_rbf() %>%
  set_engine('kernlab') %>%
  set_mode('classification')

kernlab_workflow <- 
  workflow() %>% 
  add_formula(formula = e_signed ~ .) %>% 
  add_model(svm_rbf_kernlab_spec) 

fitted_kernlab <- fit(kernlab_workflow, data = esigned_train)

# SVM 訓練集效能
svm_predict_value <- predict(fitted_kernlab, esigned_train, type = "prob")
svm_predict_value %<>% mutate(`.pred_class` = as.factor(ifelse(.pred_1 > .pred_0, 1, 0)))

(ROCit_obj <- rocit(score = svm_predict_value$.pred_1, class = esigned_train$e_signed))
plot(ROCit_obj)

caret::confusionMatrix(data = svm_predict_value$.pred_class, reference = esigned_train$e_signed, positive = "1", mode = "everything")

# SVM 測試集效能
svm_predict_value <- predict(fitted_kernlab, esigned_test, type = "prob")
svm_predict_value %<>% mutate(`.pred_class` = as.factor(ifelse(.pred_1 > .pred_0, 1, 0)))

(ROCit_obj <- rocit(score = svm_predict_value$.pred_1, class = esigned_test$e_signed))
plot(ROCit_obj)

caret::confusionMatrix(data = svm_predict_value$.pred_class, reference = esigned_test$e_signed, positive = "1", mode = "everything")