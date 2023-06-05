library(kknn)
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

nearest_neighbor_kknn_spec <-
  nearest_neighbor() %>%
  set_engine('kknn') %>%
  set_mode('classification')

kknn_workflow <- 
  workflow() %>% 
  add_formula(formula = e_signed ~ .) %>% 
  add_model(nearest_neighbor_kknn_spec) 

fitted_kknn <- fit(kknn_workflow, data = esigned_train)


# KNN 訓練集效能
knn_predict_value <- predict(fitted_kknn, esigned_train, type = "prob")
knn_predict_value %<>% mutate(`.pred_class` = as.factor(ifelse(.pred_1 > .pred_0, 1, 0)))

(ROCit_obj <- rocit(score = knn_predict_value$.pred_1, class = esigned_train$e_signed))
plot(ROCit_obj)

# knn_predict_value %>% metric_set(yardstick::precision, yardstick::recall, yardstick::f_meas)(truth = esigned_train$e_signed, estimate = knn_predict_value$.pred_class, event_level = "second")

caret::confusionMatrix(data = knn_predict_value$.pred_class, reference = esigned_train$e_signed, positive = "1", mode = "everything")


# KNN 測試集效能
knn_predict_value <- predict(fitted_kknn, esigned_test, type = "prob")
knn_predict_value %<>% mutate(`.pred_class` = as.factor(ifelse(.pred_1 > .pred_0, 1, 0)))

(ROCit_obj <- rocit(score = knn_predict_value$.pred_1, class = esigned_test$e_signed))
plot(ROCit_obj)

# knn_predict_value %>% metric_set(yardstick::precision, yardstick::recall, yardstick::f_meas)(truth = esigned_test$e_signed, estimate = knn_predict_value$.pred_class, event_level = "second")

caret::confusionMatrix(data = knn_predict_value$.pred_class, reference = esigned_test$e_signed, positive = "1", mode = "everything")