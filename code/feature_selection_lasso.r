library(xgboost)
library(caret)
library(ROCit)
library(tidyverse)
library(magrittr)
library(tidymodels)
library(recipeselectors)

# 載入資料集
esigned <- read_csv("../data/P39-Financial-Data.csv")

# 設定變數型別
esigned %<>% mutate(entry_id = as.character(entry_id), home_owner = as.factor(home_owner), has_debt = as.factor(has_debt), e_signed = as.factor(e_signed))
esigned %<>% select(-entry_id)

# 拆分訓練集與測試集
esigned_split <- initial_split(esigned, prop = 0.80, strata = e_signed)
esigned_train <- training(esigned_split)
esigned_test <- testing(esigned_split)
train_folds <- vfold_cv(esigned_train, v = 10)

# 示範網格搜索調整 XGBoost 超參數
boost_tree_xgboost_spec <-
  boost_tree(tree_depth = tune(), trees = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('classification')

xgboost_wflow <- workflow() %>%
  add_model(boost_tree_xgboost_spec) %>%
  add_formula(e_signed ~ .)

xgboost_tune <- xgboost_wflow %>%
  tune_grid(train_folds, grid = crossing(tree_depth = c(5, 10, 20), trees = c(10, 200)), metrics = metric_set(accuracy, f_meas))

# 使用 Feature Selection 預處理資料，訓練 XGBoost
rec <- recipe(e_signed ~., data = esigned)
lasso_recipe <- rec %>% step_rm(all_nominal_predictors()) %>% step_select_linear(all_predictors(), outcome = "e_signed", top_p = 5, engine = "glmnet", penalty = 0.1, mixture = 1)
lasso_estimates <- prep(lasso_recipe, training = esigned)
lasso_data <- bake(lasso_estimates, esigned)


xgboost_with_lasso <- workflow() %>%
  add_recipe(lasso_recipe) %>%
  add_model(boost_tree_xgboost_spec) %>%
  finalize_workflow(select_best(xgboost_tune, metric = "accuracy"))

fitted_xgboost_with_lasso <- fit(xgboost_with_lasso, esigned_train)

# XGBoost 訓練集效能
xgboost_predict_value <- predict(fitted_xgboost_with_lasso, esigned_train, type = "prob")
xgboost_predict_value %<>% mutate(`.pred_class` = as.factor(ifelse(.pred_1 > .pred_0, 1, 0)))

(ROCit_obj <- rocit(score = xgboost_predict_value$.pred_1, class = esigned_train$e_signed))
plot(ROCit_obj)

caret::confusionMatrix(data = xgboost_predict_value$.pred_class, reference = esigned_train$e_signed, positive = "1", mode = "everything")

# XGBoost 測試集效能
xgboost_predict_value <- predict(fitted_xgboost_with_lasso, esigned_test, type = "prob")
xgboost_predict_value %<>% mutate(`.pred_class` = as.factor(ifelse(.pred_1 > .pred_0, 1, 0)))

(ROCit_obj <- rocit(score = xgboost_predict_value$.pred_1, class = esigned_test$e_signed))
plot(ROCit_obj)

caret::confusionMatrix(data = xgboost_predict_value$.pred_class, reference = esigned_test$e_signed, positive = "1", mode = "everything")