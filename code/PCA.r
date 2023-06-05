library(tidyverse)
library(tidymodels)
library(magrittr)
library(ggbiplot)

# 載入資料集
esigned <- read_csv("../data/P39-Financial-Data.csv")

# 設定變數型別
esigned %<>% mutate(entry_id = as.character(entry_id), home_owner = as.factor(home_owner), has_debt = as.factor(has_debt), e_signed = as.factor(e_signed))
esigned %<>% select(-entry_id)

# 拆分訓練集與測試集
esigned_split <- initial_split(esigned, prop = 0.80, strata = e_signed)
esigned_train <- training(esigned_split)
esigned_test <- testing(esigned_split)

esigned.e_signed <- esigned %>% select(e_signed)
numeric.esigned <- esigned %>% select(is.numeric)
esigned.pca <- prcomp(numeric.esigned, center = TRUE, scale. = TRUE)
g <- ggbiplot(esigned.pca, choices = c(1, 2), obs.scale = 1, var.scale = 1, ellipse = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top', plot.background = element_rect(fill='transparent', color=NA), legend.background = element_rect(fill='transparent', color=NA))
g