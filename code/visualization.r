library(hexbin)
library(pals)
library(RColorBrewer)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(summarytools)
library(plotly)
library(ggthemes)
library(DataExplorer)


# 讀取 CSV 文件
data <- read_csv("../data/P39-Financial-Data.csv")

# histgram
for (col in names(data)) {
  if (is.numeric(data[[col]])) {
    p <- ggplot(data, aes(x = .data[[col]])) +
      geom_histogram() +
      labs(title = col) +
      theme_minimal()
    print(p)  # 使用 print() 函數顯示繪圖
    Sys.sleep(2)  # 為每個地塊相應地設置適當的時間
  }
}

# Scatter Plots
ggplot(data, aes(x = income, y = age )) +
  geom_point(size = 3)  # make bubble plot

# Hexbin
ggplot(data, aes(x = income, y = age)) + geom_bin2d() # using ggplot() function and geom_bin2d() function make Hexbin plot
# x and y's name is column name in csv file

# 載入資料集
esigned <- read_csv("../data/P39-Financial-Data.csv")

# 設定變數型別
esigned %<>% mutate(entry_id = as.character(entry_id), home_owner = as.factor(home_owner), has_debt = as.factor(has_debt), e_signed = as.factor(e_signed))

#移除 entry_id 欄位
esigned %<>% select(-entry_id)

# 檢視統計資料
esigned %>% dfSummary()
esigned %>% skimr::skim()

# 以年齡做分組統計人數
(ggplot(esigned, aes(x = age)) +
    geom_histogram(binwidth = 10, fill = "orange") + 
    geom_freqpoly(binwidth = 10) + 
    geom_text(aes(label = ..count..), stat = "bin", binwidth = 10) + 
    theme_economist_white()) %>% 
  ggplotly()

# 以收入做分組統計人數
(ggplot(esigned, aes(x = income)) +
    geom_histogram(binwidth = 2000, fill = "purple") + 
    geom_freqpoly(binwidth = 2000) + 
    geom_text(aes(label = ..count..), stat = "bin", binwidth = 2000) + 
    theme_economist_white()) %>% 
  ggplotly()

# 年齡密度圖
ggplot(esigned) + 
  geom_density(aes(x = age))

# 付款計畫長條圖圓餅圖
(ggplot(esigned, aes(x = pay_schedule, fill = pay_schedule)) +
    geom_bar() + 
    scale_fill_brewer(palette = "Accent") + 
    geom_text(aes(label = ..count..), stat = "count")) %>% 
  ggplotly()

# 付款計畫圓餅圖
ggplot(esigned, aes(x = "", fill = pay_schedule)) +
  geom_bar() + 
  coord_polar(theta = "y") + 
  scale_fill_brewer(palette = "Accent") + 
  theme_void()

# 以年齡做分組的收入箱型圖
ggplot(esigned, aes(x = cut_width(age, 10), y = income)) +
  geom_boxplot(notch = TRUE, fill = "pink") + 
  xlab("age") +
  theme_economist_white()

# 以收入做分組的貸款箱型圖
ggplot(esigned, aes(x = cut_width(income, 2000), y = amount_requested)) +
  geom_boxplot(notch = TRUE, fill = "yellow") + 
  xlab("income") + 
  theme_economist_white()

# Income/home_owner 比例長條圖
ggplot(esigned) + 
  aes(x = cut_width(income, 2000), fill = home_owner) + 
  xlab("income") + 
  ylab("percent") + 
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1")

# Income/has_debt 比例長條圖
ggplot(esigned) + 
  aes(x = cut_width(income, 2000), fill = has_debt) + 
  xlab("income") + 
  ylab("percent") + 
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1")

# age/has_debt 比例長條圖
ggplot(esigned) + 
  aes(x = cut_width(age, 10), fill = home_owner) + 
  xlab("age") + 
  ylab("percent") + 
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1")

# age/income/has_debt/amount_requested 散布圖
ggplot(esigned) + 
  aes(x = age, y = income, color = has_debt, size = amount_requested) + 
  geom_point() + 
  scale_color_manual(values = brewer.pal(3, "Set1"))

# amount_requested/risk_score hexbin
ggplot(esigned) +
  aes(x = amount_requested, y = risk_score) +
  geom_hex() + 
  paletteer::scale_fill_paletteer_c(palette = "pals::kovesi.cyclic_mygbm_30_95_c78")

# correlation
plot_correlation(esigned %>% select(income, amount_requested, years_employed, risk_score, risk_score_2, risk_score_3, risk_score_4, risk_score_5))

# amount_requested/risk_score/has_debt/age/e_signed scatterplot
ggplot(esigned) +
  geom_point(aes(x = amount_requested, y = risk_score, colour = e_signed, shape = has_debt, size = age)) + geom_smooth(aes(x = amount_requested, y = risk_score, colour = e_signed), method = lm)

# income/amount_requested/risk_score 3d scatterplot
plot_ly(esigned, x = ~income, y = ~amount_requested, z = ~risk_score)