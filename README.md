# [Group3] 預測申請人是否會完成貸款流程
目的是確定潛在客戶簽約貸款的可能性。大多數貸款公司分析申請人的財務歷史來決定是否貸款。即本案目的是開發一個型來預測申請者的信用風險情 況。

## Contributors
|組員|系級|學號|工作分配|
|-|-|-|-|
|楊皓丞|資科三|109703008|簡報製作、建立模型（SVM、KNN、XGBoost）、特徵工程（PCA、Lasso feature selection、MRMR feature selection）| 
|陳芸|資科三|109703043|建立模型（Decision Tree、Random Forest、Bagging Tree）|
|王世揚|資科三|109703006|建立模型（Logistic Regression、Adaboost、Dense Neural Network）|
|戴士瑋|企三乙|109305082|模型效能評估（ROC、AUC、Precision、Recall、F1、Accuracy、Sensitivity、Specificity）|
|池田美和|資科三|109703036|探索性資料分析、資料視覺化、製作海報|

## Quick start
You might provide an example commend or few commends to reproduce your analysis, i.e., the following R script
```R
Rscript code/your_script.R --input data/training --output results/performance.tsv
```

## Folder organization and its related description
idea by Noble WS (2009) [A Quick Guide to Organizing Computational Biology Projects.](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1000424) PLoS Comput Biol 5(7): e1000424.

### docs
* Your presentation, 1112_DS-FP_groupID.ppt/pptx/pdf (i.e.,1112_DS-FP_group1.ppt), by **06.08**
* Any related document for the project
  * i.e., software user guide

### data
* Input
  * Source
  * Format
  * Size 
* Output

### code
* Analysis steps
* Which method or package do you use? 
  * original packages in the paper
  * additional packages you found

### results
* What is a null model for comparison?
* How do your perform evaluation?
  * Cross-validation, or extra separated data

## References
* Packages you use
  * hexbin
  * pals
  * RColorBrewer
  * rpart
  * rpart.plot
  * randomForest
  * ipred
  * ada
  * e1071
  * keras
  * kernlab
  * ROCit
  * kknn
  * xgboost
  * ggbiplot
  * pROC
  * gtsummary
  * ggvis
  * tidyverse
  * tidymodels
  * caret
  * mlr3
  * ggplot2
  * ggthemes
  * plotly
  * magrittr
  * rgl
  * ggfittext
  * DescrTab2
  * neuralnet
  * RColorBrewer
  * viridis
  * summarytools
  * DataExplorer
  * baguette
  * ModelMetrics
* Related publications
