# [Group3] 貸款申請人信用風險危機預警模型之建立
目的是確定潛在客戶簽約貸款的可能性。大多數貸款公司分析申請人的財務歷史來決定是否貸款。即本案目的是開發一個型來預測申請者的信用風險情況。

## Contributors
|組員|系級|學號|工作分配|
|-|-|-|-|
|楊皓丞|資科三|109703008|簡報製作、建立模型（SVM、KNN、XGBoost）、特徵工程（PCA、Lasso feature selection、MRMR feature selection）| 
|陳芸|資科三|109703043|建立模型（Decision Tree、Random Forest、Bagging Tree）|
|王世揚|資科三|109703006|建立模型（Logistic Regression、Adaboost、Dense Neural Network）|
|戴士瑋|企三乙|109305082|模型效能評估（ROC、AUC、Precision、Recall、F1、Accuracy、Sensitivity、Specificity）|
|池田美和|資科三|109703036|探索性資料分析、資料視覺化、製作海報|

## Quick start
```R
Rscript code/script.R
```

## Folder organization and its related description
idea by Noble WS (2009) [A Quick Guide to Organizing Computational Biology Projects.](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1000424) PLoS Comput Biol 5(7): e1000424.

### docs
* Your presentation, 1112_DS-FP_group3.pptx
* Any related document for the project
  * i.e., software user guide

### data
* Input
  * Source
  * Format: csv
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
  * 切分訓練集（80%）與測試集（20%）

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
  * keras （同時需要Python環境及tensorflow套件）
  * kernlab
  * ROCit
  * kknn
  * xgboost
  * ggbiplot
  * pROC
  * recipeselectors
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
  * praznik
  * colino
  * glmnet
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
  * An Introduction to Statistical Learning with Applications in R, Gareth James, Daniela Witten , Trevor Hastie, Robert Tibshirani, Springer, 2013.
  * Artificial Intelligence-A Modern Approach, Fourth Edition, Stuart J. Russell and Peter Norvig, Pearson Education, 2021.
  * Data Mining —Practical Machine Learning Tools and Techniques, Fourth Edition, Ian H. Witten, Eibe Frank, Mark A. Hall, Christopher J. Pal, Elsevier, 2017.
  * Data Mining with R -Learning with Case Studies, Luís Torgo, Chapman & Hall, CRC, 2011.
  * Data Science for Business, Foster Provost and Tom Fawcett, O’Reilly Media, 2013.
  * Dive into Deep Learning, Aston Zhang, Zachary C. Lipton, Mu Li, and Alexander J. Smola
  * Hands-on Machine Learning with Scikit-Learn, Keras, and TensorFlow Concepts, Tools, and Techniques to Build Intelligent Systems, SECOND EDITION, Aurélien Géron, O’Reilly Media, 2019.
  * Introduction to Data Science, Hui Lin and Ming Li.
  * Introduction to Machine Learning, Third Edition, Ethem Alpaydın, 2014, Massachusetts Institute of Technology.
  * Introduction to Statistical Machine Learning, Masashi Sugiyama, Elsevier ,2016.
  * Machine Learning with R, Brett Lantz, Packt ,2013.
  * Mathematics for Machine Learning ,Marc Peter Deisenroth, A. Aldo Faisal, and Cheng Soon Ong, Cambridge University Press, 2020.
  * MATLAB Deep Learning-With Machine Learning, Neural Networks and Artificial Intelligence, Phil Kim, 2017.
  * Practical Data Science with R , NINA ZUMEL, JOHN MOUNT, Manning, 2014.
  * Tidy Modeling with R: A Framework for Modeling in the Tidyverse 1st Edition, Max Kuhn, Julia Silge, O'Reilly Media, Inc, 2022
  * https://ithelp.ithome.com.tw/articles/10201916
