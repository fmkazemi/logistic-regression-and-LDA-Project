# Applying logistic regression and LDA to the data A2_T2.tab (a tab-delimited text file)
This data consists of 180 features, 2 classes and 3186 instances.

First we separate 10% of the instances in the data as a validation set. 

Second, we apply logistic regression and LDA to the remaining 90% of instances. 

Third, we use the learned models to predict the class for the instances in the validation set. For this
task, we use R language to learn the model for each approach. 

Forth, an ROC curve for logistic regression and LDA on the validation set is polotted.

Fifth, the confusion matrix and performance measures for both logistic regression and LDA using the optimal probability threshold are obtained.
