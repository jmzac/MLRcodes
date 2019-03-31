
#####################################
# Build Classfier for Sonar dataset 
#####################################

library(mlbench)

# Load the sonar dataset
data("Sonar")

head(Sonar)

set.seed(42)

#split into training and test sets
library(caTools)
split=sample.split(Sonar,SplitRatio=.8)
trainset=subset(Sonar, split=="TRUE")  # training dataset
testset=subset(Sonar,split=="FALSE")   # test dataset


# 1.Build the SVM classfier with kernel set as linear
svm_model <- svm(Class~ ., data=trainset, method="C-classification", kernel="linear")
svm_model

#test set predictions
pred_test <-predict(svm_model,testset)

# Plot a confusion matrix to predict testset accuracy
confusion_matrix= table(pred = pred_test, true = testset$Class)
mean(pred_test==testset$Class)

# mean accuracy is : 0.7727273

# 2. Radial case
svm_model <- svm(Class~ ., data=trainset, method="C-classification", kernel="radial")
svm_model

#test set predictions
pred_test <-predict(svm_model,testset)

# Plot a confusion matrix to predict testset accuracy
confusion_matrix= table(pred = pred_test, true = testset$Class)
mean(pred_test==testset$Class)

# mean accuracy is : 0.9090909

# 3.Now a Naive Baye's classfier
bnc_model <- naiveBayes( Class~ ., data = trainset) 
bnc_model

# test set predictions
pred <- predict(bnc_model,  testset)

# form and display confusion matrix & overall accuracy
tab <- table(pred,  testset$Class) 
tab

# To find probability of correction
sum(tab[row(tab)==col(tab)])/sum(tab)

# Accuracy is 0.7272727

# Based on the above three accuracy results, 
# a radial SVM classfier would be a better choice for the sonar dataset

