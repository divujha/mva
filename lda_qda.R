


































install.packages("tidyverse")
library(tidyverse)
install.packages('caret’)
library(caret)
install.packages('ggplot2’)
library(ggplot2)

data=read.csv('C:/Users/Admin/Desktop/data.csv')
data
colSums(is.na(data))
str(data)

data=data[,2:32]
str(data)
data$diagnosis=as.factor(data$diagnosis)
str(data)
b=boxplot(data)

library('MASS')
model=lda(diagnosis ~ .,data=data)
model

summary(model)
model.values=predict(model)
model.values
names(model.values)

ldahist(data=model.values$x[,1],g=data$diagnosis)

#The Histogram beside typically showing “Predictor values VS 
#Predicted Target Variable” represents the probable outcome 
#of the diagnosis being Malignant or Benign based on the 
#predicted values through the Latent discriminant model.

plot(model.values$x[,1],model.values$posterior[,2])

#The scatter plot beside plots “Predictor values(x)VS Posterior values” 
#From the prediction model (model_values) Where Posterior is the 
#Posterior Probabilities of each observation belonging to class M or B.
#The plot here clearly depicts the misclassification points which are
#the misclassified end results. 

install.packages("tidyverse")
library(tidyverse)
library(caret)
C=confusionMatrix(model.values$class,data$diagnosis) 
C
#Using the Confusion Matrix in-built function 
#‘confusionMatrix()’ taking class and the outcome 
#Variable diagnosis as the two input we generate
#a confusion matrix giving the details of accuracy,
#p-value for acceptance and rejection . Here the 
#scale of True Positive and Negative along with 
#False Positive and Negative are shown . The
#Accuracy of the model has turned out to be 96.49%


#QDA
names(data)
model_qda=qda(diagnosis~.,data=data)
model_qda

#The Quadratic Discriminant Analysis is being modelled on the breast cancer 
#dataset taking the Diagnosis as the dependent variable compared to the rest of 
#the independent variable. which gives the prior probabilities of the Predicted 
#variable for each predicator variable.
model_qda_values=predict(model_qda)
model_qda_values
names(model_qda_values)

C_qda=confusionMatrix(model_qda_values$class,data$diagnosis)
C_qda

#THE ROC CURVE:
# Make predictions on the training set
predictions_lda <- as.numeric(predict(model)$class)
predictions_qda <- as.numeric(predict(model_qda)$class)
library(pROC)
# Create ROC curves
roc_lda <- roc(data$diagnosis, predictions_lda)
roc_qda <- roc(data$diagnosis, predictions_qda)
# Plot ROC curves
plot(roc_lda, col = "blue", main = "ROC Curves", lwd = 2, col.main = "black", ylim
     = c(0, 1), xlim = c(0, 1), xlab = "False Positive Rate", ylab = "True Positive Rate")
lines(roc_qda, col = "red", lwd = 2)
# Add legend
legend("bottomright", legend = c("LDA", "QDA"), col = c("blue", "red"), lwd = 2)

#We've plotted ROC curves for both LDA and QDA models By 
#comparing the curves, we can assess which model performs better in 
#terms of sensitivity and specificity. Here QDA curve dominates the LDA 
#curve clearly hence, the QDA model is superior in distinguishing 
#between the classes.

auc_qda <- auc(roc_qda)
print(auc_qda)

auc_lda <- auc(roc_lda)
print(auc_lda)

#The evaluation for Comparison of the Two methods are done on the 
#basis of the Confusion Matrix and ROC curve and AUC, in CM thereby 
#shows no major difference on the accuracy of prediction . A mere 
#difference of 0.87% is measured between the accuracies.The ROC 
#curve clears the confusion by depicting QDA superior to LDA. The 
#AUC(Area under curve) also provides the marginal difference . hence 
#we can conclude that QDA suites best for Distinguishing the classes of 
#THE BREAST CANCER DATASET.

