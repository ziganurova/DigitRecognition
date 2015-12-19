library(pROC)
library(ROCR)

#read matrixTheta from the file

matrixTheta = read.table("MatrixTheta.dat", header=T, sep=",") 
matrixTheta <- as.matrix(matrixTheta)
matrixTheta <- matrixTheta[,-1]

#Calculation of predictedLabels 

f <- matrix (0, 10000, 10)
 
for (i in 1:10000) {
  f[i,] <- 1/(1+exp(-(t(matrixTheta) %*% testData[i,])));
  predictedLabels[i] = which.max(f[i,])-1;
}

print ("Accuracy on test data:")
print(sum(predictedLabels == testLabels)/length(testLabels))

# Calculation multi-class ROC curve
print(multiclass.roc(predictedLabels[,1],testLabels[,1]))
  
# Calculation Recall, precision, specificity, F-measure, FDR and ROC for each class separately
newPredictedLabels = predictedLabels;
 
# Enter the class number k:
k = 0;
print ("Class: ")
print (k)

newPredictedLabels[(predictedLabels == k)] = 1;
newPredictedLabels[(predictedLabels != k)] = 0;
newTestLabels[(testLabels == k)] = 1;
newTestLabels[(testLabels != k)] = 0;
 
predObj = prediction(newPredictedLabels, newTestLabels)
   
#Recal - True Positive
print("Recal - True Positive")
print(performance(predObj,"rec"))
   
#Precision - Positive Predicted Value
print ("Precision - Positive Predicted Value")
print(performance(predObj,"ppv"))
   
#specificity
print ("Specificity")
print(performance(predObj,"tnr"))
   
#F-measure
print ("F-measure")
print(performance(predObj,"f"))
   
#FDR  
print("False Discovery Rate")
print(performance(predObj,"pcfall"))
   
#ROC
plot(performance(predObj,"rch"))
   
#AUC 
print ("AUC - Area under the ROC curve")
print(performance(predObj,"auc"))
   
  