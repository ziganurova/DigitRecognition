# This sciprt file contains a frame for learning handwritten digitals from the MNIST dataset

# load training data from files
data <- loadMNISTData("train-images-idx3-ubyte", "train-labels-idx1-ubyte")
trainLabels <- data$labels
trainData <- data$data

# trainingData should be 60000x786,  60000 data and 784 features (28x28), tha matrix trainData has 60000 rows and 784 columns
# trainingLabels should have 60000x1, one class label \in {0,1,...9} for each data.
#testData should be 10000x786,  10000 data and 784 features (28x28), tha matrix trainData has 10000 rows and 784 columns
#testLabels should have 10000x1, one class label \in {0,1,...9} for each data.

#uncomment the following 3 lines to see the nth training example and its class label.
# n = 10;
# image( t(matrix(trainData[n, ], ncol=28, nrow=28)), Rowv=28, Colv=28, col = heat.colors(256),  margins=c(5,10))
# print("Class label:"); print(trainLabels[n])

data <- loadMNISTData("t10k-images-idx3-ubyte", "t10k-labels-idx1-ubyte");
testLabels <- data$labels;
testData <- data$data;

#calculation Logistic Regression

nClasses <- 10;
nData <- 60000;
nFeatures <- 784;
nTestData <- 10000;

predictedLabels <- matrix(0, nTestData, 1)

#normalizing testData and trainData
testData <- testData/255;
trainData <-trainData/255;

#inserts a constant 1 row to trainData for bias 
trainData <- cbind(1, trainData); 
testData <- cbind(1, testData)

newTrainingLabels <- trainLabels; # creating newTrainingLabels vector for binary classification
newTestLabels <- testLabels; #creating newTestLabels for binary classification

#matrixTheta is a matrix containing nFeatures+1 rows and nClasses columns, i.e 785x10. 
#Theta for each class is cantained in a column
matrixTheta <- matrix (0, (nFeatures+1), 10);

for (k in 0:9) { 

  print ("CLASS NUMBER:")
  print (k)

  #redefining labels for training and testing sets
  newTrainingLabels[(trainLabels == k)] = 1;
  newTrainingLabels[(trainLabels != k)] = 0;
  newTestLabels[(testLabels == k)] = 1;
  newTestLabels[(testLabels != k)] = 0;

  theta <- runif(nFeatures+1, min = 0, max = 0.001); #initialisation theta (for binary class.)
  prevJ <- 0 ; #previous error
  term <- 0.001 #termination condition
  J <- term + 1; # actual error
  lambda <- 0;  #regularization parameter
  mu <- 0.00001; #learning rate
  h <- newTrainingLabels * 0; # for sigmoid function
  counter <- 0;
  
  while (abs(J - prevJ) > term) {
    delta <- theta * 0;
    error <- 0;
   
    counter <- counter + 1;
    print ("COUNTER")
    print (counter)

    h <- 1/(1+exp(- theta  %*% t(trainData) ));
    delta <- delta + ((h - t(newTrainingLabels)) %*% trainData);
    error <- error - sum(t(newTrainingLabels) * log(h) + ((1 - t(newTrainingLabels)) * log(1 - h)));
   
    theta <- theta - mu*delta;
    delta <- delta/nData; #normalising theta
    delta[-1] <- delta[-1] - 2*lambda*theta[-1]; 
    error <- error/nData + lambda*sum(theta[-1]^2);  #normalising error
     
    prevJ <- J
    J <- error
    print ("ERROR")
    print(J)
      
  } #closing while
  
  matrixTheta[,k+1] <- theta; #adding vector theta for 2 classes to matrix
}

#=================Testing part =====================
#The testing part and error metrics are written in "TestAndMeasures.R"

#f - function of the decision boundary
f <- matrix (0, 10000, 10)

for (i in 1:10000) {
  f[i,] <- 1/(1+exp(-(t(matrixTheta) %*% testData[i,])));
  predictedLabels[i] = which.max(f[i,])-1;
}

#calculate accuracy
print("accuracy on test data:")
print(sum(predictedLabels == testLabels)/length(testLabels))
