
#installing & Calling the Required packages/Libraries
install.packages("dplyr")
install.packages("MASS")
install.packages("ggplot2")
install.packages("readr")
library(dplyr)
library(MASS)
library(ggplot2)
library(readr)

#############################################################################################################
#############################################################################################################

#Reading in the given Data Set
owls <- read_csv("owls15.csv")

#############################################################################################################
#############################################################################################################

#Sampling the Data Set into training 66% and Testing 34%
samplesize <- floor(0.66 * nrow(owls))
fullowlsdata <- sample(seq_len(nrow(owls)), size = samplesize)

#Training Set
trainowl <- owls[fullowlsdata, ]

#Testing Set
testowl <- owls[-fullowlsdata, ]

#############################################################################################################
#############################################################################################################


########Visualizing Data#########
#Data Visualisation for distribution of Body Length and Wing Length
ggplot(owls, aes(x = BodyL, y = WingL))+ geom_point(aes(colour=Type, shape=Type),size=2)+ xlab("Body length")+ 
  ylab("Wing length")+ ggtitle("Type vs Body vs Wing lengths")+ theme(plot.title = element_text(hjust = 0.5))

#Data Visualisation for distribution of Body Length and Wing Width
ggplot(owls, aes(x = BodyW, y = WingW))+ geom_point(aes(colour=Type, shape=Type),size=2)+ xlab("Body Width")+ 
  ylab("Wing Width")+ ggtitle("Type vs Body Width vs Wing Width")+ theme(plot.title = element_text(hjust = 0.5))


#############################################################################################################
#############################################################################################################

#Writing function for Perceptron Algorithm 

perceptron <- function(a, l, stepfunction, iteration) 
  {
  #a-set of attributes from train data
  #l-set of labels from train data
  #stepfunction-Activation function, also called error correction procedure.
  #iteration-also known as epoch. divides the data into 10 sets and run the model 10 times through the respective set everytime.
 

  # initialize weight, error - false prediction, correct - true prediction vector
  weight <- rep(0, dim(a)[2] + 1)
  wrongpred <- rep(0, iteration)
  correct <- rep(0, iteration)
  
  # Iterating through the given data set given number of times.
    for (j in 1:iteration) 
    {
   cat("Iteration ",j,"\n")
    # looping through the length of data set containing Labels 
    for (i in 1:length(l)) 
      {
      # Predicting binary label -1(Wrong Prediction) or 1(Correct Prediction). Assuming the first Weight value as Threshold
      computedWeight <- sum(weight[2:length(weight)] * as.numeric(a[i, ])) + weight[1]
      
      #Depending on the computed weight predicting the labels. Threshold is assumed to be 0
      if(computedWeight < 0) 
        {
        predictedlabel <- -1
      } 
      else
        {
        predictedlabel <- 1
      }
      
      # Change weight - if prediction is wrong in order to converge to desired result.
      weightdiff <- stepfunction * (l[i] - predictedlabel) * c(1, as.numeric(a[i, ]))
      weight <- weight + weightdiff
      
      #Printing the Actual Value and the Predicted Value in each iteration.
      cat("\tActual Value: \t",l[i],"\tPredicted Value: \t",predictedlabel,"\n")
     # Storing the Values in a file
     # write.csv(y[i],file="AP.csv",append = TRUE)
      
      
      # Counting total number of Wrong Prediction
      if ((l[i] - predictedlabel) != 0) 
        {
        wrongpred[j] <- wrongpred[j] + 1
         }
    else
      {
      #Counting Total number of Correct Prediction
      correct[j] <- correct[j] + 1
      }}}
 
  #Calculating the Size of the data set
  sizeofdata <- nrow(a)
  
  #Calculating Accuracy
  accuracy <- (correct/sizeofdata)*100
  #accuracy <- ((sizeofdata-wrongpred)/sizeofdata)*100
  cat("Accuracy in %:",signif(accuracy,4),"\n")
 # cat("Correct preds: ",correct,"\n")
  cat("Average Accuracy in %:",mean(accuracy))
  return(wrongpred)
}


###########################################################################
#Training Algorithm
###########################################################################

#Seperating out attributes and labels for the given data set
training_attributes <- trainowl[, 1:4] 

#Training Algorithm to classify among the three classes taking one at a time i.e. LongEaredOwl vs 
#SnowyOwl "OR" BarnOwl and Vice versa for the remaining two.
#One vs All algorithm
#Long Eared Owl
training_label1 <- rep(-1, dim(training_attributes)[1])
training_label1[trainowl[, 5] == "LongEaredOwl"] <- 1
cat("Training Results")
#Calling the function perceptron for LongEaredOwl vs SnowyOwl OR BarnOwl.
err1 <- perceptron(training_attributes, training_label1, 0.5, 10)

#Plotting the Learning Rate - No. of wrong Predictions in each Iteration.
plot(1:10, err1, main="wrongpred in differentiating Long Eared Owl from the rest", type="b", col="red", xlab="Iteration", ylab="Errors")


#Snowy Owl
training_label2 <- rep(-1, dim(training_attributes)[1])
training_label2[trainowl[, 5] == "SnowyOwl"] <- 1
#Calling the function perceptron for SnowyOwl vs LongEaredOwl OR BarnOwl
err2 <- perceptron(training_attributes, training_label2, 0.5, 10)
#Plotting the Learning Rate - No. of wrong Predictions in each Iteration.
plot(1:10, err2, main="wrongpred in differentiating Snowy Eared Owl from the rest",type="b", col="red", xlab="Iteration", ylab="Errors")


#Barn Owl 
training_label3<- rep(-1, dim(training_attributes)[1])
training_label3[trainowl[, 5] == "BarnOwl"] <- 1
#Calling the function perceptron for BarnOwl vs LongEaredOwl OR SnowyOwl
err3 <- perceptron(training_attributes, training_label3, 0.5, 10)
#Plotting the Learning Rate - No. of wrong Predictions in each Iteration.
plot(1:10, err3, main="wrongpred in differentiating Barn Owl from the rest", type="b", col="red", xlab="Iteration", ylab="Errors")


###########################################################################
#Testing Algorithm  by following the same steps used in training the Model 
###########################################################################
cat("Testing Results")
testattribute <- testowl[, 1:4]

#Long Eared Owl
testlabel1<- rep(-1, dim(testattribute)[1])
testlabel1[testowl[, 5] == "LongEaredOwl"] <- 1
#Calling the function perceptron for LongEaredOwl vs SnowyOwl OR BarnOwl.
err4<-perceptron(testattribute,testlabel1, 0.5, 10)
#Plotting the Learning Rate - No. of wrong Predictions in each Iteration.
plot(1:10, err4, main="wrongpred in differentiating Long Eared Owl from the rest", type="b", col="red", xlab="Iteration", ylab="Errors")


#Sonwy Owl
testlabel2<- rep(-1, dim(testattribute)[1])
testlabel2[testowl[, 5] == "SnowyOwl"] <- 1
#Calling the function perceptron for SnowyOwl vs LongEaredOwl OR BarnOwl
err5<-perceptron(testattribute,testlabel2, 0.5, 10)
#Plotting the Learning Rate - No. of wrong Predictions in each Iteration.
plot(1:10, err5, main="wrongpred in differentiating Snowy Owl from the rest", type="b", col="red", xlab="Iteration", ylab="Errors")


#Barn Owl
testlabel3<- rep(-1, dim(testattribute)[1])
testlabel3[testowl[, 5] == "BarnOwl"] <- 1
#Calling the function perceptron for BarnOwl vs LongEaredOwl OR SnowyOwl
err6<- perceptron(testattribute,testlabel3, 0.5, 10)
#Plotting the Learning Rate - No. of wrong Predictions in each Iteration.
plot(1:10, err6, main="wrongpred in differentiating Barn Owl from the rest", type="b", col="red", xlab="Iteration", ylab="Errors")



