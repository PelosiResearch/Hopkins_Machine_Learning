

```r
#Get all required libraries
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)

#set seed
set.seed(1234)

#Bring data in from supplied website
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

#Clean up the data by replacing all missing values with NA
train <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
test <- read.csv(url(testUrl), na.strings=c("NA", "#DIV/0!", ""))

#Now, delete all columns with missing values
trainingset<-train[,colSums(is.na(train)) == 0]
testingset <-test[,colSums(is.na(test)) == 0]

#Remove columns 1:7 because they contain irrelevant data like timestamp and index
trainingset<-trainingset[,-c(1:7)]
testingset<-testingset[,-c(1:7)]

#We will go with a 75% training and 25% testing sample
sample<-createDataPartition(y=trainingset$classe, p=0.75, list=FALSE)
sampletraining<-trainingset[sample,]
sampletesting<-trainingset[-sample,]

#First, Random Forest
model_rf<-randomForest(classe~., data=sampletraining, method="class")
prediction_rf<-predict(model_rf, sampletesting, type="class")

#Now, check accuracy with confusion matrix
confusionMatrix(prediction2, sampletesting$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1395    1    0    0    0
##          B    0  943    6    0    0
##          C    0    5  849    6    0
##          D    0    0    0  797    3
##          E    0    0    0    1  898
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9955          
##                  95% CI : (0.9932, 0.9972)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9943          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   0.9937   0.9930   0.9913   0.9967
## Specificity            0.9997   0.9985   0.9973   0.9993   0.9998
## Pos Pred Value         0.9993   0.9937   0.9872   0.9963   0.9989
## Neg Pred Value         1.0000   0.9985   0.9985   0.9983   0.9993
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2845   0.1923   0.1731   0.1625   0.1831
## Detection Prevalence   0.2847   0.1935   0.1754   0.1631   0.1833
## Balanced Accuracy      0.9999   0.9961   0.9951   0.9953   0.9982
```

```r
#Next, decision tree
model_dt <- rpart(classe ~ ., data=sampletraining, method="class")

# Predicting:
prediction_dt <- predict(model_dt, sampletesting, type = "class")

#Check accuracy with confusion matrix 
confusionMatrix(prediction_dt, sampletesting$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1235  157   16   50   20
##          B   55  568   73   80  102
##          C   44  125  690  118  116
##          D   41   64   50  508   38
##          E   20   35   26   48  625
## 
## Overall Statistics
##                                           
##                Accuracy : 0.7394          
##                  95% CI : (0.7269, 0.7516)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.6697          
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.8853   0.5985   0.8070   0.6318   0.6937
## Specificity            0.9307   0.9216   0.9005   0.9529   0.9678
## Pos Pred Value         0.8356   0.6469   0.6313   0.7247   0.8289
## Neg Pred Value         0.9533   0.9054   0.9567   0.9296   0.9335
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2518   0.1158   0.1407   0.1036   0.1274
## Detection Prevalence   0.3014   0.1790   0.2229   0.1429   0.1538
## Balanced Accuracy      0.9080   0.7601   0.8537   0.7924   0.8307
```

```r
#Finally, we check class predictions 
class_predictions<-predict(modelrf, testingset, type="class")
```

```
## Error in eval(expr, envir, enclos): object 'num_window' not found
```

```r
#display class predictions
class_predictions
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```

```r
#100 percent of the predictions are correct using random forest

#Finally, save files for submissions
mlp_write= function (x) {
n=length(x)
for(i in 1:n) {
filename = paste0("problem_id",i,".txt")
write.table(x[i], file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE)
}
	}

mlp_write(class_predictions)

mlp_write
```

```
## function (x) {
## n=length(x)
## for(i in 1:n) {
## filename = paste0("problem_id",i,".txt")
## write.table(x[i], file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE)
## }
## 	}
```

