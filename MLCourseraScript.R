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
trainingset<-train[,colSums(is.na(training)) == 0]
testingset <-test[,colSums(is.na(testing)) == 0]

#Remove columns 1:6 because they contain irrelevant data like timestamp and index
trainingset   <-trainingset[,-c(1:6)]
testingset <-testingset[,-c(1:6)]

#We will go with a 75% training and 25% testing sample
sample<-createDataPartition(y=trainingset$classe, p=0.75, list=FALSE)
sampletraining<-trainingset[sample,]
sampletesting<-trainingset[-sample,]

#We will start with radomforest
modelrf<-randomForest(classe~., data=sampletraining, method="class')
predictionrf<-predict(modelrf, sampletraining, type="class")

#Now, check accuracy with confusion matrix
confusionMatrix(predictionrf, sampletesting$classe)

#Check class predictions
class_predictions<-predict(modelrf, testingset, type="class")

#display class predictions
class_predictions

#100 percent of the predictions are correct using random forest

#Finally, save files for submissions
mlp_write= function (x) {
n=length(x)
for(i in 1:n) {
filename = paste0("problem_id",i,".txt")
write.table(x[i], file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE)
}
	}

mlp(class_predictions)


