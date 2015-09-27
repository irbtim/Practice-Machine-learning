#Practical Machine Learning

#loading libraries
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)

#setting the seed for reproduceability
set.seed(1234)

#Loading the data set
trainingset <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!", ""))
testingset <- read.csv('pml-testing.csv', na.strings=c("NA","#DIV/0!", ""))

#Check dimensions
dim(trainingset) 
dim(testingset)

#Delete missing values
trainingset<-trainingset[,colSums(is.na(trainingset)) == 0]
testingset <-testingset[,colSums(is.na(testingset)) == 0]
trainingset   <-trainingset[,-c(1:7)]
testingset <-testingset[,-c(1:7)]

#Check dimensions
dim(trainingset)
dim(testingset)
head(trainingset)
head(testingset)

#Create data partition
subsamples <- createDataPartition(y=trainingset$classe, p=0.75, list=FALSE)
subTraining <- trainingset[subsamples, ] 
subTesting <- trainingset[-subsamples, ]
dim(subTraining)
dim(subTesting)
head(subTraining)
head(subTesting)

# look at data
plot(subTraining$classe, col="blue", main="Bar Plot of levels of the variable classe within the subTraining data set", xlab="classe levels", ylab="Frequency")

#create first model
model1 <- rpart(classe ~ ., data=subTraining, method="class")

# Predicting
prediction1 <- predict(model1, subTesting, type = "class")

# Plot of the Decision Tree
rpart.plot(model1, main="Classification Tree", extra=102, under=TRUE, faclen=0)

#Test results
confusionMatrix(prediction1, subTesting$classe)

#create second model
model2 <- randomForest(classe ~. , data=subTraining, method="class")

# Predicting
prediction2 <- predict(model2, subTesting, type = "class")

# Test results on subTesting data set:
confusionMatrix(prediction2, subTesting$classe)

#submission
predictfinal <- predict(model2, testingset, type="class")
predictfinal

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(predictfinal)
