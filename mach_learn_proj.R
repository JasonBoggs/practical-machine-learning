#get data from internet
trainURL<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testURL <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(trainURL, "pml-training.csv",  method="curl")
download.file(testURL, "pml-testing.csv",  method="curl")

#read data into R
pml_training <- read.csv("./pml-training.csv", na.strings = c("NA", ""))
pml_testing <- read.csv("./pml-testing.csv", na.strings = c("NA", ""))

#review training dataframe
dim(pml_training)
str(pml_training)
summary(pml_training)

#data clean
training_na_values = sapply(pml_training, function(x) {sum(is.na(x))})
table(training_na_values)
# 60 columns with 0 na, 100 with majority NAs, so remove those
na_columns <- colSums(is.na(pml_training))        
columns_to_remove <- (na_columns >= 19216)             
pml_training_cleaned <- pml_training[!columns_to_remove]
#remove first 7 columns that are for participant identification, not useful for
#predicting the dumbell movement type
pml_training_cleaned = pml_training_cleaned[,-c(1:7)]
dim(pml_training_cleaned)
#now we are left with 53 variable, 52 for predictors and 1 response ("classe")

#show classe variable plot - non-linear, RF model needed
summary(pml_training_cleaned$classe)
plot(pml_training_cleaned$classe)

#split data into test/train
library(caret)
library(randomForest)
library(e1071)
set.seed(100)
train_index <- createDataPartition(y=pml_training_cleaned$classe, p=0.7, list=FALSE)
training <- pml_training_cleaned[train_index,]
testing <- pml_training_cleaned[-train_index,]

#build RF model, automatically usesd cross validation
classe_prediction <- train(training$classe ~ .,
                           data = training,
                           method = "rf",
                           prox = TRUE)

saveRDS(classe_prediction, "class_pred_model.RDS")
print(classe_prediction)

#estimated in-sample accuracy
classe_prediction_train <- predict(classe_prediction, training)
confusionMatrix(classe_prediction_train, training$classe)

#estimated out-of-sample acccuracy
classe_prediction_test <- predict(classe_prediction, test)
confusionMatrix(classe_prediction_test, test$classe)

#run same data cleaning methods from pml_training against pml_testing: 20 test cases
testing_na_values = sapply(pml_testing, function(x) {sum(is.na(x))})
table(testing_na_values)
# 60 columns with 0 na, 100 with majority NAs, so remove those
na_columns_tst <- colSums(is.na(pml_testing))        
columns_to_remove_tst <- (na_columns >= 20)             
pml_testing_cleaned <- pml_testing[!columns_to_remove_tst]
#remove first 7 columns that are for participant identification, not useful for
#predicting the dumbell movement type
pml_testing_cleaned = pml_testing_cleaned[,-c(1:7)]
dim(pml_testing_cleaned)
#again we are left with 53 variable, 52 for predictors and 1 response ("classe")

#predictions for final submission using 20 test cases
answers <- predict(classe_prediction, pml_testing)
#convert answers to character vector for submission per instructions
answers <- as.character(answers)

#submission function from Prediction Assignment Submission instructions 
pml_write_files = function(x) {
  n = length(x)
  for (i in 1:n) {
    filename = paste0("problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
                col.names = FALSE)
  }
}

pml_write_files(answers)