## Load Packages

require(downloader)
require(mlbench)
require(caret)
require(randomForest)
require(doSNOW)

## Load Data

folderName <- "data"

if (!file.exists(folderName)){
        message("Creating data folder")
        dir.create(folderName)
} else {message("Data folder found in working directory")}

## Get Data

trainingname <- "data/pml-training.csv"
testingname <- "data/pml-testing.csv"

if (!file.exists("training")){
        message("Downloading training file")
        download(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                 destfile=trainingname)
} else {message("Training file found in working directory")}

if (!file.exists("testing")){
        message("Downloading testing file")
        download(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                 destfile=testingname)
} else {message("Testing file found in working directory")}


# Read data into data frames

if (!exists("training")){
        message("Storing training data in data frame")
        training <- read.table(trainingname, sep = ",", header=TRUE,
                               na.strings=c("#DIV/0!","","NA"))
} else {message("Training data frame found in working directory")}

message("Training data stored as 'training'")

if (!exists("testing")){
        message("Storing testing data in data frame")
        testing <- read.table(testingname, sep = ",", header=TRUE,
                              na.strings=c("#DIV/0!","","NA"))
} else {message("Testing data frame found in working directory")}

message("Testing data stored as 'testing'")

# The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. 
# If you use the document you create for this class for any purpose please cite them as they 
# have been very generous in allowing their data to be used for this kind of assignment

## Data Pre-Processing

### Remove 1 through 7 because of uselessness!

training_nona <- training[,colSums(is.na(training)) == 0]
testing_nona <- testing[,colSums(is.na(training)) == 0]
removeBegVals <- as.integer(1:6)
training_clean <- training_nona[,-removeBegVals]
testing_clean <- testing_nona[,-removeBegVals]


inTrain <- createDataPartition(y=training_clean$classe,
                               p=0.7, list = FALSE)
sub_training <- training_clean[inTrain,]
sub_testing <- training_clean[-inTrain,]

set.seed(1337)

library("doSNOW")
cl<-makeCluster(4) # Assign number of cores you want to use; in this case use 4 cores
registerDoSNOW(cl) # Register the cores.

userControl <-  trainControl(method = "cv", number = 4)
modFit <- train(classe ~.,data = sub_testing, method="rf", trControl = userControl)
modFit

results <- predict(modFit, sub_testing)
OoSError <- 1-(sum(results == sub_testing$classe)/nrow(sub_testing))
OoSError
stopCluster(cl) # Explicitly free up your cores again.

test_results <- predict(modFit, testing_clean)

pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

dir.create("test_case_results")
setwd("test_case_results")

pml_write_files(test_results)
