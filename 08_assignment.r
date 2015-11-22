# ============================================================================= #
# Assignment Practical Machine Learning
# ============================================================================= #

# set working directory

.libPaths("D:/Robert_R/Rpackages")
setwd("D:/Coursera/DataScientist/08_assignment/")
list.files()
# ============================================================================= #
# loading packages
# ============================================================================= #

library(caret)
# library(Hmisc)
library(dplyr)
# library(data.table)
set.seed(5580)

# ============================================================================= #
# prep for uploading answers ----
# ============================================================================= #

answers = rep("A", 20)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)

# ============================================================================= #
# data input ----
# ============================================================================= #

#raw data
Data <- as.data.frame(data.table::fread("pml-training.csv", na.strings=c("NA", "", "#DIV/0!"), 
              strip.white=T, stringsAsFactors = TRUE))
dim(Data)

# the one for submission
TestingOrg <- as.data.frame(data.table::fread("pml-testing.csv", na.strings=c("NA", "", "#DIV/0!"),
                    strip.white=T, stringsAsFactors = TRUE))
dim(TestingOrg)

# varschar <- names(Data[, as.vector(which(sapply(Data, class) == "factor"))])

# ============================================================================= #
# data cleaning ----
# ============================================================================= #

# 160 variables
names(Data)

# remove irrelevant variables
# time stamps, username etc.
Data <- select(Data, -contains("timestamp"), -V1, -user_name, -contains("window"))

# lets find out the rate of missingness
isNA <- apply(Data, 2, function(x) { sum(is.na(x)) / nrow(Data) })
table(isNA)

# only 53 variables have no NA's (or any plausible rate of missingness)
Data <- Data[ , names(isNA[isNA==0])]
dim(Data)

# # identify numeric variables
# varsnum <- names(Data[sapply(Data, is.numeric)])
# # "classe" is the only factor variable

# # check multi-collinearity
# corrMatrix <- cor(Data[, varsnum])
# corrDF <- expand.grid(row = 1:nrow(corrMatrix), 
#                       col = 1:nrow(corrMatrix))
# corrDF$correlation <- as.vector(corrMatrix)
# levelplot(correlation ~ row + col, corrDF)
# 
# highcor <- findCorrelation(corrMatrix, cutoff = .90, 
#                              verbose = TRUE, exact = TRUE)
# names(Data)[highcor]
# 
# Data <- Data[, -highcor]


# split into training and test data set
inTrain <- createDataPartition(Data$classe, p = 0.7, list = FALSE)
training <- Data[inTrain,]
testing <- Data[-inTrain,]

dim(training);dim(testing)

# ============================================================================= #
# Train a random forest ----
# Random forests build lots of bushy trees, and then average them to reduce 
# the variance (Bagging: Bootstrap aggregating)
# ============================================================================= #

parallel::detectCores() # how many cores do I hv?
ctrl <- trainControl(allowParallel=T, method="cv", number=2)

model <- train(classe ~ ., data = training, model="rf", trControl=ctrl)
pred <- predict(model, newdata = testing)

# Out-of-sample-error?
sum(pred == testing$classe) / length(pred) # 0.9898046
confusionMatrix(testing$classe, pred)$table

predict(model, newdata=TestingOrg)
answers <- predict(model, TestingOrg)
answers

# Which variables are most important in this model?
top10 <- varImp(model)$importance
top10 <- mutate(top10, vars = row.names(top10)) %>%
  arrange(desc(Overall))

print(top10 <- top10$vars[1:10])

# ============================================================================= #
# Train a smaller random forest ----
# ============================================================================= #

smallData <- training[, c( "classe", top10)]
smallModel <- train(classe ~ ., data = smallData, model = "rf", trControl=ctrl)
smallPred <- predict(smallModel, newdata = testing)

# Out-of-sample-error for model with 10 variables
1- sum(smallPred == testing$classe) / length(smallPred) # 0.0173322
# nice


confusionMatrix(testing$classe, smallPred)$table
save.image("08_assignment.rdata")



