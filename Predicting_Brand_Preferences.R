####################################################################
####################################################################
# Title: UT CPE Data Science -- Predicting Customer Brand Preferences
# Date: 12Sept2016
# Authors@R: person("Ben Fauber", "author",
#                  role = c("aut", "cre"))
# Description: Explores several classifier models for analysis of existing 
# product data and applies them to hold-out data, followed by application to 
# new product data to predict customer brand preferences.
####################################################################
####################################################################

# install packages, load libraries

install.packages("caret")
install.packages("e1071") 
install.packages("randomForest")
install.packages("C50")
install.packages("plyr")
install.packages("doMC")
install.packages("pROC")
install.packages("mlbench")
install.packages("ggplot2")
library(caret)
library(e1071)
library(randomForest)
library(C50)
library(plyr)
library(doMC)
library(pROC)
library(mlbench)
library(ggplot2)

registerDoMC(cores = 4)


####################################################################
# load the TRAINING datafile as a CSV file
####################################################################

srComp <- read.csv("~/Documents/DataSci/UT DataAnalyst/Course 2 Predicting Profitability/Task3/Raw Data/Survey_Responses_arff/Survey_Responses_Complete.csv")

# search for NA values

na.fail(srComp)

# omit NA values

srComp <- na.exclude(srComp)

# retrieve column names

colnames(srComp)

# omit unique identifiers that are not of use in models

# none found in dataset, pass

# convert remaining num and int columns to numeric
# require manual assessment of relevant cols using str()

str(srComp)

for(i in c(2,3,4,5,7:ncol(srComp))) 
{
    srComp[,i] <- as.numeric(as.character(srComp[,i]))
}

str(srComp)

# convert classifier (Y) column to factor format

for(i in c(7:ncol(srComp))) 
{
    srComp[,i] <- as.factor(as.character(srComp[,i]))
}

srComp <- na.exclude(srComp)

str(srComp)

# results in an 100,000 x 7 matrix


####################################################################
# understanding the dataset using Histograms -- AGE
####################################################################
x <- srComp$age
h <- hist(x, breaks=10, col="red", xlab="Age", 
main="Histogram with Normal Curve")
xfit <- seq(min(x),max(x),length=40)
yfit <- dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2) 

####################################################################
# understanding the dataset using Histograms -- EDUCATION LEVEL
####################################################################
x <- srComp$elevel
h <- hist(x, breaks=10, col="red", xlab="Education Level", 
main="Histogram with Normal Curve")
xfit <- seq(min(x),max(x),length=40)
yfit <- dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2) 

####################################################################
# understanding the dataset using Histograms -- CAR
####################################################################
x <- srComp$car
h <- hist(x, breaks=10, col="red", xlab="Car", 
main="Histogram with Normal Curve")
xfit <- seq(min(x),max(x),length=40)
yfit <- dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2) 


####################################################################
# understanding the dataset using Histograms -- ZIP
####################################################################
x <- srComp$zipcode
h <- hist(x, breaks=10, col="red", xlab="Zip Code", 
main="Histogram with Normal Curve")
xfit <- seq(min(x),max(x),length=40)
yfit <- dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2) 

####################################################################
# understanding the dataset with Statistics -- SALARY
####################################################################
statsSalary <- summary(srComp$salary)
print(statsSalary)

####################################################################
# understanding the dataset with Statistics -- CREDIT
####################################################################
statsCredit <- summary(srComp$credit)
print(statsCredit)


####################################################################
# building the train and test (a.k.a. hold-out) datasets
####################################################################

# dataframe = srComp
# Y Value = brand

# define an 40%/60% train/test split of the dataset

set.seed(23)

inTraining <- createDataPartition(srComp$brand, p = .4, list = FALSE)

training <- srComp[inTraining,]

testing <- srComp[-inTraining,]

# count of total values in each dataset

trainingN <- nrow(training)*ncol(training)

testingN <- nrow(testing)*ncol(testing)


####################################################################
# build a kNN Classification model with Caret
####################################################################

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

set.seed(23)

ptm <- proc.time()

class1fit <- train(brand ~., data=training, method="knn", metric="Accuracy", trControl=fitControl, tuneLength=20, preProc=c("range"), na.action=na.omit)

proc.time() - ptm

#list the predictor column names and summarize the model

predictors(class1fit)
summary(class1fit)
print(class1fit)
print(class1fit$bestTune)

class1fitImp <- varImp(class1fit)
plot(class1fitImp, main=class1fit$method)


####################################################################
# build a c5.0 Classification model with Caret
####################################################################

set.seed(23)

ptm <- proc.time()

class2fit <- train(brand ~., data=training, metric="Accuracy", trControl=fitControl, method="C5.0", tuneLength=10, verbose=FALSE)

proc.time() - ptm

#list the predictor column names and summarize the model

predictors(class2fit)
summary(class2fit)
print(class2fit)
print(class2fit$bestTune)

class2fitImp <- varImp(class2fit)
plot(class2fitImp, main=class2fit$method)


####################################################################
# build a SVM Classification model using e1071
####################################################################

# tuning the e1071 SVM model against training data

#####
# Could not tune with large training set (>=4,000 rows); model did not converge
# Used training set of 1,000 rows to arrive at optimal model parameters
#####

svmGrid3 = exp(log(10)*seq(log10(0.001),log10(100),by=1))

set.seed(23)

class3opt <- tune.svm(brand ~ ., data=training, metric="Accuracy", sampling="fix", kernel="linear", gamma = svmGrid3, cost = svmGrid3, trControl=fitControl)

summary(class3opt)
print(class3opt)
print(class3opt$best.parameters)

set.seed(23)

class3fit <- svm(brand ~ ., data=training, sampling = "fix", kernel="linear", metric="Accuracy", gamma=0.001, cost = 0.001, trControl = fitControl, na.action=na.omit)

predictors(class3fit)
summary(class3fit)

class3fitImp <- varImp(class3fit)
plot(class3fitImp, main=class3fit$method)


# polynomial kernel SVM model tuning

svmGrid4 = exp(log(10)*seq(log10(0.01),log10(10),by=1))

set.seed(23)

class4opt <- tune.svm(brand ~ ., data=training, metric="Accuracy", sampling="fix", kernel="poly", gamma = svmGrid4, cost = svmGrid4, trControl=fitControl)

# did not converge, thus dropped opt/training, used default parameters

# summary(class4opt)
# print(class4opt)
# print(class4opt$best.parameters)

set.seed(23)

class4fit <- svm(brand ~ ., data=training, sampling = "fix", kernel="poly", metric="Accuracy", gamma=0.1, cost = 1, trControl = fitControl, na.action=na.omit)

predictors(class4fit)
summary(class4fit)

class4fitImp <- varImp(class4fit)
plot(class4fitImp, main=class4fit$method)


# radial kernel SVM model tuning

svmGrid5 = exp(log(10)*seq(log10(0.001),log10(100),by=1))

set.seed(23)

class5opt <- tune.svm(brand ~ ., data=training, metric="Accuracy", sampling="fix", kernel="radial", gamma= svmGrid5, cost = svmGrid5, trControl=fitControl)

summary(class5opt)
print(class5opt)
print(class5opt$best.parameters)

set.seed(23)

class5fit <- svm(brand ~ ., data=training, sampling = "fix", kernel="radial", metric="Accuracy", gamma=0.1, cost = 100, trControl = fitControl, na.action=na.omit)

predictors(class5fit)
summary(class5fit)

class5fitImp <- varImp(class5fit)
plot(class5fitImp, main=class5fit$method)


####################################################################
# build a RandomForest (RF) Classification model using Caret
####################################################################

# optimize the mtry RF parameter

# default mtry in RF is sqrt(ncol(training))

sMt <- round(sqrt(ncol(training)), digits=0)

# bracket mtry parameters, start with grid min = sMt/2 and grid max = 2*sMt

rfGrid6 <- expand.grid(.mtry = c(sMt/2, sMt, sMt*2))

set.seed(23)

ptm <- proc.time()

class6fit <- train(brand ~., data=training, method="rf", metric="Accuracy", trControl=fitControl, ntree=500, tuneGrid=rfGrid6, importance = TRUE, na.action=na.omit)

proc.time() - ptm

predictors(class6fit)
summary(class6fit)
print(class6fit$bestTune)

class6fitImp <- varImp(class6fit)
plot(class6fitImp, main=class6fit$method)


####################################################################
# General Functions for STATS
####################################################################

# rounds numbers if numeric

round_numeric <- function(lst, decimals=2) {
    lapply(lst, function(x) {
        if (is.numeric(x)) {
            x <- round(x, decimals)
        }
        x
        })
}

# summary of model stats using a Confusion Matrix as input

sumMod <- function(cm, model) {
    sumM <- list(k=model$finalModel$k,
                 metric=model$metric,
                 value=model$results[model$results$k == model$finalModel$k, model$metric],
                 TN=cm$table[1,1],  # true negatives
                 TP=cm$table[2,2],  # true positives
                 FN=cm$table[1,2],  # false negatives
                 FP=cm$table[2,1],  # false positives
                 acc=cm$overall["Accuracy"],  # accuracy
                 pre=cm$byClass["Precision"], #precision
                 rec=cm$byClass["Recall"], #recall
                 sens=cm$byClass["Sensitivity"],  # sensitivity
                 spec=cm$byClass["Specificity"],  # specificity
                 pvalue=cm$$overall["McnemarPValue"], # McNemar P-value
                 PPV=cm$byClass["Pos Pred Value"], # positive predictive value
                 NPV=cm$byClass["Neg Pred Value"]) # negative predictive value
    round_numeric(sumM)
}

    
####################################################################
# applying the CLASS models against the TRAINING dataset
####################################################################

class1pred <- data.frame(predict(class1fit, training, interval = "predict", level =0.95))

class2pred <- data.frame(predict(class2fit, training, interval = "predict", level =0.95))

class3pred <- data.frame(predict(class3fit, training, interval = "predict", level =0.95))

class4pred <- data.frame(predict(class4fit, training, interval = "predict", level =0.95))

class5pred <- data.frame(predict(class5fit, training, interval = "predict", level =0.95))

class6pred <- data.frame(predict(class6fit, training, interval = "predict", level =0.95))


####################################################################
# combine all CLASS model stats into a single data frame
####################################################################

cmat1 <- confusionMatrix(class1pred[,1], training$brand)
cmat2 <- confusionMatrix(class2pred[,1], training$brand)
cmat3 <- confusionMatrix(class3pred[,1], training$brand)
cmat4 <- confusionMatrix(class4pred[,1], training$brand)
cmat5 <- confusionMatrix(class5pred[,1], training$brand)
cmat6 <- confusionMatrix(class6pred[,1], training$brand)

# summary of TRAINING results and metrics

model_comp <- as.data.frame(
    rbind(sumMod(cmat1, class1fit),
          sumMod(cmat2, class2fit),
          sumMod(cmat3, class3fit),
          sumMod(cmat4, class4fit),
          sumMod(cmat5, class5fit),
          sumMod(cmat6, class6fit)))

ModelName <- c("kNN", "c5.0", "e1071 linear", "e1071 poly", "e1071 radial", "RF")

model_comp <- data.frame(cbind(ModelName, model_comp))
rownames(model_comp) <- NULL

print(model_comp)


####################################################################
# apply the CLASS models against the TESTING (hold-out) dataset
####################################################################

class1predTest <- data.frame(predict(class1fit, testing, interval = "predict", level =0.95))

class2predTest <- data.frame(predict(class2fit, testing, interval = "predict", level =0.95))

class3predTest <- data.frame(predict(class3fit, testing, interval = "predict", level =0.95))

class4predTest <- data.frame(predict(class4fit, testing, interval = "predict", level =0.95))

class5predTest <- data.frame(predict(class5fit, testing, interval = "predict", level =0.95))

class6predTest <- data.frame(predict(class6fit, testing, interval = "predict", level =0.95))


####################################################################
# combine all CLASS model TESTING (hold-out) stats into a single data frame
####################################################################

cmat1T <- confusionMatrix(class1predTest[,1], testing$brand)
cmat2T <- confusionMatrix(class2predTest[,1], testing$brand)
cmat3T <- confusionMatrix(class3predTest[,1], testing$brand)
cmat4T <- confusionMatrix(class4predTest[,1], testing$brand)
cmat5T <- confusionMatrix(class5predTest[,1], testing$brand)
cmat6T <- confusionMatrix(class6predTest[,1], testing$brand)

model_compT <- as.data.frame(
    rbind(sumMod(cmat1T, class1fit),
          sumMod(cmat2T, class2fit),
          sumMod(cmat3T, class3fit),
          sumMod(cmat4T, class4fit),
          sumMod(cmat5T, class5fit),
          sumMod(cmat6T, class6fit)))

ModelName <- c("kNN TEST", "c5.0 TEST", "e1071 linear TEST", "e1071 poly TEST", "e1071 radial TEST", "RF TEST")

model_compT <- data.frame(cbind(ModelName, model_compT))
rownames(model_compT) <- NULL

print(model_compT)


####################################################################
# measure the model performance on the TRAINING vs TESTING (hold-out) data
####################################################################

# merge the training and testing datasets into one table

allStats <- data.frame(rbind(model_comp, model_compT))
row.names(allStats) <- NULL 

print(allStats)

# write an output file for any downstream plotting/presenting

allStatsS <- allStats
allStatsS$k <- NULL
allStatsS$value <- NULL
allStatsS$metric <- NULL

allStatsS <- as.matrix(allStatsS)

write.csv(allStatsS, file="~/Documents/DataSci/UT DataAnalyst/Course 2 Predicting Profitability/Task3/ModelsAllStats.csv")


####################################################################
# apply the PREFERRED MODEL against the NEW data set
####################################################################

# import the new data set

srIncomp <- read.csv("~/Documents/DataSci/UT DataAnalyst/Course 2 Predicting Profitability/Task3/Raw Data/Survey_Responses_arff/Survey_Responses_Incomplete.csv")

# retrieve column names

colnames(srIncomp)

for(i in c(2,3,4,5,7:ncol(srIncomp))) 
{
    srIncomp[,i] <- as.numeric(as.character(srIncomp[,i]))
}

str(srIncomp)

# convert classifier (Y) column to factor format

for(i in c(7:ncol(srIncomp))) 
{
    srIncomp[,i] <- as.factor(as.character(srIncomp[,i]))
}

str(srIncomp)

# results in an 100,000 x 7 matrix

# retrieve column names

colnames(srComp)

colnames(srIncomp)

#####
# compare the two data.frames to insure that they are identical
#####

# second error check for spelling differences of column names

colnames(srIncomp) <- colnames(training)

str(srIncomp)

#####
# PREFERRED MODEL is RF, c5.0 is second best
#####

# RF model

class6new <- data.frame(predict(class6fit, srIncomp, interval="predict", level=0.95, na.action=na.omit))

summary(class6new)

# c5.0 model

class2new <- data.frame(predict(class2fit, srIncomp, interval = "predict", level =0.95, na.action=na.omit))

summary(class2new)


#list the predictor column names and summarize the models

# RF model

predictors(class6fit)
summary(class6fit)
print(class6fit)
print(class6fit$bestTune)

# c5.0 model

predictors(class2fit)
summary(class2fit)
print(class2fit)
print(class2fit$bestTune)


# write output file

write.csv(class6new, file="~/Documents/DataSci/UT DataAnalyst/Course 2 Predicting Profitability/Task3/NewData_RF_Output.csv")

class6fitImpExp <- as.data.frame((as.data.frame.complex(class6fitImp))[1,1])

write.csv(class6fitImpExp, file="~/Documents/DataSci/UT DataAnalyst/Course 2 Predicting Profitability/Task3/Important_RF_Fields.csv")

write.csv(class2new, file="~/Documents/DataSci/UT DataAnalyst/Course 2 Predicting Profitability/Task3/NewData_c50_Output.csv")

class2fitImpExp <- as.data.frame((as.data.frame.complex(class2fitImp))[1,1])

write.csv(class2fitImpExp, file="~/Documents/DataSci/UT DataAnalyst/Course 2 Predicting Profitability/Task3/Important_c50_Fields.csv")


####################################################################
# INDEX
####################################################################

