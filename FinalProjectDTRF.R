#explore data
faultsdata <- read.csv("faults_recode.csv")
str(faultsdata)
summary(faultsdata)

library(C50)


#as.factor things that need to be cleaned 

faultsdata$TypeOfSteel_A300 <- as.factor(faultsdata$TypeOfSteel_A300)
faultsdata$TypeOfSteel_A400 <- as.factor(faultsdata$TypeOfSteel_A400)
faultsdata$label <- as.factor(faultsdata$label)

table(faultsdata$label)

#get rid of y values other than label column 
faultsdata <- faultsdata[-34:-28]
str(faultsdata)

#randomize faultsdata
set.seed(12345)
faultsdata <- faultsdata[order(runif(1491)),]

#divide data by test and train
faultsdata_train <- faultsdata[1:1359,]
faultsdata_test <- faultsdata[1360:1491,]

#build decision tree model 
faults_model <- C5.0(faultsdata_train[,-28], faultsdata_train$label)
str(faults_model)
plot(faults_model)

#predict fault types 
faults_pred <- predict(faults_model,faultsdata_test[,-28])
table(faults_pred)
table(faultsdata_test$label)

#cross tabulation of predicted vs actual values 
library(gmodels)
CrossTable(faultsdata_test$label, faults_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual faults', 'predicted faults'))

#improve accuracy or reliability (give cost matrix dpending on if false positive or false negative is better)
faults_boost12 <- C5.0(faultsdata_train[,-28], faultsdata_train$label, trials = 12)
faults_boost12

summary(faults_boost12)
faults_boost12_pred <- predict(faults_boost12, faultsdata_test)
CrossTable(faultsdata_test$label, faults_boost12_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual label', 'label predictions'))

#Random forest 
library(randomForest)
set.seed(300)
rf <- randomForest(label ~ ., data = faultsdata_train)
rf

#comment





