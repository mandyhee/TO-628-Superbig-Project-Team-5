## Data load and libraries

library(tidyverse)
library(gmodels)
library(naniar)
library(stargazer)

faults <- read.csv("faults_recode.csv")

str(faults)
faults %>% glimpse()
faults %>% miss_var_summary() #No NAs

#Type of steel is merged into a single column and make to factor
faults$SteelType<-ifelse(faults$TypeOfSteel_A300==1,1,0)

# Data split

faults %>% group_by(label) %>% summarise(n=n(),prop=n/nrow(faults))
faults_logit <- faults[c(1:11,14:27,36,35)]
set.seed(40)
train_set =sample(seq_len(nrow(faults_logit)), size = as.integer(dim(faults_logit)[1]*0.7)) 
fault_train = faults_logit[train_set,]
fault_test = faults_logit[-train_set,]

#Proportion of faults in train and test set
fault_train %>% group_by(label) %>% summarise(n=n(),prop=n/nrow(fault_train))
fault_test %>% group_by(label) %>% summarise(n=n(),prop=n/nrow(fault_test))

#NNET-> Multinom

multinom_1<-multinom(label~.,data=fault_train)
summary(multinom_1)
multinom_1_pred<-predict(multinom_1,fault_test[-27])
CrossTable(multinom_1_pred,fault_test$label)

#to get pvalues
stargazer(multinom_1, type="text", out="multi1.txt")

#Accuracy
sum(multinom_1_pred==fault_test$label)/nrow(fault_test)*100
