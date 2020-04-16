library(tidyverse)
library(e1071) 
library(caret)

tdata = read.csv("faults_recode.csv", header = T)

# recode steel type --------
tdata$SteelType<-ifelse(tdata$TypeOfSteel_A300==1,'A300','A400')
tdata$SteelType<-as.factor(tdata$SteelType)

# select relevant variables -----------
# outcome: sdata$label
sdata = tdata %>% select(-c(Pastry, Z_Scratch, K_Scatch, Stains, Dirtiness, Bumps, Other_Faults, 
                            TypeOfSteel_A300, TypeOfSteel_A400))
sdata$label = as.factor(sdata$label)

# split train and test dataset ------------
train_ind = sample(seq_len(nrow(sdata)), size = as.integer(dim(sdata)[1]*0.7))

train = sdata[train_ind,]
test = sdata[-train_ind,]

# start training using linear kernal -------------
svm.linear = svm(formula = label ~ ., 
                 data = train, 
                 type = 'C-classification', 
                 kernel = 'linear') 

# predict test result
svm.linear.pred = predict(svm.linear, newdata = test %>% select(-label)) 

# confusion matrix
cm.svm.linear = confusionMatrix(svm.linear.pred, as.factor(test[, "label"]))
accuracy.svm.linear = round(as.numeric(cm.svm.linear$overall[1])*100, digits = 2)
pred.table.svm.linear = cm.svm.linear$table


# start training using radial kernal -------------
svm.radial = svm(formula = label ~ ., 
                 data = train, 
                 type = 'C-classification', 
                 kernel = 'radial') 

# predict test result
svm.radial.pred = predict(svm.radial, newdata = test %>% select(-label)) 

# confusion matrix
cm.svm.radial = confusionMatrix(svm.radial.pred, as.factor(test[, "label"]))
accuracy.svm.radial = round(as.numeric(cm.svm.radial$overall[1])*100, digits = 2)
pred.table.svm.radial = cm.svm.radial$table
