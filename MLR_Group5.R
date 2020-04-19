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

#Model-1
multinom_1<-multinom(label~.,data=fault_train)
summary(multinom_1) #AIC: 2486.983 
multinom_1_pred<-predict(multinom_1,fault_test[-27])
CrossTable(multinom_1_pred,fault_test$label,max.width = 1)
confusionMatrix(multinom_1_pred,fault_test$label)
# Accuracy : 0.7221 

#model-2
multinom_2<-multinom(label~Steel_Plate_Thickness+Outside_X_Index+LogOfAreas+Log_X_Index+Length_of_Conveyer+Maximum_of_Luminosity+Edges_Index+SteelType+Log_Y_Index+X_Minimum,data=fault_train)
summary(multinom_2) #AIC: 2301.229 
multinom_2_pred<-predict(multinom_2,fault_test[-27])
confusionMatrix(multinom_2_pred,fault_test$label)
#accuracy=68.4%

#model-3
multinom_3<-multinom(label~Steel_Plate_Thickness+Outside_X_Index+LogOfAreas+Log_X_Index+Length_of_Conveyer+Maximum_of_Luminosity+Edges_Index+SteelType+Log_Y_Index+X_Minimum+Y_Minimum,data=fault_train)
summary(multinom_3) #AIC: 2275.065 
multinom_3_pred<-predict(multinom_3,fault_test[-27])
confusionMatrix(multinom_3_pred,fault_test$label)
#accuracy=69.13%


#model-4
multinom_4<-multinom(label~Steel_Plate_Thickness+Outside_X_Index+LogOfAreas+Log_X_Index+Length_of_Conveyer+Maximum_of_Luminosity+Edges_Index+SteelType+Log_Y_Index+X_Minimum+Y_Minimum+X_Minimum*LogOfAreas,data=fault_train)
summary(multinom_4) #AIC: 2241.909 
multinom_4_pred<-predict(multinom_2,fault_test[-27])
confusionMatrix(multinom_4_pred,fault_test$label)
#Accuracy : 0.6895 

#Model-5
multinom_5<-multinom(label~Steel_Plate_Thickness+Outside_X_Index+LogOfAreas+Log_X_Index+Length_of_Conveyer+Maximum_of_Luminosity+Edges_Index+SteelType+Log_Y_Index+X_Minimum+Y_Minimum+X_Minimum*LogOfAreas+Steel_Plate_Thickness*LogOfAreas,data=fault_train)
summary(multinom_5) #AIC: 2246.754
multinom_5_pred<-predict(multinom_5,fault_test[-27])
confusionMatrix(multinom_5_pred,fault_test$label)
#Accuracy : 0.6895

#Model-6
multinom_6<-multinom(label~Steel_Plate_Thickness+Outside_X_Index+LogOfAreas+Log_X_Index+Length_of_Conveyer+Maximum_of_Luminosity+Edges_Index+SteelType+Log_Y_Index+X_Minimum+Y_Minimum+X_Minimum*LogOfAreas+Steel_Plate_Thickness*LogOfAreas+Length_of_Conveyer*LogOfAreas,data=fault_train)
summary(multinom_6) #AIC: 2247.727 
multinom_6_pred<-predict(multinom_6,fault_test[-27])
confusionMatrix(multinom_6_pred,fault_test$label)
#Accuracy : 0.6792

#Model-7
multinom_7<-multinom(label~Steel_Plate_Thickness+Outside_X_Index+LogOfAreas+Log_X_Index+Length_of_Conveyer+Maximum_of_Luminosity+Edges_Index+SteelType+Log_Y_Index+X_Minimum+Y_Minimum+Square_Index,data=fault_train)
summary(multinom_7) #AIC: 2173.347
multinom_7_pred<-predict(multinom_7,fault_test[-27])
confusionMatrix(multinom_7_pred,fault_test$label)
#Accuracy : 0.6861


#Model-8
multinom_8<-multinom(label~Steel_Plate_Thickness+Outside_X_Index+LogOfAreas+Log_X_Index+Length_of_Conveyer+Maximum_of_Luminosity+Edges_Index+SteelType+Log_Y_Index+X_Minimum+Y_Minimum+Square_Index+Orientation_Index,data=fault_train)
summary(multinom_8) #AIC: 2155.391 
multinom_8_pred<-predict(multinom_8,fault_test[-27])
confusionMatrix(multinom_8_pred,fault_test$label)
#Accuracy : 0.7101 


#Model-9
multinom_9<-multinom(label~Steel_Plate_Thickness+Outside_X_Index+LogOfAreas+Log_X_Index+Length_of_Conveyer+Maximum_of_Luminosity+Edges_Index+SteelType+Log_Y_Index+X_Minimum+Y_Minimum+Square_Index+Orientation_Index+SigmoidOfAreas,data=fault_train)
summary(multinom_9) #AIC: 2153.189 
multinom_9_pred<-predict(multinom_9,fault_test[-27])
confusionMatrix(multinom_9_pred,fault_test$label)
#Accuracy : 0.7067

#Model-10
multinom_10<-multinom(label~Steel_Plate_Thickness+Outside_X_Index+LogOfAreas+Log_X_Index+Length_of_Conveyer+Maximum_of_Luminosity+Edges_Index+SteelType+Log_Y_Index+X_Minimum+Y_Minimum+Square_Index+Orientation_Index+Empty_Index,data=fault_train)
summary(multinom_10) #AIC: 2151.994 
multinom_10_pred<-predict(multinom_10,fault_test[-27])
confusionMatrix(multinom_10_pred,fault_test$label)
# Accuracy : 0.7033 #adding Empty_Index 

#Model-11
multinom_11<-multinom(label~Steel_Plate_Thickness+Outside_X_Index+LogOfAreas+Log_X_Index+Length_of_Conveyer+Maximum_of_Luminosity+Edges_Index+SteelType+Log_Y_Index+X_Minimum+Y_Minimum+Square_Index+Orientation_Index+Pixels_Areas,data=fault_train)
summary(multinom_11) #AIC: 2155.549 
multinom_11_pred<-predict(multinom_11,fault_test[-27])
confusionMatrix(multinom_11_pred,fault_test$label)
# Accuracy : 0.705 

#Model-12
multinom_12<-multinom(label~Steel_Plate_Thickness+Outside_X_Index+LogOfAreas+Log_X_Index+Length_of_Conveyer+Maximum_of_Luminosity+Edges_Index+SteelType+Log_Y_Index+X_Minimum+Y_Minimum+Square_Index+Orientation_Index+Pixels_Areas+Luminosity_Index,data=fault_train)
summary(multinom_12) #AIC: 2108.211 
multinom_12_pred<-predict(multinom_12,fault_test[-27])
confusionMatrix(multinom_12_pred,fault_test$label)
# Accuracy : 0.7153 

#Model-13
multinom_13<-multinom(label~Steel_Plate_Thickness+Outside_X_Index+LogOfAreas+Log_X_Index+Length_of_Conveyer+Maximum_of_Luminosity+Edges_Index+SteelType+Log_Y_Index+X_Minimum+Y_Minimum+Square_Index+Orientation_Index+Pixels_Areas+Luminosity_Index+X_Maximum,data=fault_train)
summary(multinom_13) #AIC: 2116.012 
multinom_13_pred<-predict(multinom_13,fault_test[-27])
confusionMatrix(multinom_13_pred,fault_test$label)
# Accuracy : 0.7204  

#Model-14
multinom_14<-multinom(label~Steel_Plate_Thickness+Outside_X_Index+LogOfAreas+Log_X_Index+Length_of_Conveyer+Maximum_of_Luminosity+Edges_Index+SteelType+Log_Y_Index+X_Minimum+Y_Minimum+Square_Index+Orientation_Index+Pixels_Areas+Luminosity_Index+X_Maximum+X_Minimum*X_Maximum,data=fault_train)
summary(multinom_14) #AIC: 2121.924
multinom_14_pred<-predict(multinom_14,fault_test[-27])
confusionMatrix(multinom_14_pred,fault_test$label)
# Accuracy : 0.7238 

#Model-15
multinom_15<-multinom(label~Steel_Plate_Thickness+Outside_X_Index+LogOfAreas+Log_X_Index+Length_of_Conveyer+Maximum_of_Luminosity+Edges_Index+SteelType+Log_Y_Index+X_Minimum+Y_Minimum+Square_Index+Orientation_Index+Pixels_Areas+Luminosity_Index+X_Maximum+X_Minimum*X_Maximum+Y_Maximum,data=fault_train)
summary(multinom_15) #AIC: 2116.719
multinom_15_pred<-predict(multinom_15,fault_test[-27])
confusionMatrix(multinom_15_pred,fault_test$label)
# Accuracy : 0.7187

#Model-16
multinom_16<-multinom(label~Steel_Plate_Thickness+Outside_X_Index+LogOfAreas+Log_X_Index+Length_of_Conveyer+Maximum_of_Luminosity+Edges_Index+SteelType+Log_Y_Index+X_Minimum+Y_Minimum+Square_Index+Orientation_Index+Pixels_Areas+Luminosity_Index+X_Maximum+X_Minimum*X_Maximum+Y_Maximum+Pixels_Areas,data=fault_train)
summary(multinom_16) #AIC: 4648.017 /++ pixels aread,AIC: 2116.719
multinom_16_pred<-predict(multinom_16,fault_test[-27])
confusionMatrix(multinom_16_pred,fault_test$label)
# Accuracy : 0.3705 (adding Y_Minimum*Y_Maximum)//Accuracy : 0.7187 

#Model-17(Remove interaction +X_Minimum*X_Maximum)
multinom_17<-multinom(label~Steel_Plate_Thickness+Outside_X_Index+LogOfAreas+Log_X_Index+Length_of_Conveyer+Maximum_of_Luminosity+Edges_Index+SteelType+Log_Y_Index+X_Minimum+Y_Minimum+Square_Index+Orientation_Index+Pixels_Areas+Luminosity_Index+X_Maximum+Y_Maximum+Minimum_of_Luminosity,data=fault_train)
summary(multinom_17) #AIC: 2099.273 
multinom_17_pred<-predict(multinom_17,fault_test[-27])
confusionMatrix(multinom_17_pred,fault_test$label)
# Accuracy : 0.729 

#Model-18
multinom_18<-multinom(label~Steel_Plate_Thickness+Outside_X_Index+LogOfAreas+Log_X_Index+Length_of_Conveyer+Maximum_of_Luminosity+Edges_Index+SteelType+Log_Y_Index+X_Minimum+Y_Minimum+Square_Index+Orientation_Index+Pixels_Areas+Luminosity_Index+X_Maximum+Y_Maximum+Minimum_of_Luminosity+Y_Perimeter,data=fault_train)
summary(multinom_18) #AIC: 2196.181 
multinom_18_pred<-predict(multinom_18,fault_test[-27])
confusionMatrix(multinom_18_pred,fault_test$label)
#  Accuracy : 0.7307


#to get pvalues
stargazer(multinom_1, type="text", out="multi1.txt")
stargazer(multinom_2, type="text", out="multi1.txt")
stargazer(multinom_3, type="text", out="multi1.txt")
stargazer(multinom_4, type="text", out="multi1.txt")
stargazer(multinom_5, type="text", out="multi1.txt")
stargazer(multinom_6, type="text", out="multi1.txt")
stargazer(multinom_7, type="text", out="multi1.txt")
stargazer(multinom_8, type="text", out="multi1.txt")
stargazer(multinom_9, type="text", out="multi1.txt")
stargazer(multinom_9, type="text", out="multi1.txt")
stargazer(multinom_11, type="text", out="multi1.txt")
stargazer(multinom_12, type="text", out="multi1.txt")
stargazer(multinom_13, type="text", out="multi1.txt")
stargazer(multinom_14, type="text", out="multi1.txt")
stargazer(multinom_15, type="text", out="multi1.txt")
stargazer(multinom_16, type="text", out="multi1.txt")
stargazer(multinom_17, type="text", out="multi1.txt")
stargazer(multinom_18, type="text", out="multi1.txt")


#Accuracy
#sum(multinom_1_pred==fault_test$label)/nrow(fault_test)*100
