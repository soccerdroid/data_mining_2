#Reemplazar con ruta de directorio de trabajo
setwd("/home/belen/Downloads")
library(caret)
library(ROCR)
library(forecast)
#Reemplazar con ruta a donde está el archivo csv
cancer_df<-read.csv("cancer_train.csv")
cancerData <- preProcess(x = cancer_df, method = c("center", "scale")) 
cancerData <- predict(cancerData, cancer_df) #stored

cancerData$tipo2 <- 0
cancerData$tipo2[cancerData$tipo=="M"]<-1
cancerData$tipo2[cancerData$tipo=="B"]<-0
cancerData$tipo<-NULL

set.seed(1) #for reproductibility of results
trainIndex <- createDataPartition(cancerData$tipo2, 
                                  times = 1,
                                  p = .80,
                                  list = FALSE)
cancerTrain <- cancerData[trainIndex,]
cancerTest <- cancerData[-trainIndex,]


model <- glm(tipo2 ~ radio +simetria, 
             family = binomial(link = "logit"), 
             data = cancerTrain)
summary(model)


#cutoff vals
thresholds = c(0.1,0.5,0.9)
for (threshold in thresholds) {
  result <- predict(model,newdata=cancerTest,type='response')
  result <- ifelse(result > threshold,1,0)
  actual_values<-cancerTest$tipo2
  conf_matrix<-table(result,actual_values)
  sensitivity = sensitivity(conf_matrix)
  specificity = specificity(conf_matrix)
  accuracy = conf_matrix[1,1]+conf_matrix[2,2]/sum(conf_matrix)
  print(paste0("Threshold ", threshold))
  print(paste0("sensitivity: ", sensitivity))
  print(paste0("specificity: ", specificity))
  print(paste0("accuracy: ", accuracy))
}

for(cutoff in 1:99){
  cu
}
#test error
for(threshold in thresholds){
  predicted_values<-ifelse(predict(model, newdata = cancerTest,type="response")>threshold,1,0)
  errors <- accuracy(model$y,cancerTest$tipo2)
  print(paste0("Threshold ", threshold))
  print(errors)
}

#train error
for(threshold in thresholds){
  predicted_values<-ifelse(predict(model, newdata = cancerTrain,type="response")>threshold,1,0)
  errors <- accuracy(predicted_values,cancerTrain$tipo2)
  print(paste0("Threshold ", threshold))
  print(errors)
}

#tema2

