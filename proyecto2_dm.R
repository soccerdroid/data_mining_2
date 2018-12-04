#Reemplazar con ruta de directorio de trabajo
setwd("/home/belen/Downloads")
library(caret)
library(ROCR)
library(pROC)
library(forecast)
library(ggplot2)
library(reshape)
library(ggthemes)
library(ggthemr)
#Reemplazar con ruta a donde está el archivo csv
cancerTrain<-read.csv("cancer_train.csv")
cancerTest = read.csv("cancer_test.csv")

cancerTrain$radio <- scale(cancerTrain$radio)
cancerTrain$simetria <- scale(cancerTrain$simetria)
cancerTest$radio <- scale(cancerTest$radio)
cancerTest$simetria <- scale(cancerTest$simetria)


cancerTrain$tipo <- sapply(cancerTrain$tipo,switch,"M" = 1, "B" = 0)
cancerTest$tipo <- sapply(cancerTest$tipo,switch,"M" = 1, "B" = 0)


model <- glm(tipo ~ radio +simetria, 
             family = "binomial", 
             data = cancerTrain)
summary(model)

cancerTrain$prediction <- predict(model, newdata = cancerTrain, type = "response" )
cancerTest$prediction  <- predict(model, newdata = cancerTest , type = "response" )

cancerTest$cutoff.1 <- ifelse(cancerTest$prediction > 0.1, 1, 0)
cancerTest$cutoff.5 <- ifelse(cancerTest$prediction > 0.5, 1, 0)
cancerTest$cutoff.9 <- ifelse(cancerTest$prediction > 0.9, 1, 0)

#Confusion matrix
#caret receives (predicted_results, true_results)
conf_matrix1 <- confusionMatrix(factor(cancerTest$cutoff.1),factor(cancerTest$tipo))
conf_matrix5 <- confusionMatrix(factor(cancerTest$cutoff.5),factor(cancerTest$tipo))
conf_matrix9 <- confusionMatrix(factor(cancerTest$cutoff.9),factor(cancerTest$tipo))

conf_matrix1
conf_matrix5
conf_matrix9


#train error
accuracy(cancerTrain$prediction,cancerTrain$tipo)
#test error
accuracy(cancerTest$prediction,cancerTest$tipo)

#check error distribution
m1_t_resid <- cancerTrain$tipo - cancerTrain$prediction
qqnorm(m1_t_resid)
qqline(m1_t_resid, lty = 2)

m2_t_resid <- cancerTest$tipo - cancerTest$prediction
qqnorm(m2_t_resid)
qqline(m2_t_resid, lty = 2)

#tema2
accuracy.vector <- c()
sensitivity.vector <- c()
specificity.vector <- c()
cutoff.vector <- c()

for (cutoff in 1:99){
  cutoff <- cutoff/100
  cutoff.vector <- c( cutoff.vector, cutoff )
  fitted_vals <- ifelse(cancerTest$prediction > cutoff ,1,0)
  misClasificError.temp <- mean(fitted_vals!= cancerTest$tipo)
  accuracy.vector <- c(accuracy.vector, 1 - misClasificError.temp)
  #u <- union(fitted_vals, cancerTest$tipo)
  confMatrix.temp <- table(factor(fitted_vals),factor(cancerTest$tipo))
  specificity.temp <- specificity(confMatrix.temp)
  sensitivity.temp <- sensitivity(confMatrix.temp)
  specificity.vector <- c(specificity.vector, specificity.temp)
  sensitivity.vector <- c(sensitivity.vector, sensitivity.temp)
}


table <- data.frame("Accuracy" =  accuracy.vector, "Sensitivity" = sensitivity.vector , "Specificity"= specificity.vector)
row.names(table) <- cutoff.vector
graph <- melt(table, id.vars = 0, variable_name = 'Error_metric')
graph$cutoff <- as.numeric(as.character(row.names(table)))
graph$value <- as.numeric(as.character(graph$value))

ggplot(data = graph, aes(x = as.numeric(cutoff), y = as.numeric(value), colour = Error_metric)) +
  ggtitle("Cutoff vs Error metrics") +
  geom_line(aes(colour = Error_metric)) +
  ylab("Error metrics")+xlab("Cutoff") + 
  theme_economist()



roc1 <- with(cancerTest,roc(tipo,cutoff.1))
plot(roc1, col="blue", print.auc=TRUE, grid=TRUE, auc.polygon=TRUE, identity.col="gray",auc.polygon.col="white",legend.title="Cutoff value of ")

roc5 <- with(cancerTest,roc(tipo,cutoff.5))
plot(roc5, col="blue", print.auc=TRUE, grid=TRUE, auc.polygon=TRUE, identity.col="gray",auc.polygon.col="white")

roc9 <- with(cancerTest,roc(tipo,cutoff.9))
plot(roc9, col="blue", print.auc=TRUE, grid=TRUE, auc.polygon=TRUE, identity.col="gray",auc.polygon.col="white")

#tema 3
ggplot(data = cancerTest, aes(x = as.numeric(cancerTest$radio), y = as.numeric(cancerTest$simetria), colour = factor(tipo))) +
  ggtitle("Simetría vs radio") +
  ylab("Simetría")+xlab("Radio") + 
  theme_light()+
  geom_point()+
  geom_abline(intercept = coef(model)[1]/(-coef(model)[3]),slope=coef(model)[2]/(-coef(model)[3]),color="blue")
  
#tema 4
library(dummies)
library(dplyr)
#Reemplazar con ruta a donde está el archivo csv
passengers<-read.csv("passengers_data.csv")

#Panorama del dataset
str(passengers)
passengers$Age<-as.numeric(passengers$Age) #change age to numeric
colSums(is.na(passengers))
hist(passengers$Age, col='darkblue',  border="white", 
     ylim=c(0,200), main='Distribución de edad',
     xlab = 'Edad', ylab = 'Frecuencia', xlim=c(0,100))

# Missing values imputation
passengers$Age[is.na(passengers$Age)] <- median(passengers$Age,na.rm=T)
passengers$Embarked[passengers$Embarked==""] <- "S"

#remove cabin
passengers <- passengers %>% select(-c(Cabin, PassengerId, Ticket, Name))
#Convert "Survived","Pclass","Sex","Embarked" to factors
for (i in c("Survived","Pclass","Sex","Embarked")){
  passengers[,i]=as.factor(passengers[,i])
}
## Create dummy variables for categorical variables
passengers <- dummy.data.frame(passengers, names=c("Pclass","Sex","Embarked"), sep="_")

#partition
inTrain<-sample(1:nrow(passengers),dim(passengers)[1]*0.70)
train.data <- passengers[inTrain,]
test.data <- passengers[-inTrain,]

#create model 
model <- glm(Survived ~.,family=binomial(link='logit'),data=train.data) 

# Predicting Test Data
test.data$prediction <- predict(model,newdata=test.data,type='response')
test.data$cutoff.7 <- ifelse(test.data$prediction > 0.7,1,0)

# Confusion matrix and statistics
library(caret)
pass_conf_mat = confusionMatrix(factor(test.data$cutoff.8), factor(test.data$Survived))

#ROC curves
roc1 <- with(test.data,roc(Survived,cutoff.7))
plot(roc1, col="blue", print.auc=TRUE, grid=TRUE, auc.polygon=TRUE, identity.col="gray",auc.polygon.col="white",legend.title="Cutoff value of ")


