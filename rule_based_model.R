library(plyr) #table count
library('ggplot2') # visualization
library('ggthemes') # visualization
library(mice)
#Reemplazar con ruta de directorio de trabajo
setwd("/home/belen/Downloads")

passengers = read.csv("passengers_test.csv")

#Ver tamaño de familia
passengers$Family<-passengers$SibSp+passengers$Parch

ggplot(passengers[1:891,], aes(x = Family, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()


#discretizar tamaño familia
passengers$Fam_size[passengers$Family==0]<- "single"
passengers$Fam_size[passengers$Family>0 & passengers$Family<4]<- "medium"
passengers$Fam_size[passengers$Family>4]<- "large"


###discretize tickets fare
passengers$Fare2 <-'31+'
passengers$Fare2[passengers$Fare < 31 & passengers$Fare>=15]<-'15-31'
passengers$Fare2[passengers$Fare < 15 & passengers$Fare>=8]<-'15-8'
passengers$Fare2[passengers$Fare < 8 ]<-'8'


passengers$Output4<-0
passengers$Output4[passengers$Sex=='female' & passengers$Fam_size!='large']<-1
#accuracy
acc_data<-count(passengers, c("Survived", "Output4"))
assert<-acc_data[4,3]+acc_data[1,3]
acc<-assert/891
acc #0.804

library(caret)
conf_mat = confusionMatrix(factor(passengers$Output4),factor(passengers$Survived))