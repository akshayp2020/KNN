Load Glass CSV

glass <- read.csv("C:/Users/hp/Downloads/KNN/glass.csv")
glass$Type <-factor(glass$Type,levels = c("1","2","3","4","5","6","7"),labels = c("Building_windows","Building_windows_non_float","Vehicle_windows","vehicle_windows_non_float","containers","tableware","headlamps"))

#Create Normalize Function
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

#normalize the glass data
glass_n <- as.data.frame(lapply(glass[1:9],normalize))
glass_n1 <- cbind(glass_n,glass$Type)


#Create Training Data
glass_train <- glass_n[1:198,]
glass_test <- glass_n[199:214,]

#Create Labels
glass_train_labels <-glass[1:198,10]
glass_test_labels <-glass[199:214,10]

#Train a model on data #Build a KNN model on Training Set
install.packages("class")
library(class)

test_acc <- NULL
train_acc <- NULL
for (i in seq(3,200,2))
{
  glass_train_pred <- knn(train = glass_train, test = glass_test,cl=glass_train_labels,k=i)
  train_acc <- c(train_acc,mean(glass_train_pred==glass_train_labels))
  glass_test_pred <-knn(train = glass_train,test = glass_test,cl=glass_train_labels,k=i)
  test_acc <- c(test_acc,mean(glass_test_pred==glass_test_labels))
}

#Testing Accuracy
# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,200,2),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,200,2),test_acc,type="l",main="Test_accuracy",col="red")

acc_df <- data.frame(train_acc=train_acc,test_acc=test_acc,neigh=seq(3,200,2))

# Plotting 2 different graphs on same co-ordinate axis
install.packages("ggplot2")
library(ggplot2)
ggplot(acc_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))



###----Evaluating model performance

install.packages("gmodels")
library(gmodels)

glass_test_pred <-knn(train = glass_train,test = glass_test,cl=glass_train_labels,k=21)

CrossTable(x=glass_test_labels,y=glass_test_pred,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)
