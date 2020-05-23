zoo = read.csv(choose.files())
View(zoo)

#Create Normalize Function

normalize <- function(x) {
  return((x-min(x))/max(x)-min(x))
}
#Normalize The Zoo Data
zoo_n <- as.data.frame(lapply(zoo[2:17],normalize))
zoo_1 <- cbind(zoo_n,zoo$type)

#Create Training Data
zoo_train <- zoo_n[1:81,]
zoo_test <- zoo_n[82:101,]

#Create Labels
zoo_train_labels <-zoo[1:81,18]
zoo_test_labels <-zoo[82:101,18]

#Train a model on data #Build a KNN model on Training Set
install.packages("class")
library(class)

test_acc <- NULL
train_acc <- NULL
for (i in seq(2,78,5))
{
  zoo_train_pred <- knn(train = zoo_train, test = zoo_test,cl=zoo_train_labels,k=i)
  train_acc <- c(train_acc,mean(zoo_train_pred==zoo_train_labels))
  zoo_test_pred <-knn(train = zoo_train,test = zoo_test,cl=zoo_train_labels,k=i)
  test_acc <- c(test_acc,mean(zoo_test_pred==zoo_test_labels))
}


#Testing Accuracy
# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(2,78,5),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(2,78,5),test_acc,type="l",main="Test_accuracy",col="red")


acc_df <- data.frame(train_acc=train_acc,test_acc=test_acc,neigh=seq(2,75,5))

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

zoo_test_pred <-knn(train = zoo_train,test = zoo_test,cl=zoo_train_labels,k=12)

CrossTable(x=zoo_test_labels,y=zoo_test_pred,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)