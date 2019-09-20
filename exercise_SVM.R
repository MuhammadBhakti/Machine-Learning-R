## Support vector machine
loans <- read.csv('C:/Users/User/Desktop/Kursus UDEMy/R Studio/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/loan_data.csv')
print(str(loans))

##convert to categorical
loans$credit.policy <- factor(loans$credit.policy)
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)
##
# explanatory data analysis
library(ggplot2)
ggplot(loans, aes(fico))+ geom_histogram(aes(fill=not.fully.paid), color='white', bins = 40, alpha=0.5)+theme_bw()+ggtitle("TEST SVM")+ scale_fill_manual(values = c('green','darkblue'))
ggplot(loans, aes(x=factor(purpose))) + geom_bar(aes(fill=not.fully.paid), position = 'dodge')+ggtitle('TEST Support Vector Machine') + theme_bw()
ggplot(loans, aes(int.rate, fico))+ geom_point(color='red')+theme_bw()+ggtitle('TEST Support Vector Machine')


##
#Build Support Vector Machine Model
library(caTools)
set.seed(101)

sample<-sample.split(loans$not.fully.paid,0.7)
train <- subset(loans,sample==T)
test <- subset(loans, sample==F)

##
#train SVM
library(e1071)
model <- svm(not.fully.paid~., data=train)
predicted.values<- predict(model, test[1:13])
table(predicted.values,test$not.fully.paid)
print(summary(model))

##
#Tuning Model
tuned.result <- tune(svm,train.x = not.fully.paid~., data = train,
                     kernel='radial', ranges = list(cost=c(100,200), gamma=c(0.1)))
print(summary(tuned.result))

tuned.model <- svm(not.fully.paid~., data=train, cost=100, gamma=0.1)
tuned.predictions <- predict(tuned.model, test[1:13])
table(tuned.predictions, test$not.fully.paid)
