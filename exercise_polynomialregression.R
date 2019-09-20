##Read the data
df<-read.csv('D:/Bhakti/Interview data science/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/bikeshare.csv', sep = ',')

## Show the data
library(ggplot2)
ggplot(df,aes(temp,count)) + geom_point(alpha=0.2, aes(color=temp)) + theme_bw()
##Plot count versus datetime as a scatterplot with a color gradient based on temperature. You'll need to convert the datetime column into POSIXct before plotting.
df$datetime <- as.POSIXct(df$datetime)
ggplot(df,aes(datetime,count)) + geom_point(aes(color=temp),alpha=0.5)  + scale_color_continuous(low='#55D8CE',high='#FF6E2E') +theme_bw()

##What is the correlation between temp and count?
cor(df[,c('temp','count')])
ggplot(df, aes(factor(season), count)) + geom_boxplot(aes(color= factor(season))) + theme_bw()

time.stamp <- df$datetime[4]
format(time.stamp,'%H')

df$hour <- sapply(df$datetime,function(x){format(x,"%H")})

library(dplyr)
## ggplot at working day (workingday = 1)
ggplot(filter(df, workingday==1), aes(hour,count))+
  geom_point(position = position_jitter(w=1, h=0), aes(color=temp), alpha=0.5)+
  scale_color_gradientn(colours =c('dark blue', 'blue', 'light blue', 'light green', 'yellow', 'orange', 'red'))+
  theme_bw() + ggtitle('Hour Working Day')

##ggplot non working day (workingday = 0)
ggplot(filter(df, workingday==0), aes(hour,count))+
  geom_point(position = position_jitter(w=1, h=0), aes(color=temp), alpha=0.5)+
  scale_color_gradientn(colours =c('dark blue', 'blue', 'light blue', 'light green', 'yellow', 'orange', 'red'))+
  theme_bw() + ggtitle('Hour Non Working Day')

## Building the model
model <- lm (count~ temp, df)
summary(model)

## You should have gotten 6.0462 as the intercept and 9.17 as the temp coeffecient. How can you interpret these values? Do some wikipedia research, re-read ISLR, or revisit the Linear Regression lecture notebook for more on this.
# Method 1
6.0462 + 9.17*25

# Method 2
temp.test <- data.frame(temp=c(25))
predict(model,temp.test)
#Use sapply() and as.numeric to change the hour column to a column of numeric values.
df$hour <- sapply(df$hour,as.numeric) # change into numeric values
model2 <- lm(count ~ . -casual - registered -datetime -atemp,df )
summary(model2)




