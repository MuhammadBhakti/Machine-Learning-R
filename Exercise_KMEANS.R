df1<-read.csv('C:/Users/User/Desktop/Kursus UDEMy/R Studio/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/winequality-red.csv', sep = ';')
df2<-read.csv('C:/Users/User/Desktop/Kursus UDEMy/R Studio/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/winequality-white.csv', sep = ';')

##dataframe manipulation using sapply

df1$label<-sapply(df1$pH, function(x){'red'})
df2$label<-sapply(df2$pH, function(x){'white'})
#combining the data
wine <- rbind(df1,df2)

#Plotting
library(ggplot2)
ggplot(wine,aes(citric.acid)) + 
  geom_histogram(aes(fill=label), color='black', bins = 50) + 
  scale_fill_manual(values=c('red', 'white')) + ggtitle('HISTOGRAM') + theme_gray()

#KMEANS CLUSTER
cluster.data <- wine[,12]
clusters.wi <- kmeans(cluster.data,2)
print(clusters.wi$centers)
