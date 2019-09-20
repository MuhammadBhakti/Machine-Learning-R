## Capstone project R for data science
##Use R to open the df.csv file and assign it to a dataframe called df using read.csv
df<- read.csv('D:/Bhakti/Interview data science/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Capstone and Data Viz Projects/Capstone Project/Batting.csv', sep = ',')

##Data manipulation and pre processing
df$BA <- df$H/df$AB # create new coloum called BA

# On Base Percentage
df$OBP <- (df$H + df$BB + df$HBP)/(df$AB + df$BB + df$HBP + df$SF)
# Creating X1B (Singles)
df$X1B <- df$H - df$X2B - df$X3B - df$HR
# Creating Slugging Average (SLG)
df$SLG <- ((1 * df$X1B) + (2 * df$X2B) + (3 * df$X3B) + (4 * df$HR) ) / df$AB

##Merging Salary Data with Batting Data
sal<- read.csv('D:/Bhakti/Interview data science/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Capstone and Data Viz Projects/Capstone Project/Salaries.csv', sep = ',')


##Use summary to get a summary of the batting data frame and notice the minimum year in the yearID column. Our batting data goes back to 1871! Our salary data starts at 1985, meaning we need to remove the batting data that occured before 1985.
summary(df)
##Use subset() to reassign batting to only contain data from 1985 and onwards
df <- subset(df, yearID>= 1985)
summary(df)

##Use the merge() function to merge the batting and sal data frames by c('playerID','yearID'). Call the new data frame combo
combo <- merge(df,sal, by=c('playerID','yearID'))

##Analyzing the Lost Players
##Use the subset() function to get a data frame called lost_players from the combo data frame consisting of those 3 players. Hint: Try to figure out how to use %in% to avoid a bunch of or statements!
lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01') )
lost_players

##Use subset again to only grab the rows where the yearID was 2001
lost_players<-subset(lost_players, yearID==2001)


##Reduce the lost_players data frame to the following columns: playerID,H,X2B,X3B,HR,OBP,SLG,BA,AB
lost_players <- subset(lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')])
lost_players

##Replacement Players
library(dplyr)
avail.players <- filter(combo, yearID==2001)
library(ggplot2)
ggplot(avail.players, aes(x=OBP, y= salary)) + geom_point(colour='blue')

##Looks like there is no point in paying above 8 million or so (I'm just eyeballing this number). I'll choose that as a cutt off point. There are also a lot of players with OBP==0. Let's get rid of them too.
avail.players <- filter(avail.players, salary <8000000,OBP>0)
avail.players <- filter(avail.players,AB >= 500)
possible <- head(arrange(avail.players, desc(OBP)),10)
possible <- possible[,c('playerID','OBP','AB', 'salary')]
possible
