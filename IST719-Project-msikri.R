# Information Visualization Poster IST -719 #
# Tennis Performance Statistics
# R code 
# Author : Manas Sikri
# Syracuse University
# Date :- 25-November-2016

# Required Packages
install.packages("gdata")
library(gdata)
install.packages("ggplot2")
library(ggplot2)
install.packages("lubridate")
library(lubridate)
install.packages("googleVis")
library(googleVis)

# Reading the source file 
# Please note Tennis.2013 contains data for year 2011,2012 and 2013 combined
Tennis.2013 <- read.xls("D:\\Syracuse Subjects\\IST719 - Information Visualization\\Project Poster\\ATP_Men_2013.xlsx")
Tennis.2014 <- read.xls("D:\\Syracuse Subjects\\IST719 - Information Visualization\\Project Poster\\ATP_Men_2014.xlsx")
Tennis.2015 <- read.xls("D:\\Syracuse Subjects\\IST719 - Information Visualization\\Project Poster\\ATP_Men_2015.xlsx")

# cleanup by removing non essentials columsn
Tennis.2013 <- Tennis.2013[,-37:-38]
Tennis.2014 <- Tennis.2014[,-37:-38]
#c combine the different datasets into one column
Tennis.Data <- rbind(Tennis.2013,Tennis.2014,Tennis.2015)
View(Tennis.Data) # view the final dataset
str(Tennis.Data) # 13143 obs. of  40 variables



## 1. Time Series ##
# changing the date format
Tennis.Data$Date <- as.Date(Tennis.Data$Date, format = "%Y-%m-%d")

# subset the data according to the top 5 players
Tennis.TOP5 <- Tennis.Data[Tennis.Data$Winner == 'Djokovic N.' | Tennis.Data$Winner == 'Murray A.'
                           | Tennis.Data$Winner == 'Federer R.' | Tennis.Data$Winner == 'Nadal R.'
                           | Tennis.Data$Winner == 'Berdych T.',]
nrow(Tennis.TOP5) # check the number of rows for new dataste

## Date Teansformation according to plot requirement

# spliting the date into year, month and day
Tennis.TOP5$Year <- format(Tennis.TOP5$Date, '%Y')
Tennis.TOP5$Month <- format(Tennis.TOP5$Date, '%m')
Tennis.TOP5$Day <- format(Tennis.TOP5$Date, '%d')

# wins of each player in months
Wins.by.month.year <- aggregate(Tennis.TOP5$Winner
                                , by =list(Tennis.TOP5$Winner, Tennis.TOP5$Month, Tennis.TOP5$Year)
                                , FUN = length)
View(Wins.by.month.year)

# change the columns name of dataframe resulted from aggregate function
colnames(Wins.by.month.year) <- c("Player", "Month", "Year", "Wins")
Wins.by.month.year$Date <- "01"
# format the date 
Wins.by.month.year$Value = paste(Wins.by.month.year$Year
                                 , Wins.by.month.year$Month, Wins.by.month.year$Date , sep="-")
# combine the values of dataframe to get the final formatted dataset
finaldata <- data.frame(Wins.by.month.year$Value
                        , Wins.by.month.year$Player
                        , Wins.by.month.year$Wins)
# change the column names
colnames(finaldata) <- c("Date", "Player", "Wins")
View(finaldata)

####################################
g <- ggplot(finaldata, aes(x = as.Date(Date), y = Wins, color = Player))  + 
  geom_smooth() + 
  #geom_line(aes(x = as.Date(Date), y = Wins, group = Player, color = Player)) +
  geom_line(alpha=.25) +
  scale_y_continuous('Wins') + geom_point(alpha=0.10) + xlab("Month") + 
  ylab("Wins") + 
  scale_colour_brewer(palette="Set1")
plot(g) # plot



## 2. Surface Performance Statistics
# subset the data for top 5 player
# one for winner
# one for loser
Winner.TOP5 <- Tennis.Data[Tennis.Data$Winner == 'Djokovic N.' | Tennis.Data$Winner == 'Murray A.'
                           | Tennis.Data$Winner == 'Federer R.' | Tennis.Data$Winner == 'Nadal R.'
                           | Tennis.Data$Winner == 'Berdych T.',]

Loser.Top5 <- Tennis.Data[Tennis.Data$Loser == 'Djokovic N.' | Tennis.Data$Loser == 'Murray A.'
                          | Tennis.Data$Loser == 'Federer R.' | Tennis.Data$Loser == 'Nadal R.'
                          | Tennis.Data$Loser == 'Berdych T.',]


# function to check for surface statistics
surface <- function(player, surfacetype) {
  # number of rows for specific player who won
  w1 <- nrow(Winner.TOP5[Winner.TOP5$Winner == player & Winner.TOP5$Surface == surfacetype,])
  
  # number of rows for specific player who lost
  l1 <- nrow(Loser.Top5[Loser.Top5$Loser == player & Loser.Top5$Surface == surfacetype,])
  
  # efficiency
  efficiency <- w1/(w1+l1)*100
  # return the value
  return (efficiency)
}

# function to call the statistics function
invoke <- function(player){
  n1 <- surface(player, 'Clay') # invoking the function 'surface' for clay
  n2 <- surface(player, 'Hard') # invoking the function 'surface' for hard
  n3 <- surface(player, 'Grass') # invoking the function 'surface' for Grass
  return (c(n1,n2,n3)) # return the value
}

# calling the function for each player
djokovic <- invoke('Djokovic N.')
Murray <- invoke('Murray A.')
Federer <- invoke('Federer R.')
Nadal <- invoke('Nadal R.')
Berdych <- invoke('Berdych T.')

# creating a dataframe
surface.statistics <- data.frame(djokovic, Murray, Federer, Nadal,Berdych)
rownames(surface.statistics) <- c('Clay', 'Hard', 'Grass')

# transpose the dataframe
surface.statistics <- t(surface.statistics)
surface.statistics <- data.frame(surface.statistics)

# changing it to matrix format
surface.statistics <- as.matrix(surface.statistics)

# stacked bar plot
barplot(surface.statistics, beside = T
        , ylim = c(0,150)
        , xlab = "Athletes", ylab = "Review"
        , col = c("aquamarine3", "coral2", "deepskyblue3", "khaki3", "mediumpurple1")
        , main = "Average Review of Players by Surface"
        , legend.text = rownames(surface.statistics))


## 3. Top Cities World Map
# create a location dataframe using aggregate function
location <- aggregate(x = Tennis.Data$Location
                      , by = list(unique.values = Tennis.Data$Location)
                      ,FUN = length)
View(location)  # check the location
# set the column names
colnames(location) <- c("Cities", "Number")

# sort with respect to number of matches
location <- location[order(location$Number, decreasing = TRUE),]
View(location)

# top 25 cities with most number of matches
top25.cities <- location[1:25,]
# adding the latitude and longitutde manually
top25.cities <- edit(top25.cities) 

# saving the dataframe for future use
write.csv(top25.cities, 'MyLatLongData.csv')

# reading the saved data frame file
my.data <- read.csv('MyLatLongData.csv')
str(my.data)
# plotting world map with cities having most number of matches throughout the year?
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="white") # create a layer of borders
mp <- ggplot() +   mapWorld
# setting the color palette
rainbowcols <- rainbow(25, s = 0.5)

mp <- mp + geom_point(aes(x=my.data$longitude
                          , y=my.data$latitude)
                      , color=rainbowcols 
                      , size=3)
mp # plot the world map



## 4. Donut Pie Chart - Performance
# Donut Pie Chart for players performance in particular city/Tournament

# function city to calculate the performance at a particular city
city <- function(city)
{
  x1 <- Tennis.Data[Tennis.Data$Location == city & Tennis.Data$Winner == 'Djokovic N.',]
  x2 <- Tennis.Data[Tennis.Data$Location == city & Tennis.Data$Winner == 'Murray A.',]
  x3 <- Tennis.Data[Tennis.Data$Location == city & Tennis.Data$Winner == 'Federer R.',]
  x4 <- Tennis.Data[Tennis.Data$Location == city & Tennis.Data$Winner == 'Nadal R.',]
  x5 <- Tennis.Data[Tennis.Data$Location == city & Tennis.Data$Winner == 'Berdych T.',]
  df <- c(nrow(x1),nrow(x2),nrow(x3),nrow(x4),nrow(x5))
  return (df)
}

# function to create a dataframe
create.df <- function(df.temp)
{
  playerInfo <- c("Djokovic N.","Murray A.","Federer R.","Nadal R.","Berdych T.")
  df.final <- data.frame(playerInfo,df.temp)
  colnames(df.final) <- c("Player", "Number of Wins")
  return(df.final)
}


# calling the function
df.australia <- create.df(city('Melbourne'))
df.newyork <- create.df(city('New York'))
df.london <- create.df(city('London'))
df.miami <- create.df(city('Miami'))
df.paris <- create.df(city('Paris'))
df.dubai <- create.df(city('Dubai '))
df.beijing <- create.df(city('Beijing'))

# pie chart
# function to create the pie chart for different cities
# australia
australia.doughnut <- gvisPieChart(df.australia
                                   , options = list(width = 600
                                                    , height = 600
                                                    , title = "Australian Open"
                                                    , pieSlicetext = 'label1'
                                                    , pieHole = 0.55
                                                    , slices = "{0: {offset: 0.2}}"
                                                    , chartid="doughnut"))
plot(australia.doughnut)

# new york
ny.doughnut <- gvisPieChart(df.newyork
                            , options = list(width = 600
                                             , height = 600
                                             , title = "US Open"
                                             , pieSlicetext = 'label1'
                                             , pieHole = 0.55
                                             , slices = "{0: {offset: 0.2}}"
                                             , chartid="doughnut"))
plot(ny.doughnut)

# london
london.doughnut <- gvisPieChart(df.london
                                , options = list(width = 600
                                                 , height = 600
                                                 , title = "Wimbeldon"
                                                 , pieSlicetext = 'label1'
                                                 , pieHole = 0.55
                                                 , slices = "{0: {offset: 0.2}}"
                                                 , chartid="doughnut"))
plot(london.doughnut)

# france
paris.doughnut <- gvisPieChart(df.paris
                               , options = list(width = 600
                                                , height = 600
                                                , title = "French Open"
                                                , pieSlicetext = 'label1'
                                                , pieHole = 0.55
                                                , slices = "{0: {offset: 0.2}}"
                                                , chartid="doughnut"))
plot(paris.doughnut)

## End of Script ##