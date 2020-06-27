#Data Manipulation WHR
library (dplyr)
library (ggplot2)




#Creating/Calling Database
db <- read.csv("C:/Users/THINKPAD/Documents/WHR2015.csv", header=TRUE)

View(db)
#Let's consider data does not have any error (remove standard error column)
db <- db[,-5]

#Create a new column continent for classification
db$continent <- NA


#Australia 
db$continent[which(db$Region %in% c("Australia and New Zealand"))] <- "Australia"
#North America
db$continent[which(db$Region %in% c("North America"))] <- "North America"
#Europe 
db$continent[which(db$Region %in% c("Western Europe", "Central and Eastern Europe"))] <- "Europe"
#Africa
db$continent[which(db$Region %in% c("Sub-Saharan Africa", "Middle East and Northern Africa"))] <- "Africa"
#Asia
db$continent[which(db$Region %in% c("Eastern Asia", "Southern Asia", "Southeastern Asia"))] <- "Asia"
#America
db$continent[which(db$Region %in% c("Latin America and Caribbean"))] <- "South America"

#Create an average of all the numerical data continent wise
hp<- aggregate(db[,4:11], list(db$continent), mean)
View(hp)

#Data Visualization of WHR (Part 1)
library(ggplot2)
library(corrgram)
library(corrplot)

#Basics
summary(db)
View(head(db,10))
View(tail(db,10))

#Graph the mean of data of all continents
ggplot(hp,aes(x=Group.1,y=Happiness.Score, fill=Group.1)) + geom_bar(stat="Identity") +ggtitle("Happiness Score of Each Continent")+ ylab("Happiness Score")+xlab ("Continents") 

#Correlation in variables
col <- sapply(db, is.numeric)
cor.data <- cor(db[,col])
corrplot(cor.data, method="square", type= "upper")

corrplot(cor.data, method="number", type= "upper")

#Boxplots region wise
box <- ggplot(db, aes(x=Region, y=Happiness.Score, color = Region))
box + geom_boxplot() + geom_jitter(aes(color=Country), size=1.0)+ ggtitle("Happiness score for Regions and Coutnries") + coord_flip() + theme(legend.position="none")

#Boxplots Continents wise
ggplot(db, aes(x=continent, y=Happiness.Score, color = continent)) + geom_boxplot() + ggtitle("Happiness Score for Continents")

#Regression for all continents
ggplot(db, aes(x=Health..Life.Expectancy. , y=Happiness.Score)) + geom_point(aes(color = continent), size = 3, alpha=0.8) + geom_smooth(aes(color=continent, fill=continent), method= "lm", fullrange=T) +facet_wrap(~continent) + theme_bw() +ggtitle("Scatter Plot for Life Expectancy")

#for economy
ggplot(db, aes(x=Economy..GDP.per.Capita. , y=Happiness.Score)) + geom_point(aes(color = continent), size = 3, alpha=0.8) + geom_smooth(aes(color=continent, fill=continent), method= "lm", fullrange=T) +facet_wrap(~continent) + theme_bw() +ggtitle("Scatter Plot for Economy")

#for freedom
ggplot(db, aes(x=Freedom , y=Happiness.Score)) + geom_point(aes(color = continent), size = 3, alpha=0.8) + geom_smooth(aes(color=continent, fill=continent), method= "lm", fullrange=T) +facet_wrap(~continent) + theme_bw() +ggtitle("Scatter Plot for Freedom")

#for family
ggplot(db, aes(x=Family , y=Happiness.Score)) + geom_point(aes(color = continent), size = 3, alpha=0.8) + geom_smooth(aes(color=continent, fill=continent), method= "lm", fullrange=T) +facet_wrap(~continent) + theme_bw() +ggtitle("Scatter Plot for Family")

#for Trust in Government
ggplot(db, aes(x=Trust..Government.Corruption. , y=Happiness.Score)) + geom_point(aes(color = continent), size = 3, alpha=0.8) + geom_smooth(aes(color=continent, fill=continent), method= "lm", fullrange=T) +facet_wrap(~continent) + theme_bw() +ggtitle("Scatter Plot for Trust in Government")

#Plots were for the whole continent
#Plot for the most unhappiest places, noticed in box plots
box + geom_boxplot() + geom_jitter(aes(color=Country), size=1.0)+ ggtitle("Happiness score for Regions and Coutnries") + coord_flip() + theme(legend.position="none")
#Conclude that Western Europe, North America and Australia and New Zealand are the happiest continent
#Conclude that Sub-Saharan is the unhappiest continent
#Conclude that other regions are neural

#PART 2
#Classify all of the data based on happiest neutral and less happy region

db$happinessmeter <- NA
db$happinessmeter[which(db$Region %in% c("Australia and New Zealand", "Western Europe","North America"))] <- "Happiest"
db$happinessmeter[which(db$Region %in% c("Sub-Saharan Africa"))] <- "Least Happiest"
db$happinessmeter[which(db$Region %in% c("Southern Asia","Southeastern Asia", "Middle East and Northern Africa", "Latin America and Caribbean", "Eastern Asia", "Central and Eastern Europe"))] <- "Neutral"

#Plot Regression for all three regions
ggplot(db, aes(x=Health..Life.Expectancy., y=Happiness.Score)) + geom_point(aes(color=happinessmeter),size=3, alpha=0.8) + geom_smooth(aes(color=happinessmeter, fill = happinessmeter), method="lm", fullrange=T) + facet_wrap(~happinessmeter) + theme_bw()

#Plot for Economy
ggplot(db, aes(x=Economy..GDP.per.Capita., y=Happiness.Score)) + geom_point(aes(color=happinessmeter),size=3, alpha=0.8) + geom_smooth(aes(color=happinessmeter, fill = happinessmeter), method="lm", fullrange=T) + facet_wrap(~happinessmeter) + theme_bw()

#Plot for Family
ggplot(db, aes(x=Family, y=Happiness.Score)) + geom_point(aes(color=happinessmeter),size=3, alpha=0.8) + geom_smooth(aes(color=happinessmeter, fill = happinessmeter), method="lm", fullrange=T) + facet_wrap(~happinessmeter) + theme_bw()

#Plot for Freedom
ggplot(db, aes(x=Freedom, y=Happiness.Score)) + geom_point(aes(color=happinessmeter),size=3, alpha=0.8) + geom_smooth(aes(color=happinessmeter, fill = happinessmeter), method="lm", fullrange=T) + facet_wrap(~happinessmeter) + theme_bw()

#Plot for Generosity
ggplot(db, aes(x=Generosity, y=Happiness.Score)) + geom_point(aes(color=happinessmeter),size=3, alpha=0.8) + geom_smooth(aes(color=happinessmeter, fill = happinessmeter), method="lm", fullrange=T) + facet_wrap(~happinessmeter) + theme_bw()

#Plot for Dystopia.Residual
ggplot(db, aes(x=Dystopia.Residual, y=Happiness.Score)) + geom_point(aes(color=happinessmeter),size=3, alpha=0.8) + geom_smooth(aes(color=happinessmeter, fill = happinessmeter), method="lm", fullrange=T) + facet_wrap(~happinessmeter) + theme_bw()

#Plot the GDP and Health Expectancy on World Map
#install rworldmap package

library(rworldmap)
d <- data.frame(country=db$Country, value=db$Economy..GDP.per.Capita.)
n <- joinCountryData2Map(d, joinCode = "NAME", nameJoinColumn = "country" )
mapCountryData(n, nameColumnToPlot = "value", mapTitle = "World Map for GDP 2015", colourPalette = "terrain" )

#Map for Health Expectancy 
d <- data.frame(country=db$Country, value=db$Health..Life.Expectancy.)
n <- joinCountryData2Map(d, joinCode = "NAME", nameJoinColumn = "country" )
mapCountryData(n, nameColumnToPlot = "value", mapTitle = "World Map for Health Expectancy 2015", colourPalette = "terrain" )


