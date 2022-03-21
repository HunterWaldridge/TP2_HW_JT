library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

wd <- read.csv("weatherAUS.csv")
head(wd)

# Data types:
str(wd)

# Missing Data:
(sum(is.na(wd))/prod(dim(wd)))*100

# Summary statistics:
summary(wd)

# Correlation matrix:
corwd <- cor(wd[sapply(wd,is.numeric)], use="complete.obs")

# Heat map of correlation matrix:
heatmap(corwd)

# Changing date to datetime format:
wd$Date<-as.Date(wd$Date, format="%m/%d/%y")
str(wd)

# Adding columns for temps in Farenheit:
wd$MinTemp_F<-(wd$MinTemp*1.8)+32
wd$MaxTemp_F<-(wd$MaxTemp*1.8)+32
wd$Temp9am_F<-(wd$Temp9am*1.8)+32
wd$Temp3pm_F<-(wd$Temp3pm*1.8)+32

head(wd)

# Rearranging temp columns:
wd2<-wd[,c(1,2,3,24,4,25,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,26,21,27,22,23)]
colnames(wd)

head(wd2)

# Renaming original temp columns to include C:
names(wd2)[3]<-"MinTemp_C"
names(wd2)[5]<-"MaxTemp_C"
names(wd2)[22]<-"Temp9am_C"
names(wd2)[24]<-"Temp3pm_C"
head(wd2)

# Summary statistics with new columns:
summary(wd2)

# Correlation matrix with new columns:
corwd2 <- cor(wd2[sapply(wd2,is.numeric)], use="complete.obs")
corwd2

# Heat map with new columns:
heatmap(corwd2)

# Yearly averages:
yearly_avg<-read.csv("yearly_avg.csv")
head(yearly_avg)

#Plotting average max temp over 2007-2017 in Australia:
ggplot(data=yearly_avg, aes(x=Date, y=MaxTemp_F)) +
  geom_line()+
  geom_point()

# Dataframe with only Sydney's info:
Sydney<-wd2[which (wd2$Location=='Sydney'),]
head(Sydney)

# Displot of Sydney's evaporation values:
ggplot(data=Sydney, aes(x=Evaporation, color = 'density')) +
  geom_histogram(aes(y= ..density..),bins=34)

# Displot of evaporation for all cities:
ggplot(data=wd2, aes(x=Evaporation, color = 'density')) +
  geom_histogram(aes(y= ..density..),bins=100)

# Dataframe of some big cities in Australia:
bigcities<-wd2[which (wd2$Location=='Adelaide' | wd2$Location=="Melbourne"),]
head(bigcities)

# Pairplot
Adelaide<-wd2[which (wd2$Location=='Adelaide'),]
Melbourne<-wd2[which (wd2$Location=='Melbourne'),]

pairs(Adelaide[,7:9], Melbourne[,7:9],pch = 19,  cex = 0.5,
      col=c("Orange","Red"),
      lower.panel=NULL, na.rm=TRUE)



# Bar plot of average annual rainfall:
ggplot(data=yearly_avg, aes(x=Date, y=Rainfall)) +
  geom_bar(stat="identity", width=0.5)

# Area plot of average annual rainfall:
ggplot(yearly_avg, aes(x=Date, y=Rainfall)) +
  geom_area(, na.rm=TRUE)

#Comparing Minimum and Maximum Temperatures in Sydney
boxplot (Sydney$MinTemp_F, Sydney$MaxTemp_F, Main = "Compare Minimum and Maximum Temperatures in Sydney", at = c(1,2) , names = c( "MinTemp_F", "MaxTemp_F"), las = 2 , col = c("red", "orange") , border = "brown" , horizontal = FALSE , notch = TRUE )

#Compare Temperatures in Sydney at 9am and 3pm
boxplot (Sydney$Temp9am_F, Sydney$Temp3pm_F, Main = "Compare Temperatures in Sydney at 9am and 3pm", at = c(1,2) , names = c( "Temp9am_F", "Temp3pm_F"), las = 2 , col = c("red", "orange") , border = "brown" , horizontal = FALSE , notch = TRUE )

#Compare Wind Speeds in Sydney at 9am and 3pm
boxplot (Sydney$WindSpeed9am, Sydney$WindSpeed3pm, Main = "Compare Wind Speeds in Sydney at 9am and 3pm", at = c(1,2) , names = c( "WindSpeed9am", "WindSpeed3pm"), las = 2 , col = c("red", "orange") , border = "brown" , horizontal = FALSE , notch = TRUE )

#Comparing Humidity in Sydney at 9am and 3pm
boxplot (Sydney$Humidity9am, Sydney$Humidity3pm, Main = "Comparing Humidity in Sydney at 9am and 3pm", at = c(1,2) , names = c( "Humidity9am", "Humidity3pm"), las = 2 , col = c("red", "orange") , border = "brown" , horizontal = FALSE , notch = TRUE )


