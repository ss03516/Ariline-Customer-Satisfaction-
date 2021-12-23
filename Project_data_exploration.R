library(ggplot2)
library(tidyverse)
library(maps)
library(ggpubr)
library(broom)
library(AICcmodavg)
library(car)
library(modeest)
setwd("/Users/92335/Desktop/BD Comp Project")

df = read.csv("train.csv") 
df2 = read.csv("test.csv")

nrow(df)
nrow(df2)

head(df)
str(df)

str(df)

#merging two dfs 

df1 = rbind(df, df2)

colnames(df1)
str(df1)
nrow(df1)
view(df1)

#Factoring catogorical_data

df1$Gender_f<-as.factor(df1$Gender)
contrasts(df1$Gender_f)

df1$Customer.Type_f<-as.factor(df1$Customer.Type)
contrasts(df1$Customer.Type_f)

df1$Type.of.Travel_f<-as.factor(df1$Type.of.Travel)
contrasts(df1$Type.of.Travel_f)

df1$Class_f<-as.factor(df1$Class)
contrasts(df1$Class_f)

df1$satisfaction_f<-as.factor(df1$satisfaction)
contrasts(df1$satisfaction_f)

str(df1)

# Data_exploration

hist(df1$Age, main = 'Frequency Distribution of Age',
     xlab ='Age', breaks = 10)

summary(df1$Age)

summary(df1) # Arrival Delay in minutes has 393 null values. 

sum(is.na(df1)) # Arrival Delay in minutes has 393 null values. 


p1 = ggplot(data = df1)

#ratio of males / females in our dataset

ggplot(data = df1) +
  geom_bar(mapping = aes(x = Gender))


#Online.booking scores vs Gender
p1 + geom_point(mapping = aes(x =  Gender_f, y = Ease.of.Online.booking), position = "jitter")

ggplot(data = df1) +
  geom_bar(mapping = aes(x = Ease.of.Online.booking, fill = Gender))


# any pattern b/w gender & satisfaction

ggplot(data = df1) +
  geom_bar(mapping = aes(x = satisfaction_f, fill = Gender))

# any pattern b/w consumer type & satisfaction
ggplot(data = df1) +
  geom_bar(mapping = aes(x = Customer.Type, fill = Gender))

p1 + geom_histogram(mapping = aes(x = Age, fill = Gender), binwidth = 5) 

p1 + geom_point(mapping = aes(x = Age, y = satisfaction_f, colour = Gender))


#There cant be any comparison b/w loyal and disloyal customer bc unbalanced sample. 
p1 +
  geom_bar(mapping = aes(x = satisfaction_f , fill = Customer.Type_f))


# Gender + Age + Type of travel 
p1 + geom_point(mapping = aes(x = Age, y = Flight.Distance, colour = Gender))

# Age and how easy they find the online systems. // poor method
p1 + geom_point(mapping = aes(x = Ease.of.Online.booking  , y = Age ))


p1 + geom_point(mapping = aes(x  Departure.Delay.in.Minutes , y = Arrival.Delay.in.Minutes))


