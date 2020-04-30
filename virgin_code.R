#############################
#
#
#Logistic Regression with R
#
#
#############################

library(ggplot2) #<- Package for data Visualisation
library(cowplot) #<- Themes for ggplot
library(plyr) #<- set of tools. working with sequences etc


df <- read.csv("Alle_virgin.csv") #<-Read the dataset

# this shows that we need to tell R which columns contain factors
# it also shows us that there are some values to convert
str(df)

#set the nationality column to a factor
df$vietnam <- as.factor(df$vietnam)

#drop 2 "useless" columns
df <- df[ , -which(names(df) %in% "age_virgin")]
df <- df[ , -which(names(df) %in% "vietnam")]

#add an ID column. We need it to make a better plot later
df$ID <- seq.int(nrow(df))

#dublicate the dataset. We need 2 sets for the plot
df2 <- df
#transform the factor "virgin" from  1/2 to 1/0
df2$virgin <- revalue(df2$virgin, c("Yes"=0))# revalue is part of the plyr libary
df2$virgin <- revalue(df2$virgin, c("No"=1))
df2$virgin <- as.integer(df2$virgin)
#subtract 1 from each value. that we get a scale from 0-1
nrow(df)#->134
df2$v2<-df2[,2 ] - seq(1,1,length.out = 134)


#check if the factors are right in booth datasets. 
str(df)
str(df2)


#Now we can do some quality control by making sure all of the factor levels are represented
xtabs(~ virgin+ gender, data=df2)
xtabs(~ virgin+ major, data=df2)


###########
##
## Now do the actual logistic regression
## We use major, gender, age to predict virginity
##
###########



logistic_all <- glm(virgin ~ major+ gender+ age, data=df, family="binomial")
# the summary gives us imformatin about the weight of each Coefficient, the Deviance Residuals and the AIC
summary(logistic_all)

## Now calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic_all$null.deviance/-2
ll.proposed <- logistic_all$deviance/-2

ll.null
ll.proposed

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

## The p-value for the R^2
1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic_all$coefficients)-1))


# Lastly, let's  see what this logistic regression predicts for all of our datas
predicted.data <- data.frame(
  probability.of.virgin=logistic_all$fitted.values,
  virgin=df$virgin)


#we ordered the data that we will get a nice logistic regression curve in sigmoid shape
predicted.data <- predicted.data[
  order(predicted.data$probability.of.virgin, decreasing=TRUE),]
predicted.data$rank <- 1:nrow(predicted.data)

#take a look to our predicted data
str(predicted.data)

#use the ggplot lib for plot the data and cowplot for a nice design
#first we plot the logistic regression courve and give the point a different color for Yes/No
ggplot(data=predicted.data, aes(x=rank, y=probability.of.virgin)) +
  geom_point(aes(color=virgin), shape=16, stroke=1) +
  #add points for yes or no. That we can compare it better
  geom_point(data=df2, aes(x=ID , y=v2))+
  #label the axis
  xlab("Person Index") +
  ylab("Predicted probability of be a virgin")

#get a look about the predicted data
#plot(predicted.data)

#save diagram on pc
ggsave("Plot_all_noNationality.png", height = 7 , width = 7 * 2.5)