#---------------------------------
#Data Visualization
#---------------------------------
#Loading data set packages
#---------------------------------
library(datasets)
library(ggplot2)     
library(data.table)
library(reshape2)
library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)
library(aod)

#---------------------------------
#Retrieving data
#---------------------------------
#Airbnb Listings in New York
#---------------------------------

listings <- read.csv("Listings.csv")
View(listings)
listings$host_response_time_score <- revalue(listings$host_response_time,c("a few days or more"="1",
                                                                           "within a day"="2",
                                                                           "within a few hours"="3",
                                                                           "within an hour"="4"))

Listings_Data<- listings[-c(5,6,7,8,9,10,11,12,16,17,18,19)]
#Removing the unwanted columns

View(Listings_Data)
Listings_Data[is.na(Listings_Data)] <- 0
Listings_Data$ReviewAverageScore <- ave(Listings_Data$review_scores_rating, 
                                        Listings_Data$review_scores_cleanliness,
                                        Listings_Data$review_scores_communication,
                                        Listings_Data$review_scores_location, 
                                        Listings_Data$host_response_time_score) 
#Calculating the Review Average score by taking the average 

View(Listings_Data)

NewYork <- filter(Listings_Data, city == "New York") 
#Filtering only the New York Data
View(NewYork)

#Calculating the number of days since the host is listed using Host_since column

time1 <- Sys.Date() #Gives today's date
time1 <- format(as.Date(time1, "%Y %m %d"), "%d-%m-%Y") #Formatting the date
NewYork$startdate <- time1
time3 <- NewYork$startdate
time2 <- NewYork$host_since
NewYork$date_diff <- as.Date(as.character(NewYork$startdate), format="%d-%m-%Y")- as.Date(as.character(NewYork$host_since), format="%d-%m-%Y")

#Data processing

sum(is.na(NewYork))
#Replacing NA values by 0
NewYork$ReviewAverageScore[is.na(NewYork$ReviewAverageScore)]<-0
NewYork$date_diff[is.na(NewYork$date_diff)]<-0
NewYork$days[is.na(NewYork$days)]<-0
NewYork$logprice=log(NewYork$price) #Taking the log for the price
View(NewYork)

#Plotting scatter plot to analyze the data 

theme_set(theme_pubr())
#Problem Statement: In New York, does being an old AirBnB lister make you get better reviews?
#Plotting date_diff vs ReviewAverageScore
ggplot(data=NewYork,aes(x = date_diff, y = ReviewAverageScore)) + geom_point() + stat_smooth()
#The Plot does show that longer the number of days since your listing the better will be the Review Score. 
#However, we can observe a significant amount of high score reviews can be seen within 1000 days.
#Thus, we can conclude that there is an advantage of being an old lister but that doesn't create any entry barrier for a new comer to get good review score

#Problem Statement: In New York, does price play a vital role in getting good reviews? 
#Plotting a graph for logprice vs ReviewAverageScore
ggplot(data=NewYork,aes(x = logprice, y = ReviewAverageScore)) + geom_point() + stat_smooth()
#The plot clearly shows that mid range of logprice has higher volume of reviews (3 to 6) 
#Thus, we can conclude that price does affect in the listing popularity but that alone doesn't lead to better reviews

#---------------------------------
#Airbnb Listings in Paris 
#---------------------------------

listings <- read.csv("Listings.csv")
View(listings)
listings$host_response_time_score <- revalue(listings$host_response_time,c("a few days or more"="1",
                                                                           "within a day"="2",
                                                                           "within a few hours"="3",
                                                                           "within an hour"="4"))

Listings_Data<- listings[-c(5,6,7,8,9,10,11,12,16,17,18,19)]
#Removing the unwanted columns

View(Listings_Data)
Listings_Data[is.na(Listings_Data)] <- 0
Listings_Data$ReviewAverageScore <- ave(Listings_Data$review_scores_rating, 
                                        Listings_Data$review_scores_cleanliness,
                                        Listings_Data$review_scores_communication,
                                        Listings_Data$review_scores_location, 
                                        Listings_Data$host_response_time_score) 
#Calculating the Review Average score by taking the average 

View(Listings_Data)

Paris <- filter(Listings_Data, city == "Paris") 
#Filtering only the Paris Data
View(Paris)

#Calculating the number of days since the host is listed using Host_since column

time1 <- Sys.Date() #Gives today's date
time1 <- format(as.Date(time1, "%Y %m %d"), "%d-%m-%Y") #Formatting the date
Paris$startdate <- time1
time3 <- Paris$startdate
time2 <- Paris$host_since
Paris$date_diff <- as.Date(as.character(Paris$startdate), format="%d-%m-%Y")- as.Date(as.character(Paris$host_since), format="%d-%m-%Y")
#Data processing

sum(is.na(Paris))
#Replacing NA values by 0
Paris$ReviewAverageScore[is.na(Paris$ReviewAverageScore)]<-0
Paris$date_diff[is.na(Paris$date_diff)]<-0
Paris$days[is.na(Paris$days)]<-0
Paris$logprice=log(Paris$price) #Taking the log for the price
View(Paris)

#Plotting scatter plot to analyze the data 

theme_set(theme_pubr())
#Problem Statement: In Paris, does being an old AirBnB Lister make you get better reviews?
#Plotting date_diff vs ReviewAverageScore
ggplot(data=Paris,aes(x = date_diff, y = ReviewAverageScore)) + geom_point() + stat_smooth()
#The Plot does show that longer the number of days since your listing the better will be the Review Score. 
#However, we can observe a significant amount of high score reviews can be seen within 1000 days.
#Thus, we can conclude that there is an advantage of being an old lister but that doesn't create any entry barrier for a new comer to get good review score

#Problem Statement: In Paris, does price play a vital role in getting good reviews? 
#Plotting a graph for logprice vs ReviewAverageScore
ggplot(data=Paris,aes(x = logprice, y = ReviewAverageScore)) + geom_point() + stat_smooth()
#The plot clearly shows that mid range of logprice has higher volume of reviews (3 to 6) 
#Thus, we can conclude that price does affect in the listing popularity but that alone doesn't lead to better reviews

#---------------------------------
#Airbnb Listings in Rome 
#---------------------------------

listings <- read.csv("Listings.csv")
View(listings)
listings$host_response_time_score <- revalue(listings$host_response_time,c("a few days or more"="1",
                                                                           "within a day"="2",
                                                                           "within a few hours"="3",
                                                                           "within an hour"="4"))

Listings_Data<- listings[-c(5,6,7,8,9,10,11,12,16,17,18,19)]
#Removing the unwanted columns

View(Listings_Data)
Listings_Data[is.na(Listings_Data)] <- 0
Listings_Data$ReviewAverageScore <- ave(Listings_Data$review_scores_rating, 
                                        Listings_Data$review_scores_cleanliness,
                                        Listings_Data$review_scores_communication,
                                        Listings_Data$review_scores_location, 
                                        Listings_Data$host_response_time_score) 
#Calculating the Review Average score by taking the average 

View(Listings_Data)

Rome <- filter(Listings_Data, city == "Rome") 
#Filtering only the Rome Data
View(Rome)

#Calculating the number of days since the host is listed using Host_since column

time1 <- Sys.Date() #Gives today's date
time1 <- format(as.Date(time1, "%Y %m %d"), "%d-%m-%Y") #Formatting the date
Rome$startdate <- time1
time3 <- Rome$startdate
time2 <- Rome$host_since
Rome$date_diff <- as.Date(as.character(Rome$startdate), format="%d-%m-%Y")- as.Date(as.character(Rome$host_since), format="%d-%m-%Y")


View(Rome)

#Data processing

sum(is.na(Rome))
#Replacing NA values by 0
Rome$ReviewAverageScore[is.na(Rome$ReviewAverageScore)]<-0
Rome$date_diff[is.na(Rome$date_diff)]<-0
Rome$days[is.na(Rome$days)]<-0
Rome$logprice=log(Rome$price) #Taking the log for the price
View(Rome)

#Plotting scatter plot to analyze the data 

theme_set(theme_pubr())
#Problem Statement: In Rome, does being an old AirBnB Lister make you get better reviews?
#Plotting date_diff vs ReviewAverageScore
ggplot(data=Rome,aes(x = date_diff, y = ReviewAverageScore)) + geom_point() + stat_smooth()
#The Plot does show that longer the number of days since your listing the better will be the Review Score. 
#However, we can observe a significant amount of high score reviews can be seen within 1000 days.
#Thus, we can conclude that there is an advantage of being an old lister but that doesn't create any entry barrier for a new comer to get good review score

#Problem Statement: In Rome, does price play a vital role in getting good reviews? 
#Plotting a graph for logprice vs ReviewAverageScore
ggplot(data=Rome,aes(x = logprice, y = ReviewAverageScore)) + geom_point() + stat_smooth()
#The plot clearly shows that mid range of logprice has higher volume of reviews (3 to 6) 
#Thus, we can conclude that price does affect in the listing popularity but that alone doesn't lead to better reviews



#---------------------------------
#Airbnb Listings in Mexico City 
#---------------------------------

listings <- read.csv("Listings.csv")
View(listings)
listings$host_response_time_score <- revalue(listings$host_response_time,c("a few days or more"="1",
                                                                           "within a day"="2",
                                                                           "within a few hours"="3",
                                                                           "within an hour"="4"))

Listings_Data<- listings[-c(5,6,7,8,9,10,11,12,16,17,18,19)]
#Removing the unwanted columns

View(Listings_Data)
Listings_Data[is.na(Listings_Data)] <- 0
Listings_Data$ReviewAverageScore <- ave(Listings_Data$review_scores_rating, 
                                        Listings_Data$review_scores_cleanliness,
                                        Listings_Data$review_scores_communication,
                                        Listings_Data$review_scores_location, 
                                        Listings_Data$host_response_time_score) 
#Calculating the Review Average score by taking the average 

View(Listings_Data)

MexicoCity <- filter(Listings_Data, city == "Mexico City") 
#Filtering only the Mexico City Data
View(MexicoCity)

#Calculating the number of days since the host is listed using Host_since column

time1 <- Sys.Date() #Gives today's date
time1 <- format(as.Date(time1, "%Y %m %d"), "%d-%m-%Y") #Formatting the date
MexicoCity$startdate <- time1
time3 <- MexicoCity$startdate
time2 <- MexicoCity$host_since
MexicoCity$date_diff <- as.Date(as.character(MexicoCity$startdate), format="%d-%m-%Y")- as.Date(as.character(MexicoCity$host_since), format="%d-%m-%Y")


#Data processing

sum(is.na(MexicoCity))
#Replacing NA values by 0
MexicoCity$ReviewAverageScore[is.na(MexicoCity$ReviewAverageScore)]<-0
MexicoCity$date_diff[is.na(MexicoCity$date_diff)]<-0
MexicoCity$days[is.na(MexicoCity$days)]<-0
MexicoCity$logprice=log(MexicoCity$price) #Taking the log for the price
View(MexicoCity)

#Plotting scatter plot to analyze the data 

theme_set(theme_pubr())
#Problem Statement: In Mexico City, does being an old AirBnB lister make you get better reviews?
#Plotting date_diff vs ReviewAverageScore
ggplot(data=MexicoCity,aes(x = date_diff, y = ReviewAverageScore)) + geom_point() + stat_smooth()
#The Plot does show that longer the number of days since your listing the better will be the Review Score. 
#However, we can observe a significant amount of high score reviews can be seen within 1000 days.
#Thus, we can conclude that there is an advantage of being an old lister but that doesn't create any entry barrier for a new comer to get good review score

#Problem Statement: In Mexico City, does price play a vital role in getting good reviews? 
#Plotting a graph for logprice vs ReviewAverageScore
ggplot(data=MexicoCity,aes(x = logprice, y = ReviewAverageScore)) + geom_point() + stat_smooth()
#The plot clearly shows that mid range of logprice has higher volume of reviews (3 to 6) 
#Thus, we can conclude that price does affect in the listing popularity but that alone doesn't lead to better reviews

#---------------------------------
#Decision Tree
#---------------------------------

#---------------------------------
#Model 1
#---------------------------------
#Comparing Whether a Host will be a Super host based on total listings and review scores
library(rpart)
library(rpart.plot)
library(caret)

airbnb.df <- read.csv("Listings.csv")
airbnb.df$listing_count <- with(airbnb.df, ave(listing_id,host_id, FUN=length))
airbnb.df <- airbnb.df[ , -c(1:4,5:8,10:19,22,24:25,27:33)]
View(airbnb.df)

# partition
set.seed(1) 
train.index <- sample(c(1:dim(airbnb.df)[1]), dim(airbnb.df)[1]*0.7)  
train.df <- airbnb.df[train.index, ]
valid.df <- airbnb.df[-train.index, ]

# classification tree
default.ct <- rpart(host_is_superhost ~ ., data = train.df ,method = "class")
# plot tree
prp(default.ct, type = 1, extra = "auto", under = TRUE, split.font = 1, varlen = -10)
# count number of leaves
length(default.ct$frame$var[default.ct$frame$var == "<leaf>"])

#based on the decision tree, we can conclude that 
#if the review score is less than 97 
#and the owner has less than 5 listings, 
#we can consider the owner to be a super host.

#based on the decision tree, we can also conclude that 
#if the review score is greater than 99 
#and the owner has more than 5 listings, 
#we can consider the owner to be a super host.

#---------------------------------
#Model 2
#---------------------------------
#Comparing Instantly Bookable with Respect to listing count and bedrooms available
airbnb2.df <- read.csv("Listings.csv")
airbnb2.df$listing_count <- with(airbnb2.df, ave(listing_id,host_id, FUN=length))
airbnb2.df <- airbnb2.df[ , -c(1:4,5:9,10,12:19,22,24:25,27:32)]

View(airbnb2.df)

# partition
set.seed(1) 
train.index <- sample(c(1:dim(airbnb2.df)[1]), dim(airbnb2.df)[1]*0.6)  
train.df <- airbnb2.df[train.index, ]
valid.df <- airbnb2.df[-train.index, ]

# classification tree
default.ct <- rpart(instant_bookable ~ ., data = train.df ,method = "class")
# plot tree
prp(default.ct, type = 1, extra = 2, under = TRUE, split.font = 1, varlen = -10)
# count number of leaves
length(default.ct$frame$var[default.ct$frame$var == "<leaf>"])

#based on the decision tree, we can conclude that 
#if the owner has more than 3 listings, 
#and the listing has less than 3
#we can consider the owner to be a super host.

#---------------------------------
#Logistic Regression
#---------------------------------

#---------------------------------
#Model 1
#---------------------------------
# Checking if being super host has a direct correlation to Host response time

library(caret)
library(pROC)

logistic.airbnb.df <- read.csv("Listings.csv")
logistic.airbnb.df <- logistic.airbnb.df[ , -c(1:4,5,7:8,10:19,22,24:25,27:33)]
# Drop Unwanted Columns in the Data.
View(logistic.airbnb.df)
# treating Host Response time as categorical
logistic.airbnb.df$host_response_time <- as.factor(logistic.airbnb.df$host_response_time)
sapply(logistic.airbnb.df, class)



#Partitioning the data
set.seed(2)
train.index <- sample(c(1:dim(logistic.airbnb.df)[1]), dim(logistic.airbnb.df)[1]*0.6)  
train.df <- logistic.airbnb.df[train.index, ]
valid.df <- logistic.airbnb.df[-train.index, ]


# run logistic regression
logit.reg <- glm(host_is_superhost ~ ., data = train.df, family = "binomial")
options(scipen=999)
summary(logit.reg)

#This logit reg function gives us the beta value. In this case, if a host responds within an hour, he is 1.68 times more likely to be a super host. 
#If the Host Response Time is in a few days or more, Beta = -0.626910319 hence Exp(Beta) = 0.5342398845 and 1/Exp(Beta) = 1.871818314
#This implies that if a host responses within a few days or more, they are 1.871818314 times more likely not to be a super host


# use predict() with type = "response" to compute predicted probabilities.
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")



# first 5 actual and predicted records
data.frame(actual = valid.df$host_is_superhost[1:5], predicted = logit.reg.pred[1:5])



logit.reg.pred.classes <- ifelse(logit.reg.pred > 0.5, 1, 0)



confusionMatrix(as.factor(logit.reg.pred.classes), as.factor(valid.df$host_is_superhost))

#With this confusion matrix, we have a confidence interval of 76.12 to 76.76 and giving us an accuracy of 76.76%.


#model selection
full.logit.reg <- glm(host_is_superhost ~ ., data = train.df, family = "binomial")
empty.logit.reg  <- glm(host_is_superhost ~ 1,data = train.df, family= "binomial")
summary(empty.logit.reg)


backwards = step(full.logit.reg)
summary(backwards)

backwards.reg.pred <- predict(backwards, valid.df, type = "response")
backwards.reg.pred.classes <- ifelse(backwards.reg.pred > 0.5, 1, 0)
confusionMatrix(as.factor(backwards.reg.pred.classes), as.factor(valid.df$host_is_superhost))

#The step function, anything below the 'none' impacts the model as 'none' is the constant term we are running the model and since none of the values are above "none", 
#all the variables contribute to the model. 
#Hence the performance is good in this model based on the step function.

#Doing the logistic regression model both from the front and the back, the accuracy remains the same, 76.44%. 
#We are able to successfully construct model using step function.

x <- roc(as.factor(valid.df$host_is_superhost),logit.reg.pred.classes )
plot.roc(x)
#This ROC curve tells us how effective our model is; the line being further away from the diagonal tells us how accurate our model is and in this case, our model is accurate.

barplot(table(logistic.airbnb.df$host_is_superhost),
main = "Hosts Vs Super Hosts", ylab = "Count", names.arg = c('Hosts', 'Super Hosts'),col="blueviolet")
#This bar plot talks about the total number of hosts and super hosts with comparison to response time.



#---------------------------------
#Model 2
#---------------------------------
#Checking Instantly Bookable with Respect to room type

logistic2.airbnb.df <- read.csv("Listings.csv")
logistic2.airbnb.df <- logistic2.airbnb.df[ , -c(1:4,5,7:8,10:18,20:22,24:25,27:32)]  
View(logistic2.airbnb.df)

# treat Room Type as categorical
logistic2.airbnb.df$room_type <- as.factor(logistic2.airbnb.df$room_type)

sapply(logistic2.airbnb.df,class)
# partition data
set.seed(2)
train.index <- sample(c(1:dim(logistic2.airbnb.df)[1]), dim(logistic2.airbnb.df)[1]*0.7)
train.df <- logistic2.airbnb.df[train.index, ]
valid.df <- logistic2.airbnb.df[-train.index, ]


# run logistic regression
logit.reg <- glm(instant_bookable ~ ., data = train.df, family = "binomial")
options(scipen=999)
summary(logit.reg)


logit.reg.pred <- predict(logit.reg, valid.df, type = "response")

# first 5 actual and predicted records
data.frame(actual = valid.df$instant_bookable[1:5], predicted = logit.reg.pred[1:5])

logit.reg.pred.classes <- ifelse(logit.reg.pred > 0.5, 1, 0)

confusionMatrix(as.factor(logit.reg.pred.classes), as.factor(valid.df$instant_bookable))

#With this confusion matrix, we have a confidence interval of 65.04 to 65.43 and giving us an accuracy of 65.04%.

# model selection
full.logit.reg <- glm(instant_bookable ~ ., data = train.df, family = "binomial")
empty.logit.reg<- glm(instant_bookable ~ 1,data = train.df, family= "binomial")
summary(empty.logit.reg)

backwards = step(full.logit.reg)
summary(backwards)

backwards.reg.pred <- predict(backwards, valid.df, type = "response")
backwards.reg.pred.classes <- ifelse(backwards.reg.pred > 0.5, 1, 0)
confusionMatrix(as.factor(backwards.reg.pred.classes), as.factor(valid.df$instant_bookable))

#In this model, we have compared instantly bookable with respect to the room type. 
#The accuracy of our model from the front gives us an accuracy 65.29%  and while working it backwards, the accuracy is 62.01%. 
#If the type of room is hotel room, the model tells us that a hotel is 2.44 times more likely to be chosen as it instantly bookable while a private room is 0.213 times more likely to be bookable as the response rate is slow.
#A shared room is 0.418 times more likely to be bookable.


x <- roc(as.factor(valid.df$instant_bookable),logit.reg.pred.classes )
plot.roc(x)

#This ROC curve tells us how effective our model is; the line being further away from the diagonal tells us how accurate our model is and in this case, our model is very accurate as our line is much further away from the diagonal.

#---------------------------------
#Model 3
#---------------------------------
#Checking Superhost with Respect to Room Type

logistic3.airbnb.df <- read.csv("Listings.csv")
logistic3.airbnb.df <- logistic3.airbnb.df[ , -c(1:5,7:8,10:18,22,24:25,27:33)] 
View(logistic3.airbnb.df)

# treat Room Type as categorical 
logistic3.airbnb.df$room_type <- as.factor(logistic3.airbnb.df$room_type)

sapply(logistic3.airbnb.df,class)
# partition data
set.seed(2)
train.index <- sample(c(1:dim(logistic3.airbnb.df)[1]), dim(logistic3.airbnb.df)[1]*0.6)
train.df <- logistic3.airbnb.df[train.index, ]
valid.df <- logistic3.airbnb.df[-train.index, ]


# run logistic regression
logit.reg <- glm(host_is_superhost ~ ., data = train.df, family = "binomial")
options(scipen=999)
summary(logit.reg)


# use predict() with type = "response" to compute predicted probabilities.
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")

# first 5 actual and predicted records
data.frame(actual = valid.df$host_is_superhost[1:5], predicted = logit.reg.pred[1:5])

logit.reg.pred.classes <- ifelse(logit.reg.pred > 0.5, 1, 0)

confusionMatrix(as.factor(logit.reg.pred.classes), as.factor(valid.df$host_is_superhost))

# model selection
full.logit.reg <- glm(host_is_superhost ~ ., data = train.df, family = "binomial")
empty.logit.reg<- glm(host_is_superhost ~ 1,data = train.df, family= "binomial")
summary(empty.logit.reg)

backwards = step(full.logit.reg)
summary(backwards)

backwards.reg.pred <- predict(backwards, valid.df, type = "response")
backwards.reg.pred.classes <- ifelse(backwards.reg.pred > 0.5, 1, 0)
confusionMatrix(as.factor(backwards.reg.pred.classes), as.factor(valid.df$host_is_superhost))

#This regression model explains the correlation between room type and super host where the accuracy is 75.06%. 
#There is a negative correlation between super host and room type with a hotel room having 1.218 times not likely to be a super host, private room 1.015 times not likely to be a super host and shared room having 1.717 times less likely to be a super host.

x <- roc(as.factor(valid.df$host_is_superhost),logit.reg.pred.classes )
plot.roc(x)

#This ROC curve tells us how effective our model is;
#the line being further away from the diagonal tells us how accurate our model is and in this case, our model is accurate.

#---------------------------------
#Model 4
#---------------------------------
#Superhost with Respect to City

logistic4.airbnb.df <- read.csv("Listings.csv")
logistic4.airbnb.df <- logistic4.airbnb.df[ , -c(1:4,5,7:8,10:14,16:19,22,24:25,27:33)] 
View(logistic4.airbnb.df)

# treat City as categorical 
logistic4.airbnb.df$city <- as.factor(logistic4.airbnb.df$city)

sapply(logistic4.airbnb.df,class)
# partition data
set.seed(2)
train.index <- sample(c(1:dim(logistic4.airbnb.df)[1]), dim(logistic4.airbnb.df)[1]*0.6)
train.df <- logistic4.airbnb.df[train.index, ]
valid.df <- logistic4.airbnb.df[-train.index, ]


# run logistic regression
logit.reg <- glm(host_is_superhost ~ ., data = train.df, family = "binomial")
options(scipen=999)
summary(logit.reg)


# use predict() with type = "response" to compute predicted probabilities.
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")

# first 5 actual and predicted records
data.frame(actual = valid.df$host_is_superhost[1:5], predicted = logit.reg.pred[1:5])

logit.reg.pred.classes <- ifelse(logit.reg.pred > 0.5, 1, 0)

confusionMatrix(as.factor(logit.reg.pred.classes), as.factor(valid.df$host_is_superhost))

#With this confusion matrix, we have a confidence interval of 76.13 to 76.78 and giving us an accuracy of 76.46%.

# model selection
full.logit.reg <- glm(host_is_superhost ~ ., data = train.df, family = "binomial")
empty.logit.reg<- glm(host_is_superhost ~ 1,data = train.df, family= "binomial")
summary(empty.logit.reg)

backwards = step(full.logit.reg)
summary(backwards)

backwards.reg.pred <- predict(backwards, valid.df, type = "response")
backwards.reg.pred.classes <- ifelse(backwards.reg.pred > 0.5, 1, 0)
confusionMatrix(as.factor(backwards.reg.pred.classes), as.factor(valid.df$host_is_superhost))

#This regression model explains the correlation between city and super host where the accuracy is 76.44% 
#There is a negative correlation between super host and city with a hotel room having 1.218 times not likely to be a super host, 
#people from Hong Kong 1.989 times not likely to be a super host
#people from Sydney 1.75 times less likely to be a super host.
#people from Rio de Janeiro 1.725 times less likely to be a super host.
#people from Mexico City 1.222 times less likely to be a super host.
#people from Rome 1.213 times less likely to be a super host.

x <- roc(as.factor(valid.df$host_is_superhost),logit.reg.pred.classes )
plot.roc(x)


#---------------------------------
#Neural Network  
#---------------------------------
#Loading data set packages
#---------------------------------

library(neuralnet)
library(caret)

set.seed(1)



listings <- read.csv("Listings.csv")
#Importing the data.
View(Omit.df)
Omit.df <- na.omit(listings)
Omit.df$superhost <- Omit.df$host_is_superhost== 1
Omit.df$host <- Omit.df$host_is_superhost== 0
#Cleaning the N/A  and separating the host_is_superhost into two columns 

train.index <- sample(c(1:dim(Omit.df)[1]), dim(Omit.df)[1]*0.7)  
train.df <- Omit.df[train.index, ]
valid.df <- Omit.df[-train.index, ]
#Creating training and validation data set 

random.df <- train.df[sample(nrow(train.df), size=3000), ]
#Taking a random sample of 3000 

nn <- neuralnet(superhost + host ~ review_scores_rating + bedrooms, data = random.df,
                linear.output = F,
                hidden = 3,
                threshold = 0.5,
                learningrate = 1e-6,
                rep =3)
plot(nn, rep="best")
#Plotting a neural network
nn$result.matrix

#Test the resulting output
temp_test <- subset(random.df, select = c("review_scores_rating","bedrooms", "accommodates", "price"))
head(temp_test)
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = random.df$host_is_superhost, prediction = nn.results$net.result)

# display weights
nn$weights

# display predictions
prediction(nn)

roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
roundedresultsdf <- roundedresultsdf[-c(3)]
attach(roundedresultsdf)
table(actual,prediction.1)
#Creating a confusion matrix with prediction.1

#From the confusion matrix we can calculate that the accuracy of the model is 65%
#Sensitivity of the model is 72.875%
#Specificity of the model is 73.878%