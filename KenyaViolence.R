install.packages("tidyverse")
library(tidyverse)
kenyaviolence <- read.csv(
  "C:/Users/HomePC/Desktop/1997-03-03-2024-04-01-Eastern_Africa-Kenya (1).csv", 
  header= TRUE)

library(dplyr)
glimpse(kenyaviolence)
View(kenyaviolence)

# making my first row the header

colnames(kenyaviolence) <- kenyaviolence[1, ]
kenyaviolence <- kenyaviolence[-1, ]
rownames(kenyaviolence) <- NULL
View(kenyaviolence)
 
#Familiarizing with the data

glimpse(kenyaviolence)
colnames(kenyaviolence)
class(kenyaviolence)
summary(kenyaviolence)

# Data cleaning
# Checking for missing values

colSums(is.na(kenyaviolence))

# There are no missing values
# Check for duplicates
kenyaviolence[duplicated(kenyaviolence),]
# There are no duplicated rows in our data set

# Data exploration
# Frequency tables

disorder_types <- table(kenyaviolence$disorder_type)
disorder_types

library(ggplot2)
ggplot(data = kenyaviolence, mapping = aes(x = disorder_type))+
  geom_bar()+
  coord_flip()+
  labs(title = "Frequency of Disorder Types", x = "Disorder Type", y = "Count")

event_types <- table(kenyaviolence$event_type)
print(event_types)
ggplot(kenyaviolence, aes(x= event_type))+
  geom_bar()+
  labs(title = "Frequency of event types", x = "Event type", y= "count")

type_subevent_types <- unique(kenyaviolence$sub_event_type)
type_subevent_types

sub_event_types <- table(kenyaviolence$sub_event_type)
sub_event_types
print(event_types)
ggplot(kenyaviolence, aes(x= sub_event_type))+
  geom_bar()+
  labs(title = "Frequency of sub_event types", x = "Sub_event type", y= "count")+
  theme(axis.text.x = element_text(angle = 75, hjust = 1))

actors <- unique(kenyaviolence$actor1)
actors

# Getting the main actors in Conflict in Kenya
kenyaviolence %>% group_by(actor1) %>% count() %>% arrange(desc(n)) %>% head(10)

associate_actors<- unique(kenyaviolence$assoc_actor_1)
associate_actors

# Getting associate actors in conflict in Kenya
kenyaviolence %>% group_by(assoc_actor_1) %>% count() %>% arrange(desc(n)) %>% head(10)

# The counties with the the highest conflict rates
counties <- kenyaviolence %>% group_by(admin1) %>% count() %>% arrange(desc(n))
counties
# The counties with the lowest conflict rates
counties %>% arrange(n)

# Sub-counties with the highest conflict rates
sub_counties <- kenyaviolence %>% group_by(admin2) %>% count() %>% arrange(desc(n))
sub_counties
# sub_counties with the lowest conflict rates
sub_counties %>% arrange(n)

#mapping conflicts
kenyaviolence$latitude <- as.numeric(kenyaviolence$latitude)
kenyaviolence$longitude <- as.numeric(kenyaviolence$longitude)
#mapping <- kenyaviolence %>% select(latitude, longitude)

kenyaviolence$event_type <- as.factor(kenyaviolence$event_type)
kenyaviolence$disorder_type <- as.factor(kenyaviolence$disorder_type)
ggplot(kenyaviolence, aes(x = longitude, y = latitude)) +
  geom_point(color = "blue") + 
  coord_quickmap()+
  labs(title = "Map of conflict zones in Kenya", x = "Longitude", y = "Latitude")  # Add labels


# Mapping event_types on Kenyan map
ggplot(kenyaviolence, aes(x = longitude, y = latitude)) +
  geom_point(aes(color = event_type)) + 
  coord_quickmap()+
  labs(title = "Map of conflict zones in Kenya", x = "Longitude", y = "Latitude")  # Add labels
# Mapping disorder_types on the Kenyan Map
ggplot(kenyaviolence, aes(x = longitude, y = latitude)) +
  geom_point(aes(color = disorder_type)) + 
  coord_quickmap()+
  labs(title = "Map of conflict zones in Kenya", x = "Longitude", y = "Latitude")  # Add labels

# Statistical analysis of fatalities in conflicts in Kenya
# Number of fatalities the previous year 2023
kenyaviolence$year <- as.numeric(kenyaviolence$year)
kenyaviolence$fatalities <- as.numeric(kenyaviolence$fatalities)
fatalities_by_year <- kenyaviolence %>% select(year, fatalities) %>% 
  group_by(year) %>% summarise(total_fatalities = sum(fatalities))
fatalities_by_year %>% arrange(desc(total_fatalities))
ggplot(fatalities_by_year, aes(x = year, y = total_fatalities)) + 
  geom_line()
ggplot(fatalities_by_year, aes(x = year, y = total_fatalities)) + 
  geom_col()

# Checking fatalities per event type
fatalities_by_event_type <- kenyaviolence %>% select(event_type, fatalities) %>% 
  group_by(event_type) %>% summarise(total_fatalities = sum(fatalities), mean_fatalities = mean(fatalities))
fatalities_by_event_type %>% arrange(desc(total_fatalities))

# Checking fatalities per disorder type
fatalities_by_disorder_type <- kenyaviolence %>% select(disorder_type, fatalities) %>% 
  group_by(disorder_type) %>% summarise(total_fatalities = sum(fatalities), mean_fatalities = mean(fatalities))
fatalities_by_disorder_type %>% arrange(desc(total_fatalities))

# Checking the universal mean fatality rate in the country per conflict
mean(kenyaviolence$fatalities)
# T-test to prove that we can say with certainty that there will be a fatality in every conflict in Kenya
#z.test(kenyaviolence$fatalities ~ 1)
# Make conclusion ???

# Data Transformation
# Creating dataset 2
kenyaViolence2 <- kenyaviolence %>% select(event_date, year, disorder_type, event_type, sub_event_type, actor1, actor2, civilian_targeting, admin1, admin2, latitude, longitude, fatalities)
View(kenyaViolence2)
kenyaViolence2$event_date <- as.Date(kenyaViolence2$event_date, format = "%d-%b-%y")
glimpse(kenyaViolence2)
kenyaViolence2$actor1 <- as.factor(kenyaViolence2$actor1)
kenyaViolence2$actor2 <- as.factor(kenyaViolence2$actor2)
kenyaViolence2$admin1 <- as.factor(kenyaViolence2$admin1)
kenyaViolence2$admin2 <- as.factor(kenyaViolence2$admin2)
kenyaViolence2$sub_event_type <- as.factor(kenyaViolence2$sub_event_type)
kenyaViolence2$civilian_targeting <- as.factor(kenyaViolence2$civilian_targeting)
glimpse(kenyaViolence2) 


# Checking for correlations
library(GGally)
ggcorr(kenyaViolence2 %>% mutate_if(is.factor, as.numeric) %>% mutate_if(is.Date, as.numeric) , label=TRUE, label_size=3, hjust=0.95, size=3)
# From the visualization we can see that there are not a lot of variables that correlate with fatalities leading us to believe that we may not
# able to create a linear model of high accuracy with fatalities as the response variable.
# To acertain we conduct regression analysis.

# Significant correlations
cor(kenyaViolence2$event_type, kenyaViolence2$civilian_targeting)
# Regression Analysis of conflict fatalities in Kenya
set.seed(123)
split<- sample(nrow(kenyaViolence2),nrow(kenyaViolence2)*0.8)
train_data<-kenyaViolence2[split,]
test_data<-kenyaViolence2[-split,]
model <- lm(fatalities ~., data = kenyaViolence2)
summary(model)

model <- lm(fatalities ~ actor1 + actor2 + event_type + sub_event_type + admin1 +admin2, data = kenyaViolence2)
summary(model)


# Predicting fatalities using the model
# Checking the accuracy
library(caret)
library(e1071)
predictions <- predict(model, test_data, type = "response")
result<-data.frame(test_data$fatalities,predict(model,test_data,type = "response"))
result


# Creating a time series for our data
# Time series analysis of conflict fatalities in Kenya 
library(tseries)
library(astsa)
library(forecast)
kenvio_ts <- kenyaViolence2 %>% select(event_date, fatalities) %>% group_by(event_date) %>% summarise(total_fatalities = sum(fatalities)) %>% arrange(event_date)
head(kenvio_ts)
class(kenvio_ts$event_date)
start_date <- min(kenvio_ts$event_date)
end_date <- max(kenvio_ts$event_date)
data_ts <- ts(kenvio_ts$total_fatalities,start = c(year(start_date), yday(start_date)), end = c(year(end_date), yday(end_date)), frequency = 365)
class(data_ts)
head(data_ts)
plot.ts(data_ts)

#Decompose the data
dec_data <- decompose(data_ts)
summary(dec_data)
plot(dec_data)
# Getting specific plots for the decomposed data
plot(dec_data$trend, main="Trend Component", ylab="Trend")
plot(dec_data$seasonal, main="Seasonal Component", ylab="Seasonal")
plot(dec_data$random, main="Residual Component", ylab="Residual")

adf.test(data_ts) # since the p-value is 0.01 we conclude that the series is stationary

acf2(data_ts)# Gathering the ACF and PACF graphs

# Differencing the ts dataset
# We differentiate to achieve stationarity

fit <- auto.arima(data_ts)
summary(fit)

seas <- auto.arima(dec_data$seasonal)
summary(seas)
# Getting SARIMA diagnostics
sarima(data_ts, 5, 1, 3)

# Creating the ts model
modelts <- arima(data_ts,order = c(5, 1, 3))
modelts
# Forecasting for the next year
forecasted <- forecast(modelts, h = 365 )
plot(forecasted)
# Machine learning analysis of conflict fatalities in Kenya

# How to run a t-test
# How to drop NA values in R

