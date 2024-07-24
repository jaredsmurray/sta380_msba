library(readr)
library(tidyverse)
library(ggplot2)
library(rpart)
library(caret)

austinhouses <- read_csv("~/Dropbox/teaching/MSBA 2024/website/data/austinhouses.csv")

austinhouses = austinhouses %>% 
  mutate(livingAreaSqFt = livingAreaSqFt/100)

fit0 = lm(latestPrice~livingAreaSqFt, data=austinhouses)
fit1 = lm(log(latestPrice)~livingAreaSqFt, data=austinhouses)

lm(log(latestPrice)~numOfBedrooms, data=austinhouses)
lm(log(latestPrice)~numOfBedrooms+livingAreaSqFt, data=austinhouses)

## What's going on?

# The overall relationship is positive...
ggplot(aes(y=log(latestPrice), x = numOfBedrooms), 
       data=austinhouses) + 
  geom_point() + geom_smooth(method='lm') 

# But when we subset by buckets based on house size:
austinhouses$area_bucket = cut(austinhouses$livingAreaSqFt, 
                               quantile(austinhouses$livingAreaSqFt, c(0:5)/5), 
                               include.lowest=TRUE)
ggplot(aes(y=log(latestPrice), x = numOfBedrooms, group=area_bucket), 
       data=austinhouses) + 
  geom_point() + geom_smooth(method='lm') +
  facet_wrap(~area_bucket)

# We see two things from this plot: One, when we compare
# houses of similar size that differ in the number of beds,
# the relationship is negative in most of the buckets.
# Just adding bedrooms without increasing the size of the house
# might be counterproductive for most houses.

# For large houses the relationship seems like it might be
# positive! Maybe we have an interaction here -- the effect
# of an additional bedroom (adjusting for size) depends
# on the actual size of the house.

# Why do the adjusted and unadjusted effects differ?
# Both variables are important predictors, and they're highly
# correlated
ggplot(aes(y=livingAreaSqFt, x=numOfBedrooms), data=austinhouses) + 
  geom_point(position='jitter', alpha=0.2)

# Recoding num_beds as categorical -- why?
table(austinhouses$numOfBedrooms)
austinhouses = 
  austinhouses %>% mutate(num_beds = cut(numOfBedrooms, c(0,2:4,8), include.lowest = TRUE))
table(austinhouses$num_beds, austinhouses$numOfBedrooms)

# We still see the "flipped" associations
fit4 = lm(log(latestPrice)~num_beds, data=austinhouses %>% filter(numOfBedrooms>0))
fit5 = lm(log(latestPrice)~num_beds + livingAreaSqFt, data=austinhouses)

# A useful aside: to create one-hots/dummy variables we can use
onehot_maker = dummyVars(~num_beds+factor(zipcode), data=austinhouses,
                    fullRank=FALSE 
                    )
# fullRank = FALSE gives k redundant dummies, instead of k-1 as in 
# linear regression. This is OK for models that use regularization, or
# tree algorithms -- almost anything except standard 
# linear/logistic regression in fact. BUT: You should still think carefully
# about how to encode categorical variables with many levels! (what should)
# we do with the zips that have very few houses?

# Generating onehots
onehots = predict(onehot_maker, newdata=austinhouses)

# Manually making onehots can be useful during CV, especially for
# categorical variables with many levels, where we might be missing a 
# level entirely from one or more folds. But! Before we one hot encode,
# do some EDA/thinking about whether we can combine or recode categories
# like we did for num_beds.
