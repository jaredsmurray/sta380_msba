
#library(gamair)
#data(chicago)

library(readr)
library(ggplot2)
library(mosaic)

data = read_csv("data/chicago.csv")

ggplot(aes(x=tmpd, y=death), data=chicago) + 
  geom_point() + 
  geom_smooth(method="lm")

# Estimate and store a linear model
death.temp.lm = lm(death ~ tmpd, data=chicago)

# Tack the residuals onto our dataset

chicago = chicago %>% mutate(resids = resid(death.temp.lm),
                             stdresids = rstandard(death.temp.lm))

# Always plot residuals vs. predictor variable
# We're looking for nonlinear trends -- indicating E(e_i|X) != 0 --
# and for patterns in the spread indicating Var(e_i|X) isn't constant
ggplot(aes(x=tmpd, y=resids), data=chicago) + 
  geom_point() + 
  geom_smooth()

ggplot(aes(x=tmpd, y=stdresids), data=chicago) + 
  geom_point() + 
  geom_smooth()


# Always plot residuals^2 vs.
# predictor variable
# It's easier to read off trends in the spread of the residuals here -- remember,
# the expected residual should be zero so the level here should be constant
ggplot(aes(x=tmpd, y=resids^2), data=chicago) + 
  geom_point() + 
  geom_smooth()

ggplot(aes(x=tmpd, y=stdresids^2), data=chicago) + 
  geom_point() + 
  geom_smooth()

# Those plots are sensitive to outliers, let's try absolute values
ggplot(aes(x=tmpd, y=abs(resids)), data=chicago) + 
  geom_point() + 
  geom_smooth() + 
  geom_hline(aes(yintercept=mean(abs(resids)), col='red'))

ggplot(aes(x=tmpd, y=abs(stdresids)), data=chicago) + 
  geom_point() + 
  geom_smooth() + 
  geom_hline(aes(yintercept=mean(abs(stdresids)), col='red')) 

## When the data are collected in some order -- time for example -- 
## plot against those too, or even just the row numbers
ggplot(aes(x=as.Date(time,origin="1993-12-31"), y=stdresids), data=chicago) + 
  geom_point() + 
  geom_smooth() + 
  geom_hline(yintercept=0, col='red') 

# Plotting successive residuals against each other
# head() and tail() here are used to get "every day except the last"
# and "every day except the first", respectively
# see help(head)
lag_df = data.frame(resid.today=head(rstandard(death.temp.lm),-1),
     resid.tomorrow = tail(rstandard(death.temp.lm),-1))

ggplot(aes(x=resid.today, y=resid.tomorrow), data=lag_df) +
  geom_point() + 
  geom_smooth()

ggplot(aes(x=resid.today, y=resid.tomorrow), 
       data=lag_df %>% filter(resid.today<5, resid.tomorrow<5)) +
  geom_point() + 
  geom_smooth()

# Always plot distribution of residuals
ggplot(aes(x=stdresids), data=chicago) + 
  geom_histogram(aes(y=..density..), bins=60) + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), col='blue')
  
## ----qqplot-of-residuals, echo=FALSE-------------------------------------
# An alternative: plot observed quantiles vs. theoretical Gaussian quantiles
qqnorm(residuals(death.temp.lm))
qqline(residuals(death.temp.lm))

qqnorm(rstandard(death.temp.lm))
qqline(rstandard(death.temp.lm))

## Does the model generalize? If the model fits well, it should do
## well on new values for X that are ``similar'' to the old. We can
## simulate this by fitting the model on one random subset of the data
## (say 90%) and looking at its fit on the remaining 10%

# Here I'm randomly re-ordering the dataset by sampling all n rows
# WITHOUT replacement, just "shuffling" the rows
chicago_shuffle = chicago %>% slice_sample(n=nrow(chicago))

# The first 90% is training data, used to fit the model. The remainder
# is test data which we'll use to evaluate the model.
n=nrow(chicago)
chicago_train = chicago_shuffle[1:floor(0.9*n),]
chicago_test  = chicago_shuffle[(1+floor(0.9*n)):n,]

# Estimate the model on the training set only
training.lm <- lm(death ~ tmpd, data=chicago_train)

# Make predictions on the testing set
# The model didn't get to see these points while it was being
# estimated, so this really checks (or tests) whether it can
# predict
testing.preds <- predict(training.lm, newdata=chicago_test)
chicago_test = chicago_test %>% mutate(resids=death-testing.preds)

sigma(training.lm)
chicago_test %>% summarize(rsd = sd(resids))


ggplot(aes(x=tmpd, y=resids), data=chicago_test) + 
  geom_point() + 
  geom_hline(yintercept=0, color='red') +
  geom_smooth()


## We can also explicitly test whether our model extrapolates
## just outside the range of the observed data. If so,
## we'd tend to feel pretty good about it!

# Note the difference here - the training observations aren't
# representative of the testing data

chicago_train = chicago %>% filter(tmpd<75)
chicago_test  = chicago %>% filter(tmpd>=75)

# Estimate the model on the training set only
training.lm <- lm(death ~ tmpd, data=chicago_train)

testing.preds <- predict(training.lm, newdata=chicago_test)
chicago_test = chicago_test %>% mutate(resids=death-testing.preds)

sigma(training.lm)
chicago_test %>% summarize(rsd = sd(resids))

ggplot(aes(x=tmpd, y=resids), data=chicago_test) + 
  geom_point() + 
  geom_hline(yintercept=0, color='red') +
  geom_smooth()
