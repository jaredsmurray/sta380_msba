# Putting this small dataset right in the R script...
housing = data.frame(
  Size=c(0.8,0.9,1.0,1.1,1.4,1.4,1.5,1.6,1.8,2.0,2.4,2.5,2.7,3.2,3.5),
  Price=c(70,83,74,93,89,58, 85,114, 95,100,138,111,124,161,172)
)

library(ggplot2)

ggplot(aes(y= Price, x=Size), data=housing) + geom_point() 
fit = lm(Price~Size, data=housing)
summary(fit)
confint(fit)

plot(predict(fit)~Size, 
     data=housing, 
     ylab="fitted values yhat")


# cor() computes the sample correlation
cor(predict(fit), housing$Size)

plot(resid(fit)~Size, 
     data=housing, 
     ylab="residuals")

mean(resid(fit));  cor(resid(fit), housing$Size)

# R^2 is on the second to last line of the R output, under
# "Multiple R-squared"
summary(fit)

# The anova() function will extract sums of squares, and do some hypothesis tests.
# We can use it to "check" how the sums of squares are related
anova(fit)

# Prediction intervals
predict(fit, newdata=data.frame(Size=2),
        interval = 'prediction', level = 0.95)

predict(fit, newdata=data.frame(Size=2),
        interval = 'confidence', level = 0.95)
