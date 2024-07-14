## ----include=FALSE------------------------------------------------------------------------------------------

library(mosaic)
library(readr)
path = "~/Dropbox/teaching/sta380/slides/16-mlr_stuff/"
tele = read_csv(paste0(path, 'tele.csv'))

plot(calls~months, data=tele)
telefit = lm(calls~months, data=tele)

plot(resid(telefit)~fitted(telefit), data=tele)
abline(h=0)

telefit2 = lm(calls~months + I(months^2), data=tele)
print(telefit2)

telefit3 = lm(calls~poly(months, 2), data=tele)
print(telefit3)

summary(telefit2)
summary(telefit3)

cor(predict(telefit2), predict(telefit3))


plotModel(telefit2)
plotModel(telefit)


library(mgcv)

telefit3 = gam(calls~s(months), data=tele)
summary(telefit3)
mean(tele$calls)

# SAT data again
library(faraway)
library(lattice)
library(mosaic)
data(sat)

schoolfit = gam(total~s(expend), data=sat)
par(mfrow=c(2,1))
schoolfit2 = gam(total~s(expend) + s(takers), data=sat)
plot(schoolfit2)
par(mfrow=c(1,1))

# Similar model, but assuming linear trend in expenditures
schoolfit3 = gam(total~ expend + s(takers), data=sat)
summary(schoolfit3)
summary(lm(total~expend + takers, data=sat))

plot(expend~takers, data=sat)

