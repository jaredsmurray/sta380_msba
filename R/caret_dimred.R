library(MASS)
library(caret)
library(randomForest)
library(glmnet)
library(tidyverse)

data(Boston)

set.seed(1022)

# Hold out 20% of the data as a final validation set
train_ix = createDataPartition(Boston$medv,
                               p = 0.8)

boston_train = Boston[train_ix$Resample1,]
boston_test  = Boston[-train_ix$Resample1,]

###########################################################################
# Lasso, Ridge, Stepwise selection
###########################################################################

# For lasso and ridge, CV is **much** faster using the glmnet package directly
# For stepwise, we don't need CV at all

lasso = cv.glmnet(x = as.matrix(boston_train[,-14]), 
                  y = boston_train[,14])
LamL = lasso$lambda.1se
plot(-log(lasso$lambda),sqrt(lasso$cvm),
     main="LASSO CV (k=10)",
     xlab="-log(lambda)",
     ylab = "RMSE",col=4,type="b",cex.lab=1.2)
abline(v=-log(LamL),lty=2,col=2,lwd=2)
coef(lasso, s=LamL)
# CV estimated RMSE for 1 SE rule
sqrt(lasso$cvm)[lasso$lambda == LamL]

ridge = cv.glmnet(x = as.matrix(boston_train[,-14]), 
                  y = boston_train[,14],
                  alpha=0)
LamR = ridge$lambda.1se
plot(-log(ridge$lambda),sqrt(ridge$cvm),
     main="ridge CV (k=10)",
     xlab="-log(lambda)",
     ylab = "RMSE",col=4,type="b",cex.lab=1.2)
abline(v=-log(LamR),lty=2,col=2,lwd=2)
coef(ridge, s=LamR)
sqrt(ridge$cvm)[ridge$lambda == LamR]

plot(coef(ridge, s=LamR), coef(lasso, s=LamL))
abline(0,1)

### Fw/BW/Stepwise selection
null = lm(medv~1, data=boston_train)
full = lm(medv~., data=boston_train)

stepwise = step(lm(medv~., data=boston_train),
                direction="both",
                scope = ~.)

stepwise_int  = step(full,
                direction="both",
                scope = ~(.)^2)

stepwise_int2 = step(lm(medv~(.)^2, data=boston_train),
                direction="both",
                scope = ~.)

# LASSO again with interactions
xx = model.matrix(medv~(.)^2, data=Boston)
xx_train = xx[train_ix$Resample1,]
xx_test  = xx[-train_ix$Resample1,]

lasso_int = cv.glmnet(x = xx_train[,-1], #Drop the intercept 
                      y = boston_train[,14])
LamLi = lasso_int$lambda.1se
plot(-log(lasso_int$lambda),sqrt(lasso_int$cvm),
     main="LASSO CV (k=10)",
     xlab="-log(lambda)",
     ylab = "RMSE",col=4,type="b",cex.lab=1.2)
abline(v=-log(LamLi),lty=2,col=2,lwd=2)
coef(lasso_int, s=LamLi)
# CV estimated RMSE for 1 SE rule
sqrt(lasso_int$cvm)[lasso_int$lambda == LamLi]

###########################################################################
# PCR, PLS
###########################################################################

# Examining principal components

pc = prcomp(Boston %>% dplyr::select(-medv), 
            center=TRUE, scale=TRUE)
plot(pc)
pc$rotation
pcs = predict(pc)
par(mfrow=c(3,5)) 

for(k in 1:13) {
  plot(pcs[,2]~Boston[,k], main=colnames(Boston)[k])
}
par(mfrow=c(1,1)) 

# Using caret to select the number of components in PCR/PLS

# Hold out 20% of the data as a final validation set
train_ix = createDataPartition(Boston$medv,
                               p = 0.8)

boston_train = Boston[train_ix$Resample1,]
boston_test  = Boston[-train_ix$Resample1,]

# Define how we're going to estimate OOS error using cross-validation

# Number of folds
kcv = 10

cv_folds = createFolds(boston_train$medv,
                       k = kcv)

fit_control <- trainControl(
  method = "cv",
  indexOut = cv_folds,
  selectionFunction="oneSE")

pcrfit <- train( medv ~ ., data = boston_train, 
                 method = "pcr", 
                 trControl = fit_control,
                 tuneGrid = data.frame(ncomp=1:13),
                 verbose = FALSE)

plsfit <- train( medv ~ ., data = boston_train, 
                 method = "pls", 
                 trControl = fit_control,
                 tuneGrid = data.frame(ncomp=1:13),
                 verbose = FALSE)

# As expected, PLS uses a few fewer components than PC regression, since
# the components are extracted to predict y
plot(pcrfit)
plot(plsfit)

# Random forests, for comparison
rffit <- train( medv ~ ., data = boston_train, 
                 method = "rf", 
                 trControl = fit_control,
                 tuneGrid = data.frame(mtry=c(3,4,7)),
                 verbose = FALSE)

# What if we add the principal components as potential split variables?

pre_proc = preProcess(Boston, method="pca")
boston_train_pc = cbind(boston_train, predict(pre_proc,boston_train))
boston_test_pc  = cbind(boston_test, predict(pre_proc,boston_test))

# Random forests, again but with pcs
rffit_pc <- train( medv ~ ., data = boston_train_pc, 
                method = "rf", 
                trControl = fit_control,
                tuneGrid = data.frame(mtry=c(3,7,15,20)))

# Anything surprising here??
varImpPlot(rffit$finalModel)
varImpPlot(rffit_pc$finalModel)

# Let's try some variable selection, just for fun
rffit_subs <- train( medv ~ rm+lstat, data = boston_train_pc, 
                        method = "rf", 
                        trControl = fit_control,
                        tuneGrid = data.frame(mtry=c(1:2)))

rffit_pc_subs <- train( medv ~ rm+lstat+PC2+PC1, data = boston_train_pc, 
                   method = "rf", 
                   trControl = fit_control,
                   tuneGrid = data.frame(mtry=c(1:4)))

### Validation time

sqrt(mean((boston_test$medv - predict(lasso, s=LamL, newx = as.matrix(boston_test[,-14])))^2))
sqrt(mean((boston_test$medv - predict(ridge, s=LamR, newx = as.matrix(boston_test[,-14])))^2))
sqrt(mean((boston_test$medv - predict(lasso_int, s=LamLi, newx = xx_test[,-1]))^2))

sqrt(mean((boston_test$medv - predict(stepwise, newdata = boston_test))^2))
sqrt(mean((boston_test$medv - predict(stepwise_int, newdata = boston_test))^2))
sqrt(mean((boston_test$medv - predict(stepwise_int2, newdata = boston_test))^2))

sqrt(mean((boston_test$medv - predict(plsfit, newdata = boston_test))^2))
sqrt(mean((boston_test$medv - predict(pcrfit, newdata = boston_test))^2))

sqrt(mean((boston_test$medv - predict(rffit, newdata = boston_test))^2))
sqrt(mean((boston_test$medv - predict(rffit_subs, newdata = boston_test_pc))^2))
sqrt(mean((boston_test$medv - predict(rffit_pc, newdata = boston_test_pc))^2))
sqrt(mean((boston_test$medv - predict(rffit_pc_subs, newdata = boston_test_pc))^2))
