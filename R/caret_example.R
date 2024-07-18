library(MASS)
library(caret)
library(randomForest)
library(gbm)
library(rpart)
library(rpart.plot)
library(tidyverse)

data(Boston)

set.seed(18)

# Hold out 20% of the data as a final validation set
train_ix = createDataPartition(Boston$medv,
                               p = 0.8)

boston_train = Boston[train_ix$Resample1,]
boston_test  = Boston[-train_ix$Resample1,]

###########################################################################
# Setup cross-validation
###########################################################################

# Define how we're going to estimate OOS error using cross-validation

# Number of folds
kcv = 10

# I'm manually making the folds here so we can look at them, and so
# they're the same when we evaluate each method below. If you omit
# the indexOut argument below caret with make the folds behind the scenes.

cv_folds = createFolds(boston_train$medv,
                               k = kcv)

# This function sets up how we're going to do our training: The method for
# estimating OOS error (CV) and associated settings (here the folds we created 
# above). I'm also going to request that our final fit is determined not by
# the minimum estimated OOS RMSE but using the one standard deviation (aka
# one standard error) rule instead by specifying selectionFunction="oneSE"

fit_control <- trainControl(
  method = "cv",
  indexOut = cv_folds,
  selectionFunction="oneSE")


###########################################################################
# Boosting
###########################################################################

# Boosting, optimizing over default grid for number of trees and depth
gbmfit <- train( medv ~ ., data = boston_train, 
                 method = "gbm", 
                 trControl = fit_control,
                 verbose = FALSE)

# Using a custom grid
gbm_grid <-  expand.grid(interaction.depth = c(1, 2, 3, 5, 10), 
                        n.trees = c(150, 500, 1000), 
                        shrinkage = c(0.01, 0.1, 0.2),
                        n.minobsinnode = 10)

gbmfit_2 <- train(medv ~ ., data = boston_train, 
                 method = "gbm", 
                 trControl = fit_control,
                 tuneGrid = gbm_grid,
                 verbose = FALSE)

print(gbmfit_2)

# It's not 100% clear how to operationalize the one SD rule here -- 
# what is "less complex" in boosting?
# See ?oneSE -- caret sorts all the candidates by n.tree and then 
# interaction.depth to choose a final model. Let's see what else was close:

# Table of results including std dev
print(gbmfit_2$results)

# Determine the max RMSE that's within one SE of best
best_ix = which.min(gbmfit_2$results$RMSE)
best = gbmfit_2$results[best_ix,]
onese_max_RMSE = best$RMSE + best$RMSESD/sqrt(kcv)

# These are the parameter values within one SD:
onese_ixs = gbmfit_2$results$RMSE<onese_max_RMSE

print(gbmfit_2$results[onese_ixs,])

# tidyverse subsetting:
# gbmfit_2$results %>% filter(RMSE<onese_max_RMSE)

# I'm also going to save a fit using the parameters with the
# lowest estimated error:

gbmfit_3 = train(medv ~ ., data = boston_train, 
                 method = "gbm", 
                 trControl = trainControl(method="none"), # No need for CV
                 tuneGrid = best[,1:4],
                 verbose = FALSE)

# Very, very similar in sample fits
plot(predict(gbmfit_2), predict(gbmfit_3))

# Plots of the results. First the defaults:
plot(gbmfit_2)
ggplot(gbmfit_2)

# Or we can build our own to choose facets/colors/etc, and add
# +/- 1 SE

gbm_plot_df = gbmfit_2$results
gbm_plot_df$n.trees = factor(gbm_plot_df$n.trees)

ggplot(aes(x=interaction.depth, y=RMSE, color=n.trees), 
       data=gbm_plot_df) +
  facet_grid(~shrinkage, labeller = label_both) +
  geom_point() + 
  geom_line() + 
  geom_segment(aes(x=interaction.depth, 
                   xend=interaction.depth, 
                   y=RMSE-RMSESD/sqrt(kcv), 
                   yend=RMSE+RMSESD/sqrt(kcv))) + 
  geom_hline(yintercept = onese_max_RMSE, linetype='dotted') +
  xlab("Max Tree Depth") + 
  ylab("RMSE (CV)") + 
  scale_color_discrete(name = "Num Boosting Iter") + 
  theme(legend.position="bottom") 

###########################################################################
# Random forests
###########################################################################

# Optimizing over a default mtry grid
rf_fit <- train( medv ~ ., data = boston_train, 
                 method = "rf", 
                 trControl = fit_control,
                 # parameter ntree below is passed to 
                 # the randomForest function call
                 ntree = 500)

# The default grid is coarse, and missing the usual rf default sqrt(13) = 3 or 4
# Here I'll use a custom grid
rf_grid = data.frame(mtry = c(3,4,7,10,13))
rf_fit <- train( medv ~ ., data = boston_train, 
                 method = "rf", 
                 trControl = fit_control,
                 tuneGrid = rf_grid,
                 ntree = 50)

# Getting a plot of CV error estimates
ggplot(rf_fit)

# Adding +/- one se

best = rf_fit$results[which.min(rf_fit$results$RMSE),]
onesd = best$RMSE + best$RMSESD/sqrt(kcv)

ggplot(rf_fit) + 
  geom_segment(aes(x=mtry, 
                   xend=mtry, 
                   y=RMSE-RMSESD/sqrt(kcv), 
                   yend=RMSE+RMSESD/sqrt(kcv)), 
               data=rf_fit$results) + 
  geom_hline(yintercept = onesd, linetype='dotted')

### Variable importance

# From caret, for methods that support it
imp = varImp(rf_fit, scale=TRUE)

# Recreating the randomForest importance plot by hand
plot_df = data.frame(variable=rownames(imp$importance),
                     rel_importance = imp$importance$Overall)
ggplot(aes(x=reorder(variable, rel_importance), 
           y=rel_importance), data=plot_df) + 
  geom_point() + 
  ylab("Relative importance (RF)") + 
  xlab("Variable") + 
  coord_flip()

# Same as from the randomForest package directly!
varImp(rf_fit$finalModel, scale=FALSE)
varImpPlot(rf_fit$finalModel)

##################################################################
# Comparing RF and Boosting...
##################################################################

# On our validation set:

gbm_yhat = predict(gbmfit_2, newdata=boston_test)
rf_yhat  = predict(rf_fit,   newdata=boston_test)

# Predicted values are very, very similar!
plot(gbm_yhat, rf_yhat)
cor(gbm_yhat, rf_yhat)
abline(0,1)

# So is validation RMSE
sqrt(mean( (boston_test$medv - gbm_yhat)^2 ))
sqrt(mean( (boston_test$medv - rf_yhat)^2 ))

# Comparing variable importance
gbm_imp = varImp(gbmfit_2)
rf_imp  = varImp(rf_fit)
combined_df = data.frame(variable=rownames(gbm_imp$importance),
                         gbm = gbm_imp$importance$Overall,
                         rf  = rf_imp$importance$Overall)

# Very different answers! Related to how correlated variables interact
# with the choice of parameters for each algorithm.
# Variable importance is hard!
View(combined_df)
pairs(Boston %>% select(rm, lstat,indus, crim, dis, nox, ptratio))

###########################################################################
# Single tree
###########################################################################

rpart_grid = data.frame(cp = c(0, exp(seq(log(0.00001), log(0.03), length.out=500))))
single_tree_fit <- train( medv ~ ., data = boston_train, 
                          method = "rpart", 
                          tuneGrid = rpart_grid,
                          trControl = fit_control)

# Extract the final fit
single_tree_fit$finalModel
rpart.plot(single_tree_fit$finalModel)

# For this very special case, it's faster/more efficient to just use rpart

set.seed(1)
bigtree = rpart(medv ~ ., data = boston_train,
                control = rpart.control(cp=0.0009, minsplit=5))
plotcp(bigtree)
printcp(bigtree)
best_cp_ix = which.min(bigtree$cptable[,4]) # "Best"
bigtree$cptable[best_cp_ix,4]

# one sd rule
tol = bigtree$cptable[best_cp_ix,4] + bigtree$cptable[best_cp_ix,5]
bigtree$cptable[bigtree$cptable[,4]<tol,][1,]
best_cp_onesd = bigtree$cptable[bigtree$cptable[,4]<tol,][1,1]
cvtree = prune(bigtree, cp=best_cp_onesd)


# Different looking trees -- mostly due to different CV folds -- 
# but very similar predictions!
plot(predict(cvtree), predict(single_tree_fit$finalModel))
abline(0,1)
# Jittering the predictions a little so they aren't on top of each other
plot(predict(cvtree)+runif(nrow(boston_train), -0.5,  0.5), 
     predict(single_tree_fit$finalModel)+runif(nrow(boston_train), -0.5,  0.5))
abline(0,1)

cor(predict(cvtree), predict(single_tree_fit$finalModel))

