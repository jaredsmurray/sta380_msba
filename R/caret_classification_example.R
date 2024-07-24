library(MASS)
library(caret)
library(randomForest)
library(gbm)
library(rpart)
library(rpart.plot)
library(tidyverse)

library(ISLR2)
library(MLmetrics)
library(MLeval)

data(Default)

# caret will treat the first level as "positive", here I 
# make sure that Yes = positive for performance metrics
Default$default = factor(Default$default, 
                         levels=c("Yes", "No"))

Default  = Default %>% 
  mutate(default = factor(default, levels=c("Yes","No")))

set.seed(18)

# Hold out 20% of the data as a final validation set
train_ix = createDataPartition(Default$default,
                               p = 0.8)

default_train = Default[train_ix$Resample1,]
default_test  = Default[-train_ix$Resample1,]

# Note that caret used stratified sampling to preserve
# the balance of Y/N:
table(Default$default[train_ix$Resample1]) %>% 
  prop.table
table(Default$default[-train_ix$Resample1]) %>% 
  prop.table

###########################################################################
# Setup cross-validation
###########################################################################

# Define how we're going to estimate OOS error using cross-validation

# Number of folds
kcv = 10

# I'm manually making the folds here so we can look at them, and so
# they're the same when we evaluate each method below. If you omit
# the indexOut argument below caret with make the folds behind the scenes.

cv_folds = createFolds(default_train$default,
                               k = kcv)

# This function sets up how we're going to do our training: The method for
# estimating OOS error (CV) and associated settings (here the folds we created 
# above). I'm also going to request that our final fit is determined not by
# the minimum estimated OOS RMSE but using the one standard deviation (aka
# one standard error) rule instead by specifying selectionFunction="oneSE"

# Defining a new summary function that computes a few different 
# error metrics using pre-defined summaries

my_summary = function(data, lev = NULL, model = NULL) {
  default = defaultSummary(data, lev, model)
  twoclass = twoClassSummary(data, lev, model)
  # Converting to TPR and FPR instead of sens/spec
  twoclass[3] = 1-twoclass[3]
  names(twoclass) = c("AUC_ROC", "TPR", "FPR")
  logloss = mnLogLoss(data, lev, model)
  c(default,twoclass, logloss)
}

fit_control <- trainControl(
  method = "cv",
  indexOut = cv_folds,
  # Save predicted probabilities, not just classifications
  classProbs = TRUE,
  # Save all the holdout predictions, to summarize and plot
  savePredictions = TRUE,
  summaryFunction = my_summary,
  selectionFunction="oneSE")

###########################################################################
# Boosting
###########################################################################

gbm_grid <-  expand.grid(interaction.depth = c(1, 3, 5, 10), 
                         n.trees = c(100, 500, 750, 1000, 1500), 
                         shrinkage = c(0.1),
                         n.minobsinnode = 10)

# I'm going to exclude some combinations to make CV faster
gbm_grid = gbm_grid %>% 
  filter(!(n.trees>1000 & interaction.depth>5))

gbmfit <- train(default ~ ., data = default_train, 
                 method = "gbm", 
                 trControl = fit_control,
                 tuneGrid = gbm_grid,
                 metric = "logLoss",
                 verbose = FALSE)

print(gbmfit)
plot(gbmfit)

# Extracting performance summaries
# Confusion matrix as proportions, not counts, since 
# the test dataset varies across folds
# These are CV estimates of error rates/accuracy using a *default* cutoff
# to classify cases

confusionMatrix(gbmfit)

thresholder(gbmfit, 
            threshold = 0.9, 
            final = TRUE,
            statistics = c("Sensitivity",
                           "Specificity"))

gbmfit_res = thresholder(gbmfit, 
                         threshold = seq(0, 1, by = 0.01), 
                         final = TRUE)

# How do metrics vary with the threshold?
pldf = gbmfit_res %>%
  mutate(TPR=Sensitivity, FPR = 1-Specificity, FNR = 1-Sensitivity) %>%
  dplyr::select(-c(n.trees, interaction.depth, shrinkage, n.minobsinnode)) %>%
  pivot_longer(-prob_threshold) 

ggplot(aes(x=prob_threshold, y=value, color=name), 
       data=pldf %>% filter(name %in% c("TPR", "FPR"))) + 
  geom_line() 

ggplot(aes(x=prob_threshold, y=value, color=name), 
       data=pldf %>% filter(name %in% c("FNR", "FPR"))) + 
  geom_line() 

# plot(J~prob_threshold, data=gbmfit_res, type='l')

# How do we get points on the ROC curve? One (TPR, FPR) pair for each threshold

thres = 0.5
tp = gbmfit_res %>% 
  dplyr::filter(prob_threshold==thres) %>% 
  dplyr::select(prob_threshold, Sensitivity, Specificity) %>%
  mutate(TPR=Sensitivity, FPR = 1-Specificity)

ggplot(aes(x=prob_threshold, y=value, color=name), 
       data=pldf %>% filter(name %in% c("TPR", "FPR"))) + 
  geom_line() + 
  geom_vline(xintercept=thres, lty=2) + 
  geom_point(aes(x=prob_threshold, y=TPR, color=NULL), data=tp) + 
  geom_point(aes(x=prob_threshold, y=FPR, color=NULL), data=tp) 

# ROC curve

optim_J = gbmfit_res[which.max(gbmfit_res$J),]

ggplot(aes(x=prob_threshold, y=J), 
       data=gbmfit_res) + 
  geom_line() + 
  geom_vline(aes(xintercept=optim_J$prob_threshold), lty=2)

ggplot(aes(x=1-Specificity, y=Sensitivity), data=gbmfit_res) + 
  geom_line() + 
  ylab("TPR (Sensitivity)") + 
  xlab("FPR (1-Specificity)") + 
  geom_abline(intercept=0, slope=1, linetype='dotted') +
  geom_segment(aes(x=1-Specificity, xend=1-Specificity, y=1-Specificity, yend=Sensitivity), color='darkred', data=optim_J) + 
  theme_bw()

# PR curve

ggplot(aes(x=prob_threshold, y=value, color=name), 
       data=pldf %>% filter(name %in% c("Precision", "Recall"))) + 
  geom_line() 

ggplot(aes(x=Recall, y=Precision), data=gbmfit_res) + 
  geom_point() + 
  geom_line() + 
  ylab("Precision") + 
  xlab("Recall (TPR)") + 
  geom_point(aes(x=Recall, y=Precision), color='darkred', data=optim_J) + 
  theme_bw()

# Lift curve

# Extract predicted probs for best-fitting model
# For each observation it's predicted prob is computed when its
# fold is the testing/holdout dataset during CV
best_pars = gbmfit$bestTune
best_preds = gbmfit$pred %>% filter(n.trees==best_pars$n.trees, 
                                      interaction.depth==best_pars$interaction.depth)

gbm_lift = caret::lift(obs~Yes, data=best_preds)

ggplot(gbm_lift) + 
  geom_abline(slope=1, linetype='dotted') +
  xlim(c(0, 100)) + 
  theme_bw() + xlim(0,10)

# Calibration plot

gbm_cal = caret::calibration(obs~Yes, data=best_preds, cuts=7)
ggplot(gbm_cal) + theme_bw()

############################################################################
# Holdout set results
############################################################################

test_probs = predict(gbmfit, newdata=default_test, type="prob")

get_metrics = function(threshold, test_probs, true_class, 
                       pos_label, neg_label) {
  # Get class predictions
  pc = factor(ifelse(test_probs[pos_label]>threshold, pos_label, neg_label), levels=c(pos_label, neg_label))
  test_set = data.frame(obs = true_class, pred = pc, test_probs)
  my_summary(test_set, lev=c(pos_label, neg_label))
}

# Get metrics for a given threshold
get_metrics(0.5, test_probs, default_test$default, "Yes", "No")

# Compute metrics on test data using a grid of thresholds
thr_seq = seq(0, 1, length.out=500)
metrics = lapply(thr_seq, function(x) get_metrics(x, test_probs, default_test$default, "Yes", "No"))
metrics_df = data.frame(do.call(rbind, metrics))

# ROC curve

ggplot(aes(x=FPR, y=TPR), data=metrics_df) + 
  geom_line() +
  ylab("TPR (Sensitivity)") + 
  xlab("FPR (1-Specificity)") + 
  geom_abline(intercept=0, slope=1, linetype='dotted') +
  annotate("text", x=0.75, y=0.25, 
           label=paste("AUC:",round(metrics_df$AUC_ROC[1], 2))) +
  theme_bw()

# Lift

gbm_oos_lift = caret::lift(default_test$default~test_probs[,1])

ggplot(gbm_oos_lift) + 
  geom_abline(slope=1, linetype='dotted') +
  xlim(c(0, 100)) + 
  theme_bw()

# Calibration

gbm_cal = caret::calibration(default_test$default~test_probs[,1], 
                             data=best_preds, cuts=7)
ggplot(gbm_cal) + theme_bw()
