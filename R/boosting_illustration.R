### Boosting illustration

set.seed(1)

n=100
x = runif(n, -2*pi, pi)
xgr = seq(-10, 10, length.out=1000)
y = sin(x)+rnorm(n, 0, 0.3)

md = 2


# Boosting works by building many "weak learners", each of which captures
# a small piece of the overall fit

# sample data
plot(x,y)

# The first weak learner is a small tree fit to the original data
fit_1 = rpart(y~x, control=rpart.control(maxdepth=md))
f1 = predict(fit_1, newdata=data.frame(x=xgr))
lines(xgr, f1)

# Next we "sweep out" the piece of the fit we captured with the first
# learner, and try to capture signal in what's left over (the residuals)
r = y-predict(fit_1)
plot(x, r, ylab = 'residuals')
fit_2 = rpart(r~x, control=rpart.control(maxdepth=md))
f2 = predict(fit_2, newdata=data.frame(x=xgr))
lines(xgr, f2)

# And we keep going; clearly there's less and less "signal"
r = r - predict(fit_2)
plot(x, r, ylab = 'residuals')
fit_3 = rpart(r~x, control=rpart.control(maxdepth=md))
f3 = predict(fit_3, newdata=data.frame(x=xgr))
lines(xgr, predict(fit_3, newdata=data.frame(x=xgr)))

# Combining our three weak learners, we get a decent approximation!
plot(x,y)
lines(xgr, f1+f2+f3, type='l', col='red')

### Adding shrinkage

# As another hedge against overfitting, we typically don't use the
# "best" fit at any step. Instead we shrink each fit toward zero
# and use many more trees

# This is a more standard boosting loop:

# Total number of sweeps (trees/weak learners)
niter = 1000

# Initializing residuals
r = y
# Saving trees for later, to make new predictions
trees = vector(niter, mode='list')

# I'm going to store the fits across a grid of x values 
# from each learner in a matrix, I use it below to make pictures
fits = matrix(NA, length(xgr), niter)

# This is the lambda parameter that additionally weakens the learners
squish = 0.1
for (i in 1:niter) {

  fit = rpart(r~x, control=rpart.control(maxdepth=md, cp=0))
  trees[[i]] = fit
  
  # These three lines evaluate the current tree function on a grid, for plotting
  orig_fit = predict(fit, newdata=data.frame(x=xgr))
  f = squish*orig_fit
  fits[,i] = f
  
  r = r - squish*predict(fit)
  
  if(i==1) {
    # Show the effect of "squishing" for the first tree; works the same
    # for all the trees
    
    plot(x, r)
    lines(xgr, orig_fit, col='red')
    lines(xgr, f, col='red', lty=2)
    
  }
  
}

# Varying nt gives the results for different numbers of trees/iterations
nt = 100
plot(x,y, main=paste0("iter = ", nt))
lines(xgr, rowSums(fits[,1:nt]), type='l', col='red')

# Animation of how the fit proceeds for increasing numbers of trees
for (nt in 2:1000) {
  plot(x,y, main=paste0("iter = ", nt))
  lines(xgr, rowSums(fits[,1:nt]), type='l', col='red')
  Sys.sleep(0.5)
}

# Visualizing fits for a few choices of the number of trees/iterations
plot_df = do.call(rbind, lapply(c(2,10,100, 1000), 
                function(x) data.frame(nt=x, x=xgr, f=rowSums(fits[,1:x]))))

ggplot() + 
  geom_point(aes(x=x,y=y)) + 
  geom_line(aes(x=x, y=f, col=factor(nt)), data=plot_df) + 
  xlim(c(min(x)-0.5, max(x)+0.5))
