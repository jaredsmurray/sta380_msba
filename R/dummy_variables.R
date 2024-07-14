## ----include=FALSE------------------------------------------------------------------------------------------
library(readr)
library(mosaic)
path = "https://jaredsmurray.github.io/sta371g_f17/data/"
salary = read_csv(paste0(path, 'salary_gender.csv'))
# Turn the year hired variable into years of experience; the command
# below makes a new column in the salary data frame called Exp
salary %>% mutate(Exp = 96-YrHired)

# This line ensures that R treats the Gender variable as categorical
# when it comes time to fit regression models
salary$Gender = factor(salary$Gender)

salaryfit = lm(Salary~Gender, data=salary)
coef(salaryfit)
confint(salaryfit)

salary %>% group_by(Gender) %>%
  summarize(mean = mean(Salary))

print(45.50544 - 37.20993)

## ----include=FALSE, echo=FALSE------------------------------------------------------------------------------
salaryfit_exp = lm(Salary~Gender+Exp, data=salary)
summary(salaryfit_exp)

## ----out.width="60%", fig.height=4, fig.width=5-------------------------------------------------------------
p = plotModel(salaryfit_exp, Salary~Exp) + geom_point(aes(shape=.color))

## ----include=FALSE------------------------------------------------------------------------------------------
housing = read_csv(paste0(path, 'MidCity.csv'))
housing = housing %>% mutate(Price = Price/1000, Size = SqFt/1000)

housing_fit = lm(Price~factor(Nbhd) + Size, data=housing)
coef(housing_fit)

## ----out.width="70%", fig.height=4, fig.width=5-------------------------------------------------------------
plotModel(housing_fit, Price~Size)

## -----------------------------------------------------------------------------------------------------------
lm(Price~Size, data=housing)

# Since more desirable neighborhoods have larger houses, omitting the location from
# the model leads to a larger overall effect of size vs the partial effect controlling for
# the neighborhood
plotModel(housing_fit, Price~Size) + 
  geom_smooth(aes(x=Size, y=Price), color="black", se=FALSE,
              data=housing, method="lm")


