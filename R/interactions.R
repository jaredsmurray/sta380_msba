## ----include=FALSE------------------------------------------------------------------------------------------
library(readr)
library(mosaic)

path = "https://jaredsmurray.github.io/sta371g_f17/data/"
salary = read_csv(paste0(path, 'salary_gender.csv'))
# Turn the year hired variable into years of experience; the command
# below makes a new column in the salary data frame called Exp
salary$Exp = 96-salary$YrHired

# This line ensures that R treats the Gender variable as categorical
# when it comes time to fit regression models
salary$Gender = factor(salary$Gender)

# The model from last time
salaryfit_exp = lm(Salary~Gender+Exp, data=salary)

plotModel(salaryfit_exp, Salary~Exp)

salaryfit_int = lm(Salary~Gender*Exp, data=salary)

summary(salaryfit_int)

plotModel(salaryfit_int, Salary~Exp)

gap1 = do(1000) * {
  fit = lm(Salary ~ Gender * Exp, data = resample(salary))
  betas = coef(fit)
  exper = 5 # 25th percentile of experience
  betas[2] + betas[4]*exper
}

confint(gap1)

gap2 = do(1000) * {
  fit = lm(Salary ~ Gender * Exp, data = resample(salary))
  betas = coef(fit)
  exper = 10 
  betas[2] + betas[4]*exper
}

confint(gap2)

## ----include=FALSE------------------------------------------------------------------------------------------
gpa = read_csv(paste0(path, 'grades.csv'))

fit1 = lm(MBAGPA ~BachGPA+Age, data=gpa)
coef(fit1)

# The model with interactions
lm(MBAGPA ~ BachGPA*Age, data=gpa)


