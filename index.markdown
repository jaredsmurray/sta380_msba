---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: home
---

# [Syllabus](files/syllabus.pdf)

# Contact and office hours

Please contact me via Canvas message (this helps me prioritize you in my inbox).

Office Hours: Tuesday/Thursday 12-2PM [on Zoom](https://utexas.zoom.us/j/97612392399)

# Class Materials

### Textbook

[Introduction to Statistical Learning with Applications in R](https://www.statlearning.com/), with R code available [here](https://www.statlearning.com/resources-second-edition). Between this R code and the code accompanying lectures, you have plenty of examples to adapt for homework and projects!

### Datasets

Individual datasets are [here](https://github.com/jaredsmurray/sta380_msba/tree/main/data/), or you can download them all [here](data.zip).

### R/Rmarkdown code for textbook labs

See [here](https://www.statlearning.com/resources-second-edition) (under "Rmarkdown files") for completed versions of the labs at the end of each chapter in the text. These are excellent annotated examples that will help you with the takehome problems and projects. Code accompanying lecture slides is below, but for many topics these completed labs are more accessible/adaptable.

### Section 1: Introduction

- [Slides](slides/01-Intro.pdf)
- [R code](R/Intro.R)

### Section 2: Tree Methods, Bagging, and Boosting

- [Slides](slides/02-Trees.pdf)
- [R code](R/Trees_MSBA.R), [California setup](R/cal_setup.txt), [BART example](R/BART_example.R), [Boosting illustration](R/boosting_illustration.R), [caret in Boston housing data](R/caret_example.R)

### Section 2.1: Overview of classification
- [Slides](slides/classification_intro.pdf)
- [R code](R/caret_classification_example.R)

### Section 3: Regression

- [Overview](slides/03-Regression.pdf)
- [R code](R/msba_regression.R)

- [Nonlinearity](slides/nonlinear.pdf)
- [R code](R/nonlinear.R)

- [Categorical predictors](slides/dummy_variables.pdf)
- [R code](R/dummy_variables.R)

- [Interactions](slides/interactions.pdf)
- [R code](R/interactions.R)

- [Building regresion models for prediction](slides/03-Building-Regression.pdf)
- [R code](R/building_regression.R)

- **In-class examples**: [Austin houses MLR](R/ah_linreg_ex.R) [Shrinkage, selection, and dimension reduction](R/caret_dimred.R)

### Section 4: Classification

- [Slides](slides/04-Classification.pdf)
- [R code](R/Classification_MSBA.R), [ClassificationFunctions](R/ClassificationFunctions.R)

### Section 5: (A Brief Intro To) Neural Nets

- [Slides](slides/05-NN.pdf)
- [R code](R/NN_MSBA.R)

### Class Recordings

See Canvas.

# Assignments

- [Take-home problems](files/takehome.pdf). Start working on these early! You can use [this Rmarkdown template](files/template.Rmd).
- [Individual prediction project (and contest!)](files/individual_project.pdf) Data: [austinhouses.csv](data/austinhouses.csv), [austinhouses_holdout.csv](data/austinhouses_holdout.csv)
- [Group project](files/group_project.pdf)

# R/Rstudio Resources

- [Rstudio](https://posit.co/download/rstudio-desktop/). A cross-platform IDE for writing R code.
- [Tutorial](R/Tutorial.pdf) A brief intro to R.
- [Irizarry, "Introduction to Data Science (Data Wrangling and Visualization with R)"](https://rafalab.dfci.harvard.edu/dsbook-part-1/). An excellent reference for modern data science in R using the `ggplot2` and `tidyverse` libraries. 

# Additional Resources

This class is fast-paced, and intended to introduce you to key methods in supervised/predictive machine learning. If you want to go deeper, or learn about other methods and software, here are some additional resources:

- [Hastie, Tibshirani, and Friedman, "The Elements of Statistical Learning"](https://hastie.su.domains/ElemStatLearn/). A more advanced/theoretical version of the class text.
- [Kuhn, "The `caret` package" ](https://topepo.github.io/caret/index.html). In class we will explore various ML methods as implemnted in individual R packages. The `caret` package provides a standardized interface to these methods (and more) that makes it easier to standardize data, do train/test splits, and train/tune/compare/ensemble models.
- [Molnar, "Interpretable Machine Learning"](https://christophm.github.io/interpretable-ml-book/). An accessible review of methods for "interpreting" or "explaining" how ML methods use data to make predictions. Terms like "interpretability"" and "explainability" aren't always defined the same by different authors, and are in some ways trying to solve an ill-posed problem. This book provides a fairly comprehensive review of methods with examples and discussion of their pros and cons.
- [scikit-learn](https://scikit-learn.org/stable/index.html) Popular Python library for machine learning. Your textbook also has a Python equivalent at the same site linked above. Once you're familiar with the methods themselves, moving between languages is (in general) pretty easy!







