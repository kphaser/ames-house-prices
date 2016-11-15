##### PREDICT 413 Final Project Code #####

### Load the data ###

library(ggplot2)
library(data.table)
library(caret)
library(VIM)
library(mice)
library(rpart)



# csv files are in the data subdirectory of the project
# ProjectTemplate automates the process
library(ProjectTemplate)
load.project()

# verify data
head(train)
head(test)
nrow(train)
nrow(test)
str(train)
str(test)

# create a time series class of data by ordering data by date and month



### EDA ###
dim(train)
summary(train)
str(train)

plot(train$SalePrice)
hist(train$SalePrice)
hist(log(train$SalePrice+1))

boxplot(split(train$SalePrice,cycle(prcp)),names=month.abb,col="lightblue")

nrow(train)
nrow(unique(train))

anyNA(train)

cat_var <- names(train)[which(sapply(train, is.character))]
cat_car <- c(cat_var, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')
numeric_var <- names(train)[which(sapply(train, is.numeric))]
train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]
train_cat <- train[,.SD, .SDcols = cat_var]
train_cont <- train[,.SD,.SDcols = numeric_var]


### Data Prep ###

# impute missing values using random forest, amelia or mice package?



# trim outliers?



# split training data into training and validation set




### Build Models ###

# Multiple linear regression



# Ridge regression



# Lasso regression



# Poisson regression



# Random forest



# Support vector machines



# XGBoost



# GAM



# Artificial neural network



# ARIMA



# ETS



### Evaluate and Validate Models ###

# check residuals
# compare validation metric




### Model Selection ###




### Submission File ###




