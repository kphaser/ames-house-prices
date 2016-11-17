##### PREDICT 413 Final Project Code #####

### Load the data ###

library(ggplot2)
library(dplyr)
library(data.table)
library(caret)
library(VIM)
library(mice)
library(rpart)


# csv files are in the data subdirectory of the project
# ProjectTemplate automates the process
library(ProjectTemplate)
load.project()

# verify data exists
head(train)
head(test)

# create a time series class of data by ordering data by date and month

# combine train and test sets?



### EDA ###
dim(train)
summary(train)
str(train)

# check for duplicate records
nrow(train)
nrow(unique(train))
nrow(train) - nrow(unique(train))
# check for missing values
colSums(sapply(train, is.na))
# check to make sure there aren't constant features
sapply(train,function(x) length(unique(x)))
# check features with near zero variance
nearZeroVar(train,saveMetrics = TRUE)

# separate categorical and numerical variables
cat_col <- names(train)[which(sapply(train, is.factor))]
cat_var <- c(cat_col, 'BedroomAbvGr', 'HalfBath', 'KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')
num_var <- names(train)[which(sapply(train, is.numeric))]
train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]
train_cat <- train[,.SD,.SDcols = cat_var]
train_cont <- train[,.SD,.SDcols = num_var]


# plots
plot(train$SalePrice)
hist(train$SalePrice)
qqnorm(train$SalePrice)
qqline(train$SalePrice)
hist(log(train$SalePrice+1)) # normalize response variable

par(mfrow=c(1,2))
boxplot(train$SalePrice,col="lightblue")
boxplot(log(train$SalePrice+1),col="red")
par(mfrow=c(1,1))

# histogram of sale price by year and by month
hist(train$SalePrice,"month")
hist(train$SalePrice,"year")


# remodeled homes
sum(train[,'YearRemodAdd', with = FALSE] != train[,'YearBuilt', with = FALSE])
train %>% select(YearBuilt, YearRemodAdd) %>%    mutate(Remodeled = as.integer(YearBuilt != YearRemodAdd)) %>% ggplot(aes(x= factor(x = Remodeled, labels = c( 'No','Yes')))) + geom_bar() + xlab('Remodeled') + theme_light()

# plotting functions for different types of plots
plotHist <- function(data_in, i) {
    data <- data.frame(x=data_in[[i]])
    p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
        theme(axis.text.x = element_text(angle = 90, hjust =1))
    return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
    pp <- list()
    for (i in ii) {
        p <- fun(data_in=data_in, i=i)
        pp <- c(pp, list(p))
    }
    do.call("grid.arrange", c(pp, ncol=ncol))
}


plotDen <- function(data_in, i){
    data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
    p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
        xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
    return(p)
}


train %>% select(LandSlope, Neighborhood, SalePrice) %>% filter(LandSlope == c('Sev', 'Mod')) %>% arrange(Neighborhood) %>% group_by(Neighborhood, LandSlope) %>% summarize(Count = n()) %>% ggplot(aes(Neighborhood, Count)) + geom_bar(aes(fill = LandSlope), position = 'dodge', stat = 'identity') + theme_light() +theme(axis.text.x = element_text(angle = 90, hjust =1))

train %>% select(Neighborhood, SalePrice) %>% ggplot(aes(factor(Neighborhood), SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Neighborhoods')

cor(train_cont)


train %>% select(OverallCond, YearBuilt) %>% ggplot(aes(factor(OverallCond),YearBuilt)) + geom_boxplot() + xlab('Overall Condition')

# plot correlation function
plotCorr <- function(data_in, i){
    data <- data.frame(x = data_in[[i]], SalePrice = data_in$SalePrice)
    p <- ggplot(data, aes(x = x, y = SalePrice)) + geom_point(shape = 1, na.rm = TRUE) + geom_smooth(method = lm ) + xlab(paste0(colnames(data_in)[i], '\n', 'R-Squared: ', round(cor(data_in[[i]], data$SalePrice, use = 'complete.obs'), 2))) + theme_light()
    return(suppressWarnings(p))
}

# selects variables with high correlation
highcorr <- c(names(correlations[,'SalePrice'])[which(correlations[,'SalePrice'] > 0.5)], names(correlations[,'SalePrice'])[which(correlations[,'SalePrice'] < -0.2)])
# subset train of variables with high correlation
data_corr <- train[,highcorr, with = FALSE]

# plot high correlation variables with sale price
doPlots(data_corr, fun = plotCorr, ii = 1:6)




### Data Prep ###

# change variables with meaningful NA to another factor name
# Alley, BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2, GarageType, GarageFinish, GarageQual, GarageCond, PoolQC, Fence, MiscFeature
okNA <- c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "GarageType", "GarageFinish", "GarageQual", "GarageCond", "PoolQC", "Fence", "MiscFeature")
ok_na <- c()

train$Alley[is.na(train$Alley)]


levels(train$Alley) <- c(levels(train$Alley),"None")
levels(train$Alley)
train$Alley[is.na(train$Alley)] <- "None"
head(train$Alley)
class(train$Alley)


# adds another factor level
addNoneLevel <- function(x){
    if(is.factor(x)) return(factor(x, levels=c(levels(x), "None")))
    
    return(x)
}

# for loop to add None factor level and assign None to NA values
for (col in okNA) {
    
}

for (col in train[,okNA,with=FALSE]

train$Alley2 <- addNoneLevel(train$Alley)
train$Alley2[is.na(train$Alley2)] <- "None"

# impute missing values using mean/median, random forest, or mice package?



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




