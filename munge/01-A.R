# Setup environment to maximize parellel computing
rm(list=ls())
memory.size(max=T)
library(doParallel)
library(foreach)
c1=makeCluster(2)
registerDoParallel(c1)

# Load the libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(caret)
library(moments)
library(corrplot)
library(VIM)

# load in data with stringsAsFactors=FALSE for one-hot encoding
train <- read.csv("data/train.csv",stringsAsFactors=FALSE)
test <- read.csv("data/test.csv",stringsAsFactors=FALSE)


# Save copy of train data for initial EDA
train_eda <- train

# Combine train and test
total <- rbind(dplyr::select(train,MSSubClass:SaleCondition),
               dplyr::select(test,MSSubClass:SaleCondition))



# log transformation of SalePrice
train$SalePrice <- log(train$SalePrice+1)




# Remove near zero variance features or constant features
removeVar <- c("Street","Alley","Utilities","LotConfig","Condition2","RoofMatl","ExterCond","BsmtFinSF2","Heating","LowQualFinSF",
               "BsmtHalfBath","EnclosedPorch","X3SsnPorch","PoolQC","MiscFeature","MiscVal","MoSold","SaleType")
total <- total[,!(names(total) %in% removeVar)]


# get numeric variables and determine the variables that have high skew 
feature_classes <- sapply(names(total),function(x){class(total[[x]])})
numeric_vars <- names(feature_classes[feature_classes!="character"])
skewed_vars <- sapply(numeric_vars,function(x){skewness(total[[x]],na.rm=TRUE)})
skewed_vars <- skewed_vars[skewed_vars>0.75]



# impute missing values in numeric variables with mean
numeric_df <- total[numeric_vars]
for(x in numeric_vars){
    mean_value <- mean(train[[x]],na.rm=TRUE)
    total[[x]][is.na(total[[x]])] <- mean_value
}


# Create new features
total$NewHome <- ifelse(total$YearBuilt>=2000,1,0)
total$Basement <- ifelse(total$TotalBsmtSF>0,1,0)
total$RemodelAge <- total$YrSold-total$YearRemodAdd
total$Fireplace <- ifelse(total$Fireplaces>0,1,0)
total$Garage <- ifelse(total$GarageCars>0,1,0)
total$GarageAge <- total$YrSold-total$GarageYrBlt
total$HouseAge <- total$YrSold-total$YearBuilt
total$Pool <- ifelse(total$PoolArea>0,1,0)
total$Fencing <- ifelse(!is.na(total$Fence),1,0)

# check summary the data
summary(total)

# there are negative values that don't make sense which need investigating
total[which(total$RemodelAge<0),] <- 0
total[which(total$HouseAge<0),] <- 0
# one of the homes have a garage built in 2207. This doesn't seem right so we'll fix it 2007.
total$GarageYrBlt[2593] <- 2007
total[which(total$GarageAge<0),] <- 0
# check to make sure there are no more negative values
summary(total)



# get categorical variables and create dummy variables
cat_vars <- names(feature_classes[feature_classes=="character"])
dummies <- dummyVars(~.,total[cat_vars])
cat_encode <- predict(dummies,total[cat_vars])
cat_encode[is.na(cat_encode)] <- 0






# transform those with high skewness using log
for(x in names(skewed_vars)) {
    total[[x]] <- log(total[[x]]+1)
}



# create vector of numeric variables to include the new created features
feature_classes <- sapply(names(total),function(x){class(total[[x]])})
numeric_vars <- names(feature_classes[feature_classes!="character"])
numeric_vars <- numeric_vars[-c(28,29)]

# combine processed data together
total <- cbind(total[numeric_vars],cat_encode)



# split into training and test
x_train <- total[1:nrow(train),]
x_test <- total[(nrow(train)+1):nrow(total),]
y <- train$SalePrice



















## Below here is first procedure for processing ##

# Convert datasets into data.tables for manipulation
train <- as.data.table(train)
test <- as.data.table(test)

# Combine train and test sets to perform factor engineering
SalePrice <- train$SalePrice # pull out label to be cbind back later
train[,SalePrice:=NULL]
combi <- rbind(train,test)

### TRAINING SET PREPROCESSING ###
# Apply new factor level 'None' for variables that have meaningful NA levels so we don't impute/delete them
ok_na <- c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", 
           "BsmtFinType2", "GarageType", "GarageFinish", "GarageQual", 
           "GarageCond", "PoolQC", "Fence", "MiscFeature")

addNoAnswer <- function(x){
    if(is.factor(x)) return(factor(x, levels=c(levels(x), "None")))
    return(x)
}

combi[,(ok_na):=lapply(.SD,addNoAnswer),.SDcols=ok_na]

changeNA <- function (x) {
    x[is.na(x)] <- "None"
    return(x)
}

combi[,(ok_na):=lapply(.SD,changeNA),.SDcols=ok_na]

# change some int var to factors and separate categorical and numerical variables to individual data frames
change_var <- c("OverallQual","OverallCond","FullBath","HalfBath","BedroomAbvGr","TotRmsAbvGrd","Fireplaces","GarageCars","MoSold","YrSold")
combi[,(change_var):=lapply(.SD,as.factor),.SDcols=change_var]


### REMOVE NEAR ZERO VARIANCE / CONSTANT VARIABLES ###
removeVar <- c("Street","Alley","LandContour","Utilities","LandSlope","Condition2","RoofMatl","BsmtCond","BsmtFinType2","BsmtFinSF2",
               "Heating","LowQualFinSF","KitchenAbvGr","Functional","OpenPorchSF","EnclosedPorch","X3SsnPorch","ScreenPorch","PoolArea","PoolQC",
               "MiscFeature","MiscVal")
combi[,(removeVar):=NULL,with=FALSE]

# create new features
combi$YearMo=as.numeric(combi$YrSold)*12+as.numeric(combi$MoSold)

# impute using mice
imp <- mice(combi, m=3, method='cart', seed=123)
train <- complete(imp.train)
train_complete <- train

# impute the dataset using missForest
imp <- missForest(combi,parallelize='forests')
combi_imp <- imp$ximp

# split out the train and test data since we are done performing factoring
train <- combi_imp[1:1460,]
train <- cbind(train,SalePrice)
test <- combi_imp[1461:2919,]
train_complete <- combi_imp[1:1460,]
train_complete <- cbind(train_complete,SalePrice)

# Determine categorical and continous variables
cat_var <- names(train)[which(sapply(train, is.factor))]
num_var <- names(train)[which(sapply(train, is.numeric))]

# Split data frame into separate continuous and categorical data frames for analysis
# train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]
train_cat <- train[,.SD,.SDcols = cat_var]
train_num <- train[,.SD,.SDcols = num_var]



# impute missing values using mice package
imp.train <- mice(train, m=3, method='cart', seed=123)
train <- complete(imp.train)
train_complete <- train

### TEST SET PREPROCESSING ###
# test[,(ok_na):=lapply(.SD,addNoAnswer),.SDcols=ok_na]
# test[,(ok_na):=lapply(.SD,changeNA),.SDcols=ok_na]
# 
# cat_var2 <- names(test)[which(sapply(test, is.factor))]
# num_var2 <- names(test)[which(sapply(test, is.numeric))]
# 
# test[,(change_var):=lapply(.SD,as.factor),.SDcols=change_var]
# 
# test[,(cat_var2) := lapply(.SD, as.factor), .SDcols = cat_var2]
# 
# # remove near zero variance variables
# test[,(removeVar):=NULL,with=FALSE]

# imp.test <- mice(test, m=3, method='cart', seed=123)
# test <- complete(imp.test)


# # make factor levels equal between train and test 
# # levels(test$Utilities) <- levels(train$Utilities)
# # levels(test$Condition2) <- levels(train$Condition2)
# levels(test$HouseStyle) <- levels(train$HouseStyle)
# levels(test$RoofMatl) <- levels(train$RoofMatl)
# levels(test$Exterior1st) <- levels(train$Exterior1st)
# levels(test$Exterior2nd) <- levels(train$Exterior2nd)
# # levels(test$Heating) <- levels(train$Heating)
# levels(test$Electrical) <- levels(train$Electrical)
# levels(test$GarageQual) <- levels(train$GarageQual)
# levels(train$FullBath) <- levels(test$FullBath)
# levels(test$BedroomAbvGr) <- levels(train$BedroomAbvGr)
# levels(train$Fireplaces) <- levels(test$Fireplaces)
# levels(train$GarageCars) <- levels(test$GarageCars)
# # levels(test$PoolQC) <- levels(train$PoolQC)
# # levels(test$MiscFeature) <- levels(train$MiscFeature)


### DATA SPLITTING ###
# n <- dim(train)[1]
# set.seed(123)
# valid.sample <- sample(n,round(n/4))
# valid <- train[valid.sample,]
# train <- train[-valid.sample,]
