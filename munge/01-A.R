# Convert datasets into data.tables
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
change_var <- c("OverallQual","OverallCond","FullBath","HalfBath","BedroomAbvGr","TotRmsAbvGrd","Fireplaces","GarageCars","MoSold")
combi[,(change_var):=lapply(.SD,as.factor),.SDcols=change_var]



### REMOVE NEAR ZERO VARIANCE / CONSTANT VARIABLES ###
removeVar <- c("Street","Alley","LandContour","Utilities","LandSlope","Condition2","RoofMatl","BsmtCond","BsmtFinType2","BsmtFinSF2",
               "Heating","LowQualFinSF","KitchenAbvGr","Functional","EnclosedPorch","X3SsnPorch","ScreenPorch","PoolArea","PoolQC",
               "MiscFeature","MiscVal")
combi[,(removeVar):=NULL,with=FALSE]

# split out the train and test data since we are done performing factoring
train <- combi[1:1460,]
train <- cbind(train,SalePrice)
test <- combi[1461:2919,]


# Determine categorical and continous variables
cat_var <- names(train)[which(sapply(train, is.factor))]
num_var <- names(train)[which(sapply(train, is.numeric))]

# Split data frame into separate continuous and categorical data frames for analysis
# train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]
train_cat <- train[,.SD,.SDcols = cat_var]
train_num <- train[,.SD,.SDcols = num_var]



# impute missing values using mice package
imp.train <- mice(train, m=3, method='cart', maxit=1, seed=123)
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

imp.test <- mice(test, m=3, method='cart', maxit=1, seed=123)
test <- complete(imp.test)


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
n <- dim(train)[1]
set.seed(123)
valid.sample <- sample(n,round(n/4))
valid <- train[valid.sample,]
train <- train[-valid.sample,]
