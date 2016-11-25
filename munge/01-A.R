# Setup environment to maximize parellel computing
memory.size(max=T)
library(doParallel)
library(foreach)
c1=makeCluster(2)
registerDoParallel(c1)


# Save copy of train data for initial EDA
train_eda <- train

# Combine train and test
total <- rbind(dplyr::select(train,MSSubClass:SaleCondition),
               dplyr::select(test,MSSubClass:SaleCondition))

# remove near zero variance features
removeVar <- c("Street","Alley","LandContour","Utilities","LandSlope","Condition2","RoofMatl","BsmtCond","BsmtFinType2","BsmtFinSF2",
               "Heating","LowQualFinSF","KitchenAbvGr","Functional","OpenPorchSF","EnclosedPorch","X3SsnPorch","ScreenPorch","PoolArea","PoolQC",
               "MiscFeature","MiscVal")
total <- total[,!(names(total) %in% removeVar)]


# log transformation of SalePrice
train$SalePrice <- log(train$SalePrice+1)

# create new features
total$YearMo <- as.numeric(total$YrSold)*12+as.numeric(total$MoSold)

# get numeric variables and transform those with high skewness using log
feature_classes <- sapply(names(total),function(x){class(total[[x]])})
numeric_vars <- names(feature_classes[feature_classes!="character"])
skewed_vars <- sapply(numeric_vars,function(x){skewness(total[[x]],na.rm=TRUE)})
skewed_vars <- skewed_vars[skewed_vars>0.75]
for(x in names(skewed_vars)) {
    total[[x]] <- log(total[[x]]+1)
}

# get categorical variables and create dummy variables
cat_vars <- names(feature_classes[feature_classes=="character"])
dummies <- dummyVars(~.,total[cat_vars])
cat_encode <- predict(dummies,total[cat_vars])
cat_encode[is.na(cat_encode)] <- 0

# impute missing values in numeric variables with mean
numeric_df <- total[numeric_vars]
for(x in numeric_vars){
    mean_value <- mean(train[[x]],na.rm=TRUE)
    total[[x]][is.na(total[[x]])] <- mean_value
}

# impute missing values in numeric variables with median
numeric_df <- total[numeric_vars]
for(x in numeric_vars){
    median_value <- median(train[[x]],na.rm=TRUE)
    total[[x]][is.na(total[[x]])] <- median_value
}


# combine processed data together
total <- cbind(total[numeric_vars],cat_encode)

# impute using mice
imp <- mice(total, m=1, method='cart', seed=123, maxit=1)
total <- complete(imp)


# split into training and test
x_train <- total[1:nrow(train),]
x_test <- total[(nrow(train)+1):nrow(total),]
y <- train$SalePrice

# create data frame for correlated variables for later
# corr_df <- data.frame(total$X1stFlrSF,total$X2ndFlrSF,total$LowQualFinSF,total$GrLivArea,total$TotFlrSF,total$AllLivArea)

# drop highly correlated variables
# dropvars <- c("X1stFlrSF","X2ndFlrSF","GrLivArea")
# total <- total[,!(names(total)%in%dropvars)]





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
