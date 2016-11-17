# Convert datasets into data.tables
train <- as.data.table(train)
test <- as.data.table(test)

# Apply new factor levels for variables that have meaningful NA levels so we don't impute/delete them
ok_na <- c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "GarageType", "GarageFinish", "GarageQual", "GarageCond", "PoolQC", "Fence", "MiscFeature")

addNoAnswer <- function(x){
    if(is.factor(x)) return(factor(x, levels=c(levels(x), "None")))
    return(x)
}

train[,(ok_na):=lapply(.SD,addNoAnswer),.SDcols=ok_na]

changeNA <- function (x) {
    x[is.na(x)] <- "None"
    return(x)
}

train[,(ok_na):=lapply(.SD,changeNA),.SDcols=ok_na]

# Determine categorical and continous variables
cat_var <- names(train)[which(sapply(train, is.factor))]
num_var <- names(train)[which(sapply(train, is.numeric))]

# Split data frame into separate continuous and categorical data frames for analysis
train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]
train_cat <- train[,.SD,.SDcols = cat_var]
train_num <- train[,.SD,.SDcols = num_var]
