# Convert datasets into data.tables
train <- as.data.table(train)
test <- as.data.table(test)

# Determine categorical and continous variables
change_var <- c("OverallQual","OverallCond","FullBath","TotRmsAbvGrd","Fireplaces","GarageCars","MoSold")
train[,(change_var):=lapply(.SD,as.factor),.SDcols=change_var]
cat_var <- names(train)[which(sapply(train, is.factor))]
num_var <- names(train)[which(sapply(train, is.numeric))]

# Split data frame into separate continuous and categorical data frames for analysis
train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]
train_cat <- train[,.SD,.SDcols = cat_var]
train_num <- train[,.SD,.SDcols = num_var]
