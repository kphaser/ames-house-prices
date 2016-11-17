# Convert datasets into data.tables
train <- as.data.table(train)
test <- as.data.table(test)

# Determine categorical and continous variables
cat_col <- names(train)[which(sapply(train, is.factor))]
cat_var <- c(cat_col, 'BedroomAbvGr', 'HalfBath', 'KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')
num_var <- names(train)[which(sapply(train, is.numeric))]

# Split data frame into separate continuous and categorical data frames for analysis
train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]
train_cat <- train[,.SD,.SDcols = cat_var]
train_cont <- train[,.SD,.SDcols = num_var]
