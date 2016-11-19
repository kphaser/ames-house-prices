##### PREDICT 413 Final Project Code #####

### Load the data ###

library(ggplot2)
library(dplyr)
library(data.table)
library(caret)
library(VIM)
library(mice)
library(rpart)

train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")


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

# change some int var to factors and separate categorical and numerical variables to individual data frames
change_var <- c("OverallQual","OverallCond","FullBath","TotRmsAbvGrd","Fireplaces","GarageCars","MoSold")
train[,(change_var):=lapply(.SD,as.factor),.SDcols=change_var]
cat_var <- names(train)[which(sapply(train, is.factor))]
num_var <- names(train)[which(sapply(train, is.numeric))]
train[,(cat_var) := lapply(.SD, as.factor), .SDcols = cat_var]
train_cat <- train[,.SD,.SDcols = cat_var]
train_num <- train[,.SD,.SDcols = num_var]

# apply new factor levels for variables that have meaningful NA levels so we don't impute/delete them
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

head(train$Alley)
head(train$BsmtQual)
class(train$Alley)



# initial plots
plot(train$SalePrice)
hist(train$SalePrice)
qqnorm(train$SalePrice)
qqline(train$SalePrice)
hist(log(train$SalePrice+1)) # normalize response variable

par(mfrow=c(1,2))
boxplot(train$SalePrice,col="lightblue")
boxplot(log(train$SalePrice+1),col="red")
par(mfrow=c(1,1))


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

cor(train_num,use="complete.obs")


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


# # variable importance with boruta
# library(boruta)
# ID.VAR <- "Id"
# TARGET.VAR <- "SalePrice"
# candidate.features <- setdiff(names(train_complete),c(ID.VAR,TARGET.VAR))
# data.type <- sapply(candidate.features,function(x){class(train_complete[[x]])})
# table(data.type)
# # pull out the response variable
# response <- train_complete$SalePrice
# # remove identifier and response variables
# train_complete <- train_complete[candidate.features]
# 
# set.seed(123)
# bor.results <- Boruta(train_complete,response,
#                       maxRuns=101,
#                       doTrace=0)
# bor.results
# plot(bor.results)

### Data Prep ###


# impute missing values using mean/median, random forest, or mice package?

md.pattern(train_num)
aggr_plot <- aggr(train_num, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

imp.train <- mice(train, method='cart', maxit=3, seed=123)
train_complete <- complete(imp.train)


# split training data into training and validation set
n <- dim(train)[1]
set.seed(123)
valid.sample <- sample(n,round(n/4))
valid <- train[valid.sample,]
train <- train[-valid.sample,]
train_control <- trainControl(method="cv", number=5)


### Build Models ###

# Multiple linear regression
lmfit <- lm(SalePrice~.-Id,data=train)
summary(lmfit)
pred <- predict(lmfit,newdata=valid)
rmse(log(valid$SalePrice),log(pred))

# Ridge regression
model <- train(SalePrice~.-Id, data=train, trControl=train_control, method="rf")
print(model)


# Lasso regression



# Random forest
rfmod <- randomForest(SalePrice~.-Id,data=train)
pred <- predict(rfmod,newdata=valid)
rmse(log(valid$SalePrice),log(pred))

model <- train(SalePrice~.-Id, data=train, trControl=train_control, method="rf")
print(model)



# Support vector regression
library(e1071)
svmfit <- svm(PRCP~.-month,r.train)
svmpred <- predict(svmfit,r.test)
# perform a grid search
tuneResult <- tune(svm, PRCP ~ .-month,  data = r.train,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
print(tuneResult)
# Draw the tuning graph
plot(tuneResult)
tunedModel <- tuneResult$best.model
tunedSVM <- predict(tunedModel, r.test) 

sqrt(mean((tunedSVM-r.test$PRCP)^2))


# GBM/XGBoost
fitControl <- trainControl(method = "cv", number = 5)

set.seed(123)
gbmFit1 <- train(SalePrice ~ . -Id, data = train, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)
gbmFit1
pred <- predict(gbmFit1,newdata=valid)
rmse(log(valid$SalePrice),log(pred))



# GAM



# Artificial neural network




### Evaluate and Validate Models ###

# check residuals
# compare validation metric




### Model Selection ###




### Submission File ###

# random forest no parameter adjustments
rfsubmit <- randomForest(SalePrice~.-Id,data=train_complete)
pred <- predict(rfsubmit,newdata=test)
results <- data.frame(Id=test$Id,SalePrice=pred)
write.csv(results,'randomforest1.csv',row.names=FALSE)

# gbm w/ n.trees=150,interaction.depth=3,shrinkage=0.1,n.minobsinnode=10
gbmsubmit <- gbm(SalePrice~.-Id,data=train_complete,n.trees=150,interaction.depth=3,shrinkage=0.1,n.minobsinnode=10,verbose=FALSE)
pred <- predict(gbmsubmit,newdata=test,n.trees=150,type="response")
results <- data.frame(Id=test$Id,SalePrice=pred)
write.csv(results,'gbm1.csv',row.names=FALSE)
