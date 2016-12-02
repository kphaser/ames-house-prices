################# SETUP ###################

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
library(mice)
library(plyr)
library(Amelia)
library(psych)
library(Metrics)

# train data
train <- read.csv("data/train.csv",stringsAsFactors=TRUE)
train_eda <- train # save a copy for eda purposes
str(train)
a <- rep(0,length(ncol(train)))
for(i in 1:ncol(train)){a[i]=sum(is.na(train[,i]))}
a
train <- train[,c(-4,-7,-58,-73,-74,-75)]
str(train)

# test data
test <- read.csv("data/test.csv",stringsAsFactors=TRUE)
str(test) 
test$SalePrice=as.integer(rep("",length(test$Id)))
test <- test[,c(-4,-7,-58,-73,-74,-75)]


# combine train and test
total <- rbind(train,test)
a <- rep(0,length(ncol(total)))
for(i in 1:ncol(total)){a[i]=sum(is.na(total[,i]))} # check NAs
a 

################### EDA AND DATA PREPROCESSING #####################

# typical eda
describe(total) 
hist(total$SalePrice) # need to transform this to be normal

# remove near zero variance features or constant features
nearZeroVar(train,saveMetrics = TRUE)
removeVar <- c("Street","LandContour","Utilities","LandSlope","Condition2","RoofMatl","BsmtCond","BsmtFinType2","BsmtFinSF2",
               "Heating","LowQualFinSF","KitchenAbvGr","Functional","GarageQual","GarageCond","EnclosedPorch","X3SsnPorch",
               "ScreenPorch","PoolArea","MiscVal")
total <- total[,!(names(total) %in% removeVar)]

# change some int columns with 5 levels or less to factors
total$MoSold <- as.factor(total$MoSold)
total$YrSold <- as.factor(total$YrSold)
total$HalfBath <- as.factor(total$HalfBath)
total$FullBath <- as.factor(total$FullBath)
total$GarageCars <- as.factor(total$GarageCars)
total$BsmtFullBath <- as.factor(total$BsmtFullBath)
total$BsmtHalfBath <- as.factor(total$BsmtHalfBath)
total$Fireplaces <- as.factor(total$Fireplaces)

# plot of SalePrice by Neighborhood
train %>% select(Neighborhood, SalePrice) %>% ggplot(aes(factor(Neighborhood), SalePrice)) + geom_boxplot() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Neighborhoods')
# plot of SalePrice by FullBath
train %>% select(FullBath, SalePrice) %>% ggplot(aes(factor(FullBath), SalePrice)) + geom_boxplot() + xlab('FullBath')
# plot of SalePrice by GarageFinish
train %>% select(GarageFinish, SalePrice) %>% ggplot(aes(factor(GarageFinish), SalePrice)) + geom_boxplot() + xlab('GarageFinish')
# plot of SalePrice by CentralAir
train %>% select(CentralAir, SalePrice) %>% ggplot(aes(factor(CentralAir), SalePrice)) + geom_boxplot() + xlab('CentralAir')

# save SalePrice separately and remove it and Id from combined dataset
y_train <- train$SalePrice # save a copy of SalePrice for later
total$SalePrice <- NULL
total$Id <- NULL

# get numeric variables and determine the variables that have high skew 
feature_classes <- sapply(names(total),function(x){class(total[[x]])})
numeric_vars <- names(feature_classes[feature_classes!="factor"])
skewed_vars <- sapply(numeric_vars,function(x){skewness(total[[x]],na.rm=TRUE)})
skewed_vars <- skewed_vars[skewed_vars>0.75]


# impute missing values using mice package
missmap(total, main="Missing Map")
imp <- mice(total,m=3,method="cart",maxit=1)
total <- complete(imp)
missmap(total)
anyNA(total) # no more NAs

# plot correlations
numericdf <- total[,numeric_vars]
save(numericdf,file="numericdf.RData") # save for loading into Rmd file
ames_cor <- cor(numericdf)
col <- colorRampPalette(c("#BB4444","#EE9988","#FFFFFF","#77AADD","#4477AA"))
corrplot(ames_cor,method="shade",shade.col=NA,tl.col="black",tl.srt=45,col=col(200),addCoef.col="black",order="AOE")

# log transformation of response
y_train <- log(y_train+1)

# Create new features
# total$NewHome <- ifelse(total$YearBuilt>=2000,1,0)
# total$Basement <- ifelse(total$TotalBsmtSF>0,1,0)
# total$RemodelAge <- total$YrSold-total$YearRemodAdd
# total$Fireplace <- ifelse(total$Fireplaces>0,1,0)
# total$Garage <- ifelse(total$GarageCars>0,1,0)
# total$GarageAge <- total$YrSold-total$GarageYrBlt
# total$HouseAge <- total$YrSold-total$YearBuilt
# 
# summary(total)


# get categorical variables and create dummy variables
cat_vars <- names(feature_classes[feature_classes=="factor"])
dummies <- dummyVars(~.,total[cat_vars])
cat_encode <- predict(dummies,total[cat_vars])
cat_encode[is.na(cat_encode)] <- 0


# transform those with high skewness using log
for(x in names(skewed_vars)) {
    total[[x]] <- log(total[[x]]+1)
}


# combine processed data together
total <- cbind(total[numeric_vars],cat_encode)


# split into training and test
x_train <- total[1:nrow(train),]
x_test <- total[(nrow(train)+1):nrow(total),]

# save RData file for use in presentation and markdown later
save.image("final.RData")

################### MODEL DEVELOPMENT #############################

########### CARET MODELS #############
# control parameters
fitControl <- trainControl(method="repeatedcv",
                           number=5,
                           repeats=5,
                           verboseIter=FALSE)

# lasso
set.seed(123)  # for reproducibility
lasso_mod <- train(x=train,y=y_train,
                   method="glmnet",
                   metric="RMSE",
                   maximize=FALSE,
                   trControl=fitControl,
                   tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                        lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                 0.00075,0.0005,0.0001)))
lasso_mod

preds <- exp(predict(lasso_mod,newdata=x_test)) - 1
solution <- data.frame(Id=as.integer(rownames(x_test)),SalePrice=preds)
write.csv(solution,"lasso.csv",row.names=FALSE)


######### H2O MODELS ############
library(h2o)
library(h2oEnsemble)
h2o.init(nthreads = -1)

data <- cbind(y_train,x_train)
colnames(data)[1] <- "SalePrice"
data <- as.h2o(data) # need to create h2o data frame to use h2o algorithms
test_h2o <- as.h2o(x_test)

# Partition the data to train and valid splits for parameter tuning
splits <- h2o.splitFrame(data = data, 
                         ratios = 0.75,  #partition data into 75%, 25%
                         seed = 1)  #setting a seed will guarantee reproducibility
train_h2o <- splits[[1]]
valid_h2o <- splits[[2]]

# Identify response and predictors
y <- "SalePrice"
x <- setdiff(names(data),y)
print(x)


## Models
# GLM
glm_fit1 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = train_h2o,
                    model_id = "glm_fit1",
                    family = "gaussian")
glm_fit2 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = train_h2o,
                    model_id = "glm_fit2",
                    validation_frame = valid_h2o,
                    family = "gaussian",
                    lambda_search = TRUE)
glm_fit3 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = train_h2o,
                    model_id = "glm_fit3",
                    validation_frame = valid_h2o,
                    family = "gaussian",
                    alpha = 1, # lasso penalty
                    lambda_search = TRUE)
glm_fit4 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = train_h2o,
                    model_id = "glm_fit4",
                    validation_frame = valid_h2o,
                    family = "gaussian",
                    alpha = 0, # ridge penalty
                    lambda_search = TRUE)


glm_perf1 <- h2o.performance(model = glm_fit1,
                             newdata = valid_h2o)
glm_perf2 <- h2o.performance(model = glm_fit2,
                             newdata = valid_h2o)
glm_perf3 <- h2o.performance(model = glm_fit3,
                             newdata = valid_h2o)
glm_perf4 <- h2o.performance(model = glm_fit4,
                             newdata = valid_h2o)
glm_perf1
glm_perf2
glm_perf3
glm_perf4

h2o.rmse(glm_perf1)
h2o.rmse(glm_perf2)
h2o.rmse(glm_perf3)
h2o.rmse(glm_perf4)

# Random Forest
rf_fit1 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train_h2o,
                            model_id = "rf_fit1",
                            seed = 1)

rf_fit2 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train_h2o,
                            model_id = "rf_fit2",
                            #validation_frame = valid_h2o,  #only used if stopping_rounds > 0
                            ntrees = 1000,
                            seed = 1)
rf_fit3 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train_h2o,
                            model_id = "rf_fit3",
                            validation_frame = valid_h2o,
                            stopping_rounds = 1,
                            stopping_metric = "MSE",
                            ntrees = 1000,
                            seed = 1)
rf_fit4 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train_h2o,
                            model_id = "rf_fit4",
                            validation_frame = valid_h2o,
                            # stopping_rounds = 1,
                            # stopping_metric = "MSE",
                            ntrees = 2000,
                            seed = 1)


rf_perf1 <- h2o.performance(model = rf_fit1,
                            newdata = valid_h2o)
rf_perf2 <- h2o.performance(model = rf_fit2,
                            newdata = valid_h2o)
rf_perf3 <- h2o.performance(model = rf_fit3,
                            newdata = valid_h2o)
rf_perf4 <- h2o.performance(model = rf_fit4,
                            newdata = valid_h2o)

rf_perf1
rf_perf2
rf_perf3
rf_perf4

h2o.rmse(rf_perf1)
h2o.rmse(rf_perf2)
h2o.rmse(rf_perf3)
h2o.rmse(rf_perf4)


# GBM
gbm_fit1 <- h2o.gbm(x = x,
                    y = y,
                    training_frame = train_h2o,
                    model_id = "gbm_fit1",
                    seed = 1)

gbm_fit2 <- h2o.gbm(x = x,
                    y = y,
                    training_frame = train_h2o,
                    model_id = "gbm_fit2",
                    #validation_frame = valid,  #only used if stopping_rounds > 0
                    ntrees = 500,
                    seed = 1)

gbm_fit3 <- h2o.gbm(x = x,
                    y = y,
                    training_frame = train_h2o,
                    model_id = "gbm_fit3",
                    validation_frame = valid_h2o,  #only used if stopping_rounds > 0
                    ntrees = 500,
                    score_tree_interval = 5,      #used for early stopping
                    stopping_rounds = 3,          #used for early stopping
                    stopping_metric = "AUTO",      #used for early stopping
                    stopping_tolerance = 0.0005,  #used for early stopping
                    seed = 1)

gbm_perf1 <- h2o.performance(model = gbm_fit1,
                             newdata = valid_h2o)
gbm_perf2 <- h2o.performance(model = gbm_fit2,
                             newdata = valid_h2o)
gbm_perf3 <- h2o.performance(model = gbm_fit3,
                             newdata = valid_h2o)

gbm_perf1
gbm_perf2
gbm_perf3

h2o.scoreHistory(gbm_fit2)
h2o.scoreHistory(gbm_fit3)

plot(gbm_fit3, 
     timestep = "number_of_trees", 
     metric = "rmse")

# Deep Learning Neural Networks
dl_fit1 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train_h2o,
                            model_id = "dl_fit1",
                            seed = 1)

dl_fit2 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train_h2o,
                            model_id = "dl_fit2",
                            #validation_frame = valid,  #only used if stopping_rounds > 0
                            epochs = 20,
                            hidden= c(10,10),
                            stopping_rounds = 0,  # disable early stopping
                            seed = 1)
dl_fit3 <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = train_h2o,
                            model_id = "dl_fit3",
                            validation_frame = valid_h2o,  #in DL, early stopping is on by default
                            epochs = 20,
                            hidden = c(10,10),
                            score_interval = 1,           #used for early stopping
                            stopping_rounds = 3,          #used for early stopping
                            stopping_metric = "AUTO",     #used for early stopping
                            stopping_tolerance = 0.0005,   #used for early stopping
                            seed = 1)

dl_perf1 <- h2o.performance(model = dl_fit1,
                            newdata = valid_h2o)
dl_perf2 <- h2o.performance(model = dl_fit2,
                            newdata = valid_h2o)
dl_perf3 <- h2o.performance(model = dl_fit3,
                            newdata = valid_h2o)

dl_perf1
dl_perf2
dl_perf3

h2o.scoreHistory(dl_fit3)

plot(dl_fit3, 
     timestep = "epochs", 
     metric = "rmse")

# Ensemble model
fit_a <- h2o.glm(x = x, 
                 y = y, 
                 training_frame = train_h2o,
                 model_id = "glm_fit3",
                 validation_frame = valid_h2o,
                 family = "gaussian",
                 alpha = 1, # lasso penalty
                 lambda_search = TRUE)
fit_b <- h2o.gbm(x = x,
                 y = y,
                 training_frame = train_h2o,
                 model_id = "gbm_fit3",
                 validation_frame = valid_h2o,  #only used if stopping_rounds > 0
                 ntrees = 500,
                 score_tree_interval = 5,      #used for early stopping
                 stopping_rounds = 3,          #used for early stopping
                 stopping_metric = "AUTO",      #used for early stopping
                 stopping_tolerance = 0.0005,  #used for early stopping
                 seed = 1)
pred1 <- predict(fit_a, valid_h2o)
pred2 <- predict(fit_b, valid_h2o)
totalpred <- (pred1+pred2)/2
predictions <- as.data.frame(totalpred)[,1]
labels <- as.data.frame(valid_h2o[,y])[,1]

rmse(predictions,labels)


########## MODEL SUBMISSIONS #############
# Random Forest (rf_fit2)
fit <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = data,
                            model_id = "rf_fit2",
                            #validation_frame = valid_h2o,  #only used if stopping_rounds > 0
                            ntrees = 1000,
                            seed = 1)
# GBM (gbm_fit3)
fit <- h2o.gbm(x = x,
                    y = y,
                    training_frame = data,
                    model_id = "gbm_fit3",
                    validation_frame = valid_h2o,  #only used if stopping_rounds > 0
                    ntrees = 500,
                    score_tree_interval = 5,      #used for early stopping
                    stopping_rounds = 3,          #used for early stopping
                    stopping_metric = "AUTO",      #used for early stopping
                    stopping_tolerance = 0.0005,  #used for early stopping
                    seed = 1)

# Neural Network (dl_fit1)
fit <- h2o.deeplearning(x = x,
                            y = y,
                            training_frame = data,
                            model_id = "dl_fit1",
                            seed = 1)

# Ensemble model
fit_a <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = data,
                    model_id = "glm_fit3",
                    validation_frame = valid_h2o,
                    family = "gaussian",
                    alpha = 1, # lasso penalty
                    lambda_search = TRUE)
fit_b <- h2o.gbm(x = x,
               y = y,
               training_frame = data,
               model_id = "gbm_fit3",
               validation_frame = valid_h2o,  #only used if stopping_rounds > 0
               ntrees = 500,
               score_tree_interval = 5,      #used for early stopping
               stopping_rounds = 3,          #used for early stopping
               stopping_metric = "AUTO",      #used for early stopping
               stopping_tolerance = 0.0005,  #used for early stopping
               seed = 1)
pred_a <- predict(fit_a,test_h2o)
pred_b <- predict(fit_b,test_h2o)
full_pred <- (pred_a+pred_b)/2
predictions <- as.data.frame(full_pred$pred)[,1]
results <- exp(predictions) - 1
solution <- data.frame(Id=as.integer(rownames(x_test)),SalePrice=results)
write.csv(solution,"2modelensemble.csv",row.names=FALSE)

### Submission file for individual models
pred <- predict(fit, test_h2o)
predictions <- as.data.frame(pred$pred)[,1]
results <- exp(predictions) - 1
solution <- data.frame(Id=as.integer(rownames(x_test)),SalePrice=results)
write.csv(solution,"nn.csv",row.names=FALSE)


# Average model with 12 strong performing models
fit1 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = data,
                    model_id = "glm_fit1",
                    family = "gaussian")
fit2 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = data,
                    model_id = "glm_fit2",
                    validation_frame = valid_h2o,
                    family = "gaussian",
                    lambda_search = TRUE)
fit3 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = data,
                    model_id = "glm_fit3",
                    validation_frame = valid_h2o,
                    family = "gaussian",
                    alpha = 1, # lasso penalty
                    lambda_search = TRUE)
fit4 <- h2o.glm(x = x, 
                    y = y, 
                    training_frame = data,
                    model_id = "glm_fit4",
                    validation_frame = valid_h2o,
                    family = "gaussian",
                    alpha = 0, # ridge penalty
                    lambda_search = TRUE)
fit5 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = data,
                            model_id = "rf_fit1",
                            seed = 1)

fit6 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = data,
                            model_id = "rf_fit2",
                            #validation_frame = valid_h2o,  #only used if stopping_rounds > 0
                            ntrees = 1000,
                            seed = 1)
fit7 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = data,
                            model_id = "rf_fit3",
                            validation_frame = valid_h2o,
                            stopping_rounds = 1,
                            stopping_metric = "MSE",
                            ntrees = 1000,
                            seed = 1)
fit8 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = data,
                            model_id = "rf_fit4",
                            validation_frame = valid_h2o,
                            # stopping_rounds = 1,
                            # stopping_metric = "MSE",
                            ntrees = 2000,
                            seed = 1)
fit9 <- h2o.gbm(x = x,
                    y = y,
                    training_frame = data,
                    model_id = "gbm_fit1",
                    seed = 1)

fit10 <- h2o.gbm(x = x,
                    y = y,
                    training_frame = data,
                    model_id = "gbm_fit2",
                    #validation_frame = valid,  #only used if stopping_rounds > 0
                    ntrees = 500,
                    seed = 1)

fit11 <- h2o.gbm(x = x,
                    y = y,
                    training_frame = data,
                    model_id = "gbm_fit3",
                    validation_frame = valid_h2o,  #only used if stopping_rounds > 0
                    ntrees = 500,
                    score_tree_interval = 5,      #used for early stopping
                    stopping_rounds = 3,          #used for early stopping
                    stopping_metric = "AUTO",      #used for early stopping
                    stopping_tolerance = 0.0005,  #used for early stopping
                    seed = 1)

# Ensemble Stacking with default wrappers
learner <- c ("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
              "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")
metalearner <- "h2o.glm.wrapper"
fit12 <- h2o.ensemble(x = x, y = y, 
                    training_frame = data, 
                    family = "gaussian", 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5))

# get all predictions and average equally
pred1 <- h2o.predict(fit1, test_h2o)
results1 <- exp(pred1) - 1
pred2 <- h2o.predict(fit2, test_h2o)
results2 <- exp(pred2) - 1
pred3 <- h2o.predict(fit3, test_h2o)
results3 <- exp(pred3) - 1
pred4 <- h2o.predict(fit4, test_h2o)
results4 <- exp(pred4) - 1
pred5 <- h2o.predict(fit5, test_h2o)
results5 <- exp(pred5) - 1
pred6 <- h2o.predict(fit6, test_h2o)
results6 <- exp(pred6) - 1
pred7 <- h2o.predict(fit7, test_h2o)
results7 <- exp(pred7) - 1
pred8 <- h2o.predict(fit8, test_h2o)
results8 <- exp(pred8) - 1
pred9 <- h2o.predict(fit9, test_h2o)
results9 <- exp(pred9) - 1
pred10 <- h2o.predict(fit10, test_h2o)
results10 <- exp(pred10) - 1
pred11 <- h2o.predict(fit11, test_h2o)
results11 <- exp(pred11) - 1

pred12 <- predict(fit12, test_h2o)
predictions12 <- as.data.frame(pred12$pred)[,1]
results12 <- exp(predictions12) - 1
results12 <- as.h2o(results12)
avgresults <- (results1+results2+results3+results4+results5+results6+results7+results8+results9+results10+results11+results12)/12
avgresults <- as.data.frame(avgresults)
solution <- data.frame(Id=as.integer(rownames(x_test)),SalePrice=avgresults)
colnames(solution)[2] <- "SalePrice"
write.csv(solution,"h2oavgmodelfinal.csv",row.names=FALSE)


# Shutdown H2O    
h2o.shutdown(prompt=FALSE)
