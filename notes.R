# Data preprocessing
# 
# 1) save a copy of train for eda
# 2) combine train and test set
# 3) remove near zero variance predictors
# 4) log transform response variable
# 4.5) create new features
# 5) separate numeric and categorical columns and do transformations for skewness and dummy variables
# 6) impute missing values with mean
# 7) remove highly correlated variables?
# 8) split back into train and test
# 
# Model building 
# Lasso, Ridge, Random Forest, GBM, XGBoost, SVM, Neural Networks, Multiple Linear Regression with Stepwise selection

# To try
# another stacking model x
# h2o glm model x
# average h2o models x
# use factors instead of dummy variables?
# remove factors and create a single binary variable for some categorical variables
# trim outliers

# Variables to fix/examine/remove
# Street, Alley, Utilities, LotConfig, Condition2, RoofMatl, ExterCond, BsmtFinSF2, Heating, LowQualFinSF, BsmtHalfBath, EnclosedPorch, X3SsnPorch,
# PoolArea, PoolQC, MiscFeature, MiscVal, MoSold, YrSold, SaleType

# New variables?
# New home, AccStreet, SaleCondNorm, Tsldx, Fenc, Basement, RemodAge, Fireplace, Garage, GarageAge, HouseAge



## H2O package
# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("methods","statmod","stats","graphics","RCurl","jsonlite","tools","utils")
for (pkg in pkgs) {
    if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg, repos = "http://cran.rstudio.com/") }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-turing/10/R")))
library(h2o)
localH2O = h2o.init(nthreads=-1)

# Finally, let's run a demo to see H2O at work.
demo(h2o.kmeans)


