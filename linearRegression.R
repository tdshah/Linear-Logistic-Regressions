# Sources: glmnet, lm, plot, boxcox
# https://www.rdocumentation.org/packages/glmnet/versions/2.0-12/topics/glmnet 
# https://www.rdocumentation.org/packages/stats/versions/3.4.3/topics/lm 
# https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/plot 
# https://www.rdocumentation.org/packages/EnvStats/versions/2.3.0/topics/boxcox 
library(glmnet)
library(MASS)

# Read in Data
setwd('~/Documents/UIUC/CS_498_AML/HW_6')
wdat <- read.csv('default_plus_chromatic_features_1059_tracks.csv', header=FALSE)

features <- as.matrix(data.frame(wdat[,-c(ncol(wdat)-1, ncol(wdat))]))
latitude <- as.numeric(wdat[, ncol(wdat)-1])
longitude <- as.numeric(wdat[, ncol(wdat)])
N <- dim(wdat)[1]


# Simple Linear Regression
lat_lr <- lm(latitude ~ features)
summary(lat_lr)$r.squared
plot(lat_lr$fitted.values, lat_lr$residuals, main = 'Plot of Fitted Latitude Values Against Residual', 
     xlab = 'Fitted Latitude Values', ylab = 'Residual')
sum(lat_lr$residuals^2) / N

long_lr <- lm(longitude ~ features)
summary(long_lr)$r.squared
plot(long_lr$fitted.values, long_lr$residuals, main = 'Plot of Fitted Longitude Values Against Residual', 
     xlab = 'Fitted Longitude Values', ylab = 'Residual')
sum(long_lr$residuals^2) / N


# Box-Cox Transformation
lat_transf <- latitude - min(latitude) + 1
lat_bc <- boxcox(lm(lat_transf ~ features))
lat_bc_lambda <- lat_bc$x[which.max(lat_bc$y)]
lat_bc <- lm(lat_transf^lat_bc_lambda ~ features)
summary(lat_bc)$r.squared
plot(lat_bc$fitted.values, lat_bc$residuals, main = 'Box-Cox Transformed Latitude Values Against Residual', 
     xlab = 'Fitted Box-Cox Transformed Latitude Values', ylab = 'Residual')
sum(lat_bc$residuals^2) / N

long_transf <- longitude - min(longitude) + 1
long_bc <- boxcox(lm(long_transf ~ features))
long_bc_lambda <- long_bc$x[which.max(long_bc$y)]
long_bc <- lm(long_transf^long_bc_lambda ~ features)
summary(long_bc)$r.squared
plot(long_bc$fitted.values, long_bc$residuals, main = 'Box-Cox Transformed Longitude Values Against Residual', 
     xlab = 'Fitted Box-Cox Transformed Longitude Values', ylab = 'Residual')
sum(long_bc$residuals^2) / N


# Cross Validation for unregularized MSE
lat_cv <- cv.glmnet(features, latitude, alpha=0, lambda = c(1e-9,1e-8))
min(lat_cv$cvm)
long_cv <- cv.glmnet(features, longitude, alpha=0, lambda = c(1e-9,1e-8))
min(long_cv$cvm)


# Ridge Regression
lat_ridge <- cv.glmnet(features, latitude, alpha = 0)
min(lat_ridge$cvm)
lat_ridge$lambda.min
lat_ridge$nzero[match(lat_ridge$lambda.min, lat_ridge$lambda)]
plot(lat_ridge)

long_ridge <- cv.glmnet(features, longitude, alpha = 0)
min(long_ridge$cvm)
long_ridge$lambda.min
long_ridge$nzero[match(long_ridge$lambda.min, long_ridge$lambda)]
plot(long_ridge)

# Lasso Regression
lat_lasso <- cv.glmnet(features, latitude, alpha = 1)
min(lat_lasso$cvm)
lat_lasso$lambda.min
lat_lasso$nzero[match(lat_lasso$lambda.min, lat_lasso$lambda)]
plot(lat_lasso)

long_lasso <- cv.glmnet(features, longitude, alpha = 1)
min(long_lasso$cvm)
long_lasso$lambda.min
long_lasso$nzero[match(long_lasso$lambda.min, long_lasso$lambda)]
plot(long_lasso)

# Elastic Net Regression (alpha = 0.25, 0.50, and 0.75)
lat_elastic_25 <- cv.glmnet(features, latitude, alpha = 0.25)
min(lat_elastic_25$cvm)
lat_elastic_25$lambda.min
lat_elastic_25$nzero[match(lat_elastic_25$lambda.min, lat_elastic_25$lambda)]
plot(lat_elastic_25)

lat_elastic_50 <- cv.glmnet(features, latitude, alpha = 0.50)
min(lat_elastic_50$cvm)
lat_elastic_50$lambda.min
lat_elastic_50$nzero[match(lat_elastic_50$lambda.min, lat_elastic_50$lambda)]
plot(lat_elastic_50)

lat_elastic_75 <- cv.glmnet(features, latitude, alpha = 0.75)
min(lat_elastic_75$cvm)
lat_elastic_75$lambda.min
lat_elastic_75$nzero[match(lat_elastic_75$lambda.min, lat_elastic_75$lambda)]
plot(lat_elastic_75)

long_elastic_25 <- cv.glmnet(features, longitude, alpha = 0.25)
min(long_elastic_25$cvm)
long_elastic_25$lambda.min
long_elastic_25$nzero[match(long_elastic_25$lambda.min, long_elastic_25$lambda)]
plot(long_elastic_25)

long_elastic_50 <- cv.glmnet(features, longitude, alpha = 0.50)
min(long_elastic_50$cvm)
long_elastic_50$lambda.min
long_elastic_50$nzero[match(long_elastic_50$lambda.min, long_elastic_50$lambda)]
plot(long_elastic_50)

long_elastic_75 <- cv.glmnet(features, longitude, alpha = 0.75)
min(long_elastic_75$cvm)
long_elastic_75$lambda.min
long_elastic_75$nzero[match(long_elastic_75$lambda.min, long_elastic_75$lambda)]
plot(long_elastic_75)
