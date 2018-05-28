# Sources: glmnet, lm, plot
# https://www.rdocumentation.org/packages/glmnet/versions/2.0-12/topics/glmnet 
# https://www.rdocumentation.org/packages/stats/versions/3.4.3/topics/lm 
# https://www.rdocumentation.org/packages/graphics/versions/3.4.3/topics/plot 
library(glmnet)

# Read in Data
setwd('~/Documents/UIUC/CS_498_AML/HW_6')
wdat <- read.csv('default_of_credit_card_clients.csv', skip = 1, header = TRUE)
features <- as.matrix(data.frame(wdat[, seq(from = 2, to = 24)]))
default <- as.numeric(wdat[, 25])

# Simple Unregularized Logistic Regression
log_lr <- cv.glmnet(features, default, family = "binomial", type.measure = "class", alpha = 0, lambda = c(1e-9,1e-8))
min(log_lr$cvm)
log_lr$nzero[match(log_lr$lambda.min, log_lr$lambda)]

# Ridge Regression
ridge <- cv.glmnet(features, default, family = "binomial", type.measure = "class", alpha = 0)
min(ridge$cvm)
ridge$lambda.min
ridge$nzero[match(ridge$lambda.min, ridge$lambda)]
plot(ridge)

# Lasso Regression
lasso <- cv.glmnet(features, default, family = "binomial", type.measure = "class", alpha = 1)
min(lasso$cvm)
lasso$lambda.min
lasso$nzero[match(lasso$lambda.min, lasso$lambda)]
plot(lasso)

# Elastic Net Regression
elastic_25 <- cv.glmnet(features, default, family = "binomial", type.measure = "class", alpha = 0.25)
min(elastic_25$cvm)
elastic_25$lambda.min
elastic_25$nzero[match(elastic_25$lambda.min, elastic_25$lambda)]
plot(elastic_25)

elastic_50 <- cv.glmnet(features, default, family = "binomial", type.measure = "class", alpha = 0.50)
min(elastic_50$cvm)
elastic_50$lambda.min
elastic_50$nzero[match(elastic_50$lambda.min, elastic_50$lambda)]
plot(elastic_50)

elastic_75 <- cv.glmnet(features, default, family = "binomial", type.measure = "class", alpha = 0.75)
min(elastic_75$cvm)
elastic_75$lambda.min
elastic_75$nzero[match(elastic_75$lambda.min, elastic_75$lambda)]
plot(elastic_75)
