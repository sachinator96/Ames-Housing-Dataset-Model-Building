# Author - Sachin Santhosh
# Date - Spetember 2019



# Start by cleaning up memory of current R session:
rm(list=ls(all=TRUE))

# Setting global option decimal places to 4
options(digits = 4)

# Call all required library packages:
install.packages("data.table")


library(data.table)   
library(plotly)
library(ggplot2)




# ------------------------------------------------------------ #
# ----------------------- Load Files ------------------------- #
# ------------------------------------------------------------ #
train = data.frame(fread("newtrain.csv"), stringsAsFactors = FALSE)
hpte = data.frame(fread("newtest.csv"), stringsAsFactors = FALSE)



# ------------------------------------------------------------ #
# ------------ Creating a test and train set: ---------------- #
# ------------------------------------------------------------ #
set.seed(270)
seq_index = sample(seq_len(nrow(train)), size = 300)

hptr = train[-seq_index,]
validf = train[seq_index,]
rm(seq_index)

#Therefore hptr = Training set, validf = Validation set, hpte = Test set



# -------------------------------------------------------- #
# -------------------------------------------------------- #
# Section 3 - Apply ML Algorithms
# -------------------------------------------------------- #
# -------------------------------------------------------- #

# Model 1 - LINEAR REGRESSION MODEL
lmodel <- lm(SalePrice ~ MSSubClass + LotArea + LandContour + 
               Utilities + LotConfig + Neighborhood + BldgType + 
               HouseStyle + OverallQual + OverallCond + YearBuilt +  #Exterior1st +
                MasVnrType + Foundation + BsmtCond + 
               BsmtFinType1 + BsmtFinSF1 + TotalBsmtSF + Heating + 
               X1stFlrSF + X2ndFlrSF + FullBath + HalfBath + KitchenQual + 
               GarageCars + GarageArea + GarageCond + SaleType + 
               SaleCondition, data = hptr)
summary(lmodel)

# Validation of model and error rates
#Creating Model with prediction on Validation Set
fitmodel = data.frame(predict(lmodel, validf, interval = "prediction"))
finalvalid = data.frame(Id = validf$Id, PredictedSalePrice = fitmodel$fit, oldprice = validf$SalePrice) #Creating new Df with Id, predicted price and Old Price
finalvalid$err_rate = sqrt(abs((finalvalid$PredictedSalePrice^2) - (finalvalid$oldprice^2)))
finalvalid$Percenterr = abs((finalvalid$oldprice - finalvalid$PredictedSalePrice )/finalvalid$oldprice)*100

median(finalvalid$err_rate)  # ->Median prediction error in $ = 62085$
median(finalvalid$pcterr)    # ->Median prediction error as % difference from correct value = 7.861 %

validtestdf = data.frame(Id = finalvalid$Id, trueval = finalvalid$oldprice  ,
                         linval = finalvalid$PredictedSalePrice)

# Predict values for test set 
fitmodel = data.frame(predict(lmodel, hpte, interval = "prediction"))
final_test_predictions = data.frame(Id = hpte$Id, LMPredictedSalePrice = fitmodel$fit)



write.csv(final, "model1_linreg.csv", row.names = FALSE)








# Model 2 - NEURAL NETWORK MODEL
library("nnet")

fitnnmodel = nnet(SalePrice ~ OverallQual + OverallCond + YearBuilt + YearRemodAdd +
                    ExterQual + BsmtFinSF1 + TotalBsmtSF + X1stFlrSF + 
                    X2ndFlrSF + GrLivArea + TotRmsAbvGrd + CentralAir +
                    LotArea + BsmtUnfSF + FullBath ,
                  data = hptr, size = 3, skip = TRUE, linout = TRUE)

# Predict for validation dataset:
prednn <- predict(fitnnmodel, validf)
xdf = data.frame(Id= validf$Id, PredictedSalePrice = prednn, oldprice = validf$SalePrice)
xdf$err_rate = sqrt(abs((xdf$PredictedSalePrice^2) - (xdf$oldprice^2)))
xdf$pcterr = abs((xdf$oldprice - xdf$PredictedSalePrice )/xdf$oldprice)*100

median(xdf$err_rate)  # Median prediction error in $ = 69430$
median(xdf$pcterr)    # Median prediction error as % difference from correct value = 8.34%
# Clearly this model is not better than the Linear Regression Model.

validtestdf$nnetval = xdf$PredictedSalePrice

prednn <- predict(fitnnmodel, hpte)
xdf = data.frame(Id= hpte$Id, SalePrice = prednn)

final_test_predictions$NnetwPredictedSalePrice=xdf$SalePrice

write.csv(xdf, "model2_nnet.csv", row.names = FALSE)






# Model 4 - RANDOM FOREST MODEL
library("randomForest")
set.seed(280)


# Converting character values to factors

hptr$ExterQual = as.factor(hptr$ExterQual)
validf$ExterQual = as.factor(validf$ExterQual)
hpte$ExterQual = as.factor(hpte$ExterQual)

hptr$CentralAir = as.factor(hptr$CentralAir)
validf$CentralAir = as.factor(validf$CentralAir)
hpte$CentralAir = as.factor(hpte$CentralAir)

hptr$Neighborhood = factor(hptr$Neighborhood, levels = c("Blmngtn", "Blueste", "BrDale", "BrkSide", "ClearCr", "CollgCr", "Crawfor", "Edwards", "Gilbert", "IDOTRR", "NWAmes", "OldTown", "Sawyer", "SawyerW", "Somerst", "StoneBr", "SWISU", "Timber", "Veenker"))
validf$Neighborhood = factor(validf$Neighborhood, levels = c("Blmngtn", "Blueste", "BrDale", "BrkSide", "ClearCr", "CollgCr", "Crawfor", "Edwards", "Gilbert", "IDOTRR", "NWAmes", "OldTown", "Sawyer", "SawyerW", "Somerst", "StoneBr", "SWISU", "Timber", "Veenker"))
hpte$Neighborhood = factor(hpte$Neighborhood, levels = c("Blmngtn", "Blueste", "BrDale", "BrkSide", "ClearCr", "CollgCr", "Crawfor", "Edwards", "Gilbert", "IDOTRR", "NWAmes", "OldTown", "Sawyer", "SawyerW", "Somerst", "StoneBr", "SWISU", "Timber", "Veenker"))

set.seed(370)
fitrfmodel = randomForest(SalePrice ~ OverallQual +  OverallCond + YearBuilt + 
                            YearRemodAdd + ExterQual + BsmtFinSF1 + TotalBsmtSF 
                          + X1stFlrSF + X2ndFlrSF + GrLivArea + TotRmsAbvGrd 
                          + CentralAir + LotArea + BsmtUnfSF + FullBath ,
                          data = hptr, 
                          ntree = 700, 
                          importance = TRUE)
summary(fitrfmodel)
plot(fitrfmodel)


# Validate model:
predrf = predict(fitrfmodel, validf)
ydf = data.frame(Id = validf$Id, PredictedSalePrice = predrf, oldprice = validf$SalePrice)
ydf$err_rate = sqrt(abs((ydf$PredictedSalePrice^2) - (ydf$oldprice^2)))
ydf$pcterr = abs((ydf$oldprice - ydf$PredictedSalePrice )/ydf$oldprice)*100

median(ydf$err_rate)  # Median prediction error in $ = 59495$
median(ydf$pcterr)    # Median prediction error as % difference from correct value = 6.959%
# Clearly this model is only slightly better than the linear regression model.

validtestdf$RFval = ydf$PredictedSalePrice

predrf = predict(fitrfmodel, hpte)
predmodel = data.frame(Id = hpte$Id, PredictedSalePrice = predrf)

write.csv(predmodel, "model3_rf.csv", row.names = FALSE)

final_test_predictions$RFval = predmodel$PredictedSalePrice




# Model 4 - GLM MODEL:

train = data.frame(fread("newtrain.csv"), stringsAsFactors = FALSE)
hpte = data.frame(fread("newtest.csv"), stringsAsFactors = FALSE)

set.seed(270)
seq_index = sample(seq_len(nrow(train)), size = 300)

hptr = train[-seq_index,]
validf = train[seq_index,]
rm(seq_index)

set.seed(280)

hptr$GarageCond = as.factor(hptr$GarageCond)
hpte$GarageCond = as.factor(hpte$GarageCond)
validf$GarageCond = as.factor(validf$GarageCond)

hptr$SaleType = as.factor(hptr$SaleType)
hpte$SaleType = as.factor(hpte$SaleType)
validf$SaleType = as.factor(validf$SaleType)

glmlmodel <- glm(SalePrice ~ MSSubClass + LotArea + LandContour + 
                   Utilities + LotConfig + Neighborhood + BldgType + 
                   HouseStyle + OverallQual + OverallCond + YearBuilt + #Exterior1st +
                    MasVnrType + BsmtCond + 
                   BsmtFinSF1 + TotalBsmtSF + Heating + 
                   X1stFlrSF + X2ndFlrSF + FullBath + HalfBath + KitchenQual + 
                   GarageCars + GarageArea + GarageCond + SaleType + 
                   SaleCondition, 
                 data = hptr)
                 
summary(glmlmodel)

# Validation checks:
predglm = predict(glmlmodel, validf)
zdf = data.frame(Id = validf$Id, PredictedSalePrice = predglm, oldprice = validf$SalePrice)
zdf$err_rate = sqrt(abs((zdf$PredictedSalePrice^2) - (zdf$oldprice^2)))
zdf$pcterr = abs((zdf$oldprice - zdf$PredictedSalePrice )/zdf$oldprice)*100

median(zdf$err_rate)  # Median prediction error in $ = 61554$
median(zdf$pcterr)    # Median prediction error as % difference from correct value = 6.999%
# This model is slightly better than the regression model ( challenger model) , 
# but only because we reduced some of the variables compared to the linear model.

validtestdf$GLMval = zdf$PredictedSalePrice

predglm = predict(glmlmodel, hpte)
predgmodel = data.frame(Id = hpte$Id, PredictedSalePrice = predglm)



write.csv(predgmodel, "model4_glm.csv", row.names = FALSE)


final_test_predictions$GLMval = predgmodel$PredictedSalePrice


# Calculating Mean of all predicted sale prices for each model on Validation Set:
validtestdf$meansval = rowMeans(validtestdf[, 3:6])
validtestdf$err_rate = sqrt(abs((validtestdf$trueval^2) - (validtestdf$meansval^2)))
validtestdf$pcterr = abs((validtestdf$trueval - validtestdf$meansval )/validtestdf$trueval)*100
median(validtestdf$err_rate)  # Median prediction error in $ = 60101$
median(validtestdf$pcterr)    # Median prediction error as % difference from correct value = 6.35%

# Calculating Mean of all predicted sale prices for each model on Test Set:

final_test_predictions$meanval = rowMeans(final_test_predictions[, 2:5])








