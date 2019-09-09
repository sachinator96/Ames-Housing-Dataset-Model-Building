# Author - Sachin Santhosh
# Date - September 1st 2019
# Description -   Predictive analytics using Kaggle dataset on home prices.
#First File where data cleaning is performed

#Start by cleaning up memory completely
rm(list=ls())

# Setting global option of decimal values=0
options(digits = 4)

# Call all required library packages:
install.packages('data.table')
library(data.table)   



# We will follow the steps as in this order:
# 1. Obtain data
# 2. Cleandata
# 3. Feature Selection
# 4. Choose an algorithm.
# 5. Predict values for the test set. 



# ---------------------------------------------------------------- #
#  I Section - Load the files and check for missing values
# ---------------------------------------------------------------- #

hptr = data.frame(fread("train.csv"), stringsAsFactors = FALSE)
hpte = data.frame(fread("test.csv"), stringsAsFactors = FALSE)





# Checking for number of NAs for each variable
sapply(hptr, function(x) sum(is.na(x)))
# Training set shows missing values for the following variables:
# LotFrontage - 259, Alley - 1369, MasVnrType - 8, BsmtQual - 37,
# BsmtCond - 37, BsmtExposure - 38, BsmtFinType1 - 37, BsmtFinType2 - 38,
# FireplaceQu - 690, GarageType - 81, GarageCond - 81, PoolQC - 1453, 
# Fence - 1179, MiscFeature - 1406.

# We will discard the following columns as they have too many(More than 40%) missing 
# values to be of any use. The rest of the columns we will process in the 
# next section 
hptr$Alley <- NULL
hptr$FireplaceQu <- NULL
hptr$Fence <- NULL
hptr$MiscFeature <- NULL
hptr$PoolQC <- NULL


sapply(hpte, function(x) sum(is.na(x)))
# Test set shows missing values for the following variables:
# LotFrontage - 227, Alley - 1352, MasVnrType - 16, BsmtQual - 44,
# BsmtCond - 45, BsmtExposure - 44, BsmtFinType1 - 42, BsmtFinType2 - 42,
# FireplaceQu - 730, GarageType - 76, GarageCond - 78, PoolQC - 1456, 
# Fence - 1169, MiscFeature - 1408.
# MSZoning - 4, Utilities - 2, Exterior1st - 1, Exterior2nd - 2, 
# MasVnrArea - 15, BsmtFinSF1 - 1, BsmtFinSF2 - 1, BsmtUnfSF - 1, 
# TotalBsmtSF - 1, BsmtFullBath - 2, BsmtHalfBath - 2, KitchenQual - 1,
# GarageYrBlt/ GarageQual/ GarageFinish - 78, GarageCars/ GarageArea - 1,
# SaleType - 1 


# We start by deleting the columns we deleted for training set. 
# The rest we will process later.
hpte$Alley <- NULL
hpte$FireplaceQu <- NULL
hpte$Fence <- NULL
hpte$MiscFeature <- NULL
hpte$PoolQC <- NULL




# -------------------------------------------------------- #
#  II Section - Data Cleansing
#  We explore information in all the columns in the dataset and also assign values for the missing/empty values.
# -------------------------------------------------------- #


# We start with the variables we identified in the previous section.
# LotFrontage: Linear feet of street connected to property
# a) Training set:
x = data.frame(table(hptr$LotFrontage))
sum(x$Freq) 
# We see ~260 missing variables which we will code as "0"
hptr$LotFrontage[is.na(hptr$LotFrontage)] <- 0

# b) Test set:
y = data.frame(table(hpte$LotFrontage))
sum(y$Freq) 
hpte$LotFrontage[is.na(hpte$LotFrontage)] <- 0 
# We code for ~230 missing records in test dataset.


# MasVnrType - We mark "empty" cells with the majority option "None"
x = data.frame(table(hptr$MasVnrType))
sum(x$Freq)
hptr$MasVnrType[is.na(hptr$MasVnrType)] <- "None"

y = data.frame(table(hpte$MasVnrType))
sum(y$Freq)
hpte$MasVnrType[is.na(hpte$MasVnrType)] <- "None"


# BsmtQual - No single majority option, so we create a new option called "unk"

x = data.frame(table(hptr$BsmtQual))
sum(x$Freq)
hptr$BsmtQual[is.na(hptr$BsmtQual)] <- "unk"

y = data.frame(table(hpte$BsmtQual))
sum(y$Freq)
hpte$BsmtQual[is.na(hpte$BsmtQual)] <- "unk"


# BsmtCond - similar to previous one, we create a new option called "unk"
x = data.frame(table(hptr$BsmtCond))
sum(x$Freq)
hptr$BsmtCond[is.na(hptr$BsmtCond)] <- "unk"

y = data.frame(table(hpte$BsmtCond))
sum(y$Freq)
hpte$BsmtCond[is.na(hpte$BsmtCond)] <- "unk"


# BsmtExposure - Missing values are marked as "No"
x = data.frame(table(hptr$BsmtExposure))
sum(x$Freq)
hptr$BsmtExposure[is.na(hptr$BsmtExposure)] <- "No"

y = data.frame(table(hpte$BsmtExposure))
sum(y$Freq)
hpte$BsmtExposure[is.na(hpte$BsmtExposure)] <- "No"


# BsmtFinType1 - Missing values are marked as "unk"
x = data.frame(table(hptr$BsmtFinType1))
sum(x$Freq)
hptr$BsmtFinType1[is.na(hptr$BsmtFinType1)] <- "unk"

y = data.frame(table(hpte$BsmtFinType1))
sum(y$Freq)
hpte$BsmtFinType1[is.na(hpte$BsmtFinType1)] <- "unk"


# BsmtFinType2 - Missing values are marked as "unk"
x = data.frame(table(hptr$BsmtFinType2))
sum(x$Freq)
hptr$BsmtFinType2[is.na(hptr$BsmtFinType2)] <- "unk"

y = data.frame(table(hpte$BsmtFinType2))
sum(y$Freq)
hpte$BsmtFinType2[is.na(hpte$BsmtFinType2)] <- "unk"


# GarageType - Missing values are marked as "Attchd"
x = data.frame(table(hptr$GarageType))
sum(x$Freq)
hptr$GarageType[is.na(hptr$GarageType)] <- "Attchd"

y = data.frame(table(hpte$GarageType))
sum(y$Freq)
hpte$GarageType[is.na(hpte$GarageType)] <- "Attchd"



# GarageCond - Missing values are marked as "TA"
x = data.frame(table(hptr$GarageCond))
sum(x$Freq)
hptr$GarageCond[is.na(hptr$GarageCond)] <- "TA"

y = data.frame(table(hpte$GarageCond))
sum(y$Freq)
hpte$GarageCond[is.na(hpte$GarageCond)] <- "TA"


# ----------------------------------------------------------------- #
# These values are missing only in the test set.
# Applying the same strategy here too.
# ----------------------------------------------------------------- #

# MSZoning - Missing values are marked as "RL"
y = data.frame(table(hpte$MSZoning))
sum(y$Freq)
hpte$MSZoning[is.na(hpte$MSZoning)] <- "RL"
hptr$MSZoning[is.na(hptr$MSZoning)] <- "RL"


# Utilities - Missing values are marked as "AllPub"
y = data.frame(table(hpte$Utilities))
sum(y$Freq)
hpte$Utilities[is.na(hpte$Utilities)] <- "AllPub"
hpte$Utilities[is.na(hpte$Utilities)] <- "AllPub"


# Exterior1st - Missing values are marked as "VinylSd"
y = data.frame(table(hpte$Exterior1st))
sum(y$Freq)
hpte$Exterior1st[is.na(hpte$Exterior1st)] <- "VinylSd"
hptr$Exterior1st[is.na(hptr$Exterior1st)] <- "VinylSd"


# Exterior2nd - Missing values are marked as "VinylSd"
y = data.frame(table(hpte$Exterior2nd))
sum(y$Freq)
hpte$Exterior2nd[is.na(hpte$Exterior2nd)] <- "VinylSd"
hptr$Exterior2nd[is.na(hptr$Exterior2nd)] <- "VinylSd"


# MasVnrArea - Missing values are marked as 0
y = data.frame(table(hpte$MasVnrArea))
sum(y$Freq)
hpte$MasVnrArea[is.na(hpte$MasVnrArea)] <- 0
hptr$MasVnrArea[is.na(hptr$MasVnrArea)] <- 0


# BsmtFinSF1  - Missing values are marked as 0
y = data.frame(table(hpte$BsmtFinSF1 ))
sum(y$Freq)
hpte$BsmtFinSF1 [is.na(hpte$BsmtFinSF1 )] <- 0
hptr$BsmtFinSF1 [is.na(hptr$BsmtFinSF1 )] <- 0


# BsmtFinSF2 - Missing values are marked as 0
y = data.frame(table(hpte$BsmtFinSF2))
sum(y$Freq)
hpte$BsmtFinSF2[is.na(hpte$BsmtFinSF2)] <- 0
hptr$BsmtFinSF2[is.na(hptr$BsmtFinSF2)] <- 0


# BsmtUnfSF - Missing values are marked as 0
y = data.frame(table(hpte$BsmtUnfSF))
sum(y$Freq)
hpte$BsmtUnfSF[is.na(hpte$BsmtUnfSF)] <- 0
hptr$BsmtUnfSF[is.na(hptr$BsmtUnfSF)] <- 0


# TotalBsmtSF - Missing values are marked as- 1
y = data.frame(table(hpte$TotalBsmtSF))
sum(y$Freq)
hpte$TotalBsmtSF[is.na(hpte$TotalBsmtSF)] <- -1
hptr$TotalBsmtSF[is.na(hptr$TotalBsmtSF)] <- -1


# BsmtFullBath - Missing values are marked as 0
y = data.frame(table(hpte$BsmtFullBath))
sum(y$Freq)
hpte$BsmtFullBath[is.na(hpte$BsmtFullBath)] <- 0
hptr$BsmtFullBath[is.na(hptr$BsmtFullBath)] <- 0


# BsmtHalfBath - Missing values are marked as 0
y = data.frame(table(hptr$BsmtHalfBath))
sum(y$Freq)
hpte$BsmtHalfBath[is.na(hpte$BsmtHalfBath)] <- 0
hptr$BsmtHalfBath[is.na(hptr$BsmtHalfBath)] <- 0


# KitchenQual - Missing values are marked as"TA"
y = data.frame(table(hpte$KitchenQual))
sum(y$Freq)
hpte$KitchenQual[is.na(hpte$KitchenQual)] <- "TA"
hptr$KitchenQual[is.na(hptr$KitchenQual)] <- "TA"


# GarageYrBlt - Missing values are marked as 0
y = data.frame(table(hpte$GarageYrBlt))
sum(y$Freq)
hpte$GarageYrBlt[is.na(hpte$GarageYrBlt)] <- 0
hptr$GarageYrBlt[is.na(hptr$GarageYrBlt)] <- 0


# GarageQual - Missing values are marked as "TA"
y = data.frame(table(hpte$GarageQual))
sum(y$Freq)
hpte$GarageQual[is.na(hpte$GarageQual)] <- "-TA"
hptr$GarageQual[is.na(hptr$GarageQual)] <- "TA"


# GarageFinish - Missing values are marked as"unk"
y = data.frame(table(hpte$GarageFinish))
sum(y$Freq)
hpte$GarageFinish[is.na(hpte$GarageFinish)] <- "unk"
hptr$GarageFinish[is.na(hptr$GarageFinish)] <- "unk"

 
# GarageCars - Missing values are marked as 2
y = data.frame(table(hpte$GarageCars))
sum(y$Freq)
hpte$GarageCars[is.na(hpte$GarageCars)] <- 2
hptr$GarageCars[is.na(hptr$GarageCars)] <- 2


# SaleType - Missing values are marked as "WD"
y = data.frame(table(hpte$SaleType))
sum(y$Freq)
hpte$SaleType[is.na(hpte$SaleType)] <- "WD"
hptr$SaleType[is.na(hptr$SaleType)] <- "WD"


# GarageArea - Missing values are marked as 0
y = data.frame(table(hpte$GarageArea))
sum(y$Freq)
hpte$GarageArea[is.na(hpte$GarageArea)] <- 0
hptr$GarageArea[is.na(hptr$GarageArea)] <- 0


# Checking for NAs again:
sapply(hptr, function(x) sum(is.na(x)))
sapply(hpte, function(x) sum(is.na(x)))

# Writing the new  cleaned files seaprately for further steps

write.csv(hptr, "newtrain.csv", row.names = FALSE)
write.csv(hpte, "newtest.csv", row.names = FALSE)






