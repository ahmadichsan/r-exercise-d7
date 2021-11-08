### Homework Day 7

### Author: Ahmad Ichsan Baihaqi
### Email: ahmadichsanbaihaqi@gmail.com

housePrice = read.csv('train.csv')
# View(housePrice)

## Missing Value and Cleansing Missing Value
# Find variable that contain missing value
missingValue = is.na(housePrice)

# Count and summarize variable that contain missing value
sumMissingValue = colSums(missingValue)
# View(sumMissingValue)

# Varable to be cleansed, for this case, I choose variable MasVnrArea
# MasVnrArea has 8 missing value

# Remove row which has missing value in MasVnrArea
filterRowNonMissing = !is.na(housePrice['MasVnrArea'])

# House price after MasVnrArea with missing value has been removed
housePriceRmMasVnrArea = housePrice[filterRowNonMissing,]
# View(summary(housePriceRmMasVnrArea))

# Now, MasVnrArea has 0 missing value
# View(colSums(is.na(housePriceRmMasVnrArea)))

# ===============================================================
# About standardization and normalization
# use standardization if the data is normally distributed/gaussian
# use standardization if the model used assumed about normality (linear regression)
# user normalization if both of criteria above is not fit

# Variable used for standardization/normalization is YearBuilt

# Make sure that YearBuilt didn't have missing value
# print(sum(is.na(housePriceRmMasVnrArea$YearBuilt)))

# Define if variable YearBuilt is normally distributed or not
yearBuilt = housePriceRmMasVnrArea$YearBuilt
hist(yearBuilt)

# Based on above visualization, the YearBuilt variable categorized as Negative Skew
# It means, the data more distributed to the right
# Based on above observation, we couldn't use standardization, instead, we use normalization

# Normalization
# is a process to transform values of features into certain scale
# install.packages('caret')
library(caret)

normScales = preProcess(housePriceRmMasVnrArea['YearBuilt'], method = c('range'))
housePriceRmMasVnrArea['YearBuilt'] = predict(normScales, housePriceRmMasVnrArea['YearBuilt'])
# View(housePriceRmMasVnrArea)
# Now, YearBuilt has been normalized between 0 and 1
# summary(housePriceRmMasVnrArea$YearBuilt)

# ===============================================================
# Data Transformation
# Transforming SalePrice variable into normally distributed
salesPrice = housePriceRmMasVnrArea$SalePrice

# min 34900
# max 755000
# summary(salesPrice)

hist(salesPrice)
# Based on above visualization, raw data of SalesPrice is Positively Skewed

# For this case, I use log transformation
# Now, log the salesPrice to transform it into normally distributed
salesPriceLog = log(salesPrice)

hist(salesPriceLog)
# Now, based on above visualization, SalePrice data is now normally distributed

# Now, repopulate the updated SalePrice data into our data frame
housePriceRmMasVnrArea$SalePrice = salesPriceLog
# hist(housePriceRmMasVnrArea$SalePrice)

# ===============================================================
# Outlier Handling
# Now, handling outlier in logged SalePrice
boxplot(housePriceRmMasVnrArea$SalePrice)

# First, check if SalesPriceLog has outliers or not
# Q1 value => 11.77452
salesQ1 = quantile(salesPriceLog, 0.25)
# print(salesQ1)

# Q3 value => 12.27373
salesQ3 = quantile(salesPriceLog, 0.75)
# print(salesQ3)

# IQR => 0.499
salesIqr = IQR(salesPriceLog)

# Minimum value to define if the data is outliers or not => 11.0257
minSalesValue = salesQ1 - (1.5 * salesIqr)
# print(minSalesValue)

# Now, find the minimum value in our data => 10.46024
minSalesData = min(salesPriceLog)
# print(minSalesData)

# Maximum value to define if the data is outliers or not => 13.02255
maxSalesValue = salesQ3 + (1.5 * salesIqr)
# print(maxSalesValue)

# Now, find the maximum value in our data => 13.53447
maxSalesData = max(salesPriceLog)
# print(max_sales_data)

# Now, check if there are any outliers => result is TRUE
isHasOutliers = (minSalesData < minSalesValue) | (maxSalesData > maxSalesValue)
# print(isHasOutliers)

# Now, filter data only use data which has SalesPrice
# a. more than minSalesValue AND
# b. less than maxSalesValue
filterRowNonOutlier = housePriceRmMasVnrArea$SalePrice > minSalesValue &
  housePriceRmMasVnrArea$SalePrice < maxSalesValue

# Data after filtered with outlier SalesPrice
housePriceRmMasVnrArea = housePriceRmMasVnrArea[filterRowNonOutlier,]

boxplot(housePriceRmMasVnrArea$SalePrice)
# Based on above visualization, it still has outliers, let's iterate once more

## Outlier Handline 2nd iteration
salesNewIteration = housePriceRmMasVnrArea$SalePrice

# Q1 value => 11.77529
salesQ1Two = quantile(salesNewIteration, 0.25)
# print(salesQ1Two)

# Q3 value => 12.26905
salesQ3Two = quantile(salesNewIteration, 0.75)
# print(salesQ3Two)

# IQR => 0.4937577
salesIqrTwo = IQR(salesNewIteration)
# print(salesIqrTwo)

# Minimum value to define if the data is outliers or not => 11.03465
minSalesValueTwo = salesQ1Two - (1.5 * salesIqrTwo)
# print(minSalesValueTwo)

# Now, find the minimum value in our data => 11.04105
minSalesDataTwo = min(salesNewIteration)
# print(minSalesDataTwo)

# Maximum value to define if the data is outliers or not => 13.00968
maxSalesValueTwo = salesQ3Two + (1.5 * salesIqrTwo)
# print(maxSalesValueTwo)

# Now, find the maximum value in our data => 13.02133
maxSalesDataTwo = max(salesNewIteration)
# print(maxSalesDataTwo)

# Now, check if there are any outliers => result is TRUE
isHasOutliersTwo = (minSalesDataTwo < minSalesValueTwo) | (maxSalesDataTwo > maxSalesValueTwo)
# print(isHasOutliersTwo)

# Now, filter data only use data which has SalesPrice
# a. more than minSalesValueTwo AND
# b. less than maxSalesValueTwo
filterRowNonOutlierTwo = salesNewIteration > minSalesValueTwo &
  salesNewIteration < maxSalesValueTwo

# Data after filtered with outlier SalesPrice
housePriceRmMasVnrArea = housePriceRmMasVnrArea[filterRowNonOutlierTwo,]

boxplot(housePriceRmMasVnrArea$SalePrice)
# No outlier anymore XD

# ===============================================================
# Data Categorical Handling
# For this case, I choose MSZoning variable

table(housePriceRmMasVnrArea$MSZoning)
# based on above observation, MSZoning seems contain categorical data which not an ordered categorical
# it means we can used One Hot Encoding for MSZoning variable

# create dummy var
dummy = dummyVars("~.", data = housePriceRmMasVnrArea['MSZoning'])
myDummyVar = data.frame(predict(dummy, newdata = housePriceRmMasVnrArea['MSZoning']))
# View(myDummyVar)

housePriceRmMasVnrArea = cbind(housePriceRmMasVnrArea, myDummyVar)

# Our final data
# var missing value => MasVnrArea
# var normalization => YearBuilt
# var data transformation and outlier handling => SalePrice
# var categorical handling => MSZoning
finalData = data.frame(
  housePriceRmMasVnrArea$MasVnrArea,
  housePriceRmMasVnrArea$YearBuilt,
  housePriceRmMasVnrArea$SalePrice,
  housePriceRmMasVnrArea$MSZoning,
  housePriceRmMasVnrArea$MSZoningC..all.,
  housePriceRmMasVnrArea$MSZoningFV,
  housePriceRmMasVnrArea$MSZoningRH,
  housePriceRmMasVnrArea$MSZoningRL,
  housePriceRmMasVnrArea$MSZoningRM
)
# View(finalData)

colnames(finalData)[which(names(finalData) == "housePriceRmMasVnrArea.MasVnrArea")] = "MasVnrArea"
colnames(finalData)[which(names(finalData) == "housePriceRmMasVnrArea.YearBuilt")] = "YearBuilt"
colnames(finalData)[which(names(finalData) == "housePriceRmMasVnrArea.SalePrice")] = "SalePrice"
colnames(finalData)[which(names(finalData) == "housePriceRmMasVnrArea.MSZoning")] = "MSZoning"
colnames(finalData)[which(names(finalData) == "housePriceRmMasVnrArea.MSZoningC..all.")] = "MSZoningC..all."
colnames(finalData)[which(names(finalData) == "housePriceRmMasVnrArea.MSZoningFV")] = "MSZoningFV"
colnames(finalData)[which(names(finalData) == "housePriceRmMasVnrArea.MSZoningRH")] = "MSZoningRH"
colnames(finalData)[which(names(finalData) == "housePriceRmMasVnrArea.MSZoningRL")] = "MSZoningRL"
colnames(finalData)[which(names(finalData) == "housePriceRmMasVnrArea.MSZoningRM")] = "MSZoningRM"

View(finalData)

# Export to csv
currentDir = getwd()
csvFilePath = paste0(currentDir, "/cleansed.csv")
write.csv(finalData, csvFilePath, row.names = FALSE)

# cleansedData = read.csv('cleansed.csv')
# View(cleansedData)



