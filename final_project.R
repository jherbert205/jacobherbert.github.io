#Download necessary packages
library(quantmod)
library(tidyquant)
library(dplyr)
library(ISLR)
library(glmnet)

#Create list of companies to analyze
#Twenty largest energy companies by market cap in the world
myList = c("XOM", "CVX", "RYDBF", "TOT", "PTR", "BP", "PBR", "ENB", "SNP", "NTOIF", "CUAEF", "NOVKY", "LUKOY", "EPD", "COP", "TRP", "CEO", "E", "SLB", "KMI")


#Import data and create variables that do not depend on the equity being analyzed
#Commodities
natgas <- lag(as.vector(getSymbols("UNG", source = "yahoo", auto.assign = FALSE, return.class = "xts")[,6]["2018/"]), 30)
natGasChange <- natgas - lag(natgas, 5) 
oil <- lag(as.vector(getSymbols("USO", source = "yahoo", auto.assign = FALSE, return.class = "xts")[,6]["2018/"]), 30)
oilChange <- oil - lag(oil, 5) 
coalChange <- coal - lag(coal, 5)
coal <- lag(as.vector(getSymbols("KOL", source = "yahoo", auto.assign = FALSE, return.class = "xts")[,6]["2018/"]), 30)

#Equity ETFs and volatility index
SP500 <-  lag(as.vector(getSymbols("VOO", source = "yahoo", auto.assign = FALSE, return.class = "xts")[,6]["2018/"]), 30)
SP500Change <- lag(SP500, 5) - SP500
dow <-  lag(as.vector(getSymbols("DIA", source = "yahoo", auto.assign = FALSE, return.class = "xts")[,6]["2018/"]), 30)
dowChange <- lag(dow, 5) - dow
vix <- lag(as.vector(getSymbols("UVXY", source = "yahoo", auto.assign = FALSE, return.class = "xts")[,6]["2018/"]), 30)


#Initialize indicators
simple <- c()
lasso <- c()
ridge <- c()


#Beginning of loop to analyze each stock in my list
for(val in myList)
{
  
#Import data and create variables that depend on the equity analyzed
  stock <- lag(as.vector(getSymbols(val, source = "yahoo", auto.assign = FALSE, return.class = "xts")[,6]["2018/"]), 30)
  priceChange <-  getSymbols(val, source = "yahoo", auto.assign = FALSE, return.class = "xts")[,6]["2018/"]-stock
  volume <- lag(as.vector(getSymbols(val, source = "yahoo", auto.assign = FALSE, return.class = "xts")[,5]["2018/"]), 30)
  macd <- MACD(stock, nFast = 12, nSlow = 26, nSig = 9, percent = TRUE)
  rsi <- RSI(stock, n=14)
  rsiVar <- rsi - mean(rsi, na.rm = TRUE)
  moment <- momentum(stock, n = 14)
  
  
#Create table with all relevant data to ensure data lines up properly and can be more easily used in analysis
  Data <- na.omit(as.data.frame(cbind(stock, priceChange,SP500, rsiVar, moment, SP500Change, natgas, oil, coal, vix, coalChange, oilChange, natGasChange, volume, dow, dowChange)))
  colnames(Data)<- c("stock", "priceChange", "SP500", "rsiVar", "momentum", "SP500Change", "natgas", "oil", "coal", "vix", "coalChange", "oilChange", "natGasChange", "volume", "dow", "dowChange")
  

#Create simple linear model with all variables included
  simpleReg <- lm(priceChange ~ stock+SP500+rsiVar+momentum+SP500Change+natgas+oil+coal+vix+coalChange+oilChange+natGasChange+volume+dow+dowChange, data=Data)
 
#Create variables to more easily implement LASSO and Ridge Regression
  X <- model.matrix(Data$priceChange~.-1,data=Data)
  y <- Data$priceChange 
  
#Create LASSO model
  fitLasso <- glmnet(X,y)
  cvLasso <- cv.glmnet(X,y)
  
#Create Ridge Regression model
  fitRidge <- glmnet(X, y, alpha=0)
  cvRidge <- cv.glmnet(X, y, alpha = 0)
  
#Create 30 day change prediction
   simplePredict <-predict(simpleReg)[length(Data$stock)]
   lassoPredict <- predict(cvLasso, newx = X)[length(Data$stock)]
   ridgePredict <- (as.matrix(cbind(const=1,X)) %*% coef(cvRidge))[length(Data$stock)]
     
#Add prediction to corresponding vector
  simple <- append(simple, simplePredict)
  lasso <-  append(lasso, lassoPredict)
  ridge <-  append(ridge, ridgePredict)
  
}  

#Combine result vectors to create table
predictionTable <- as.data.frame(cbind(myList, simple, lasso, ridge))
colnames(predictionTable) <- c("Stock", "LM Prediction", "LASSO Prediction", "RR Prediction")

#Display results in form of chart
print(predictionTable)




#Create model for individual stock with more detail
#For an example, I will use BP, but any of them can be used

#import and create variables
stock <- lag(as.vector(getSymbols("BP", source = "yahoo", auto.assign = FALSE, return.class = "xts")[,6]["2018/"]), 30)
priceChange <-  getSymbols("BP", source = "yahoo", auto.assign = FALSE, return.class = "xts")[,6]["2018/"]-stock
View(priceChange)
volume <- lag(as.vector(getSymbols("BP", source = "yahoo", auto.assign = FALSE, return.class = "xts")[,5]["2018/"]), 30)
rsi <- RSI(stock, n=14)
rsiVar <- rsi - mean(rsi, na.rm = TRUE)
moment <- momentum(stock, n = 14)

#Make data table
Data <- na.omit(as.data.frame(cbind(stock, priceChange,SP500, rsiVar, moment, SP500Change, natgas, oil, coal, vix, coalChange, oilChange, natGasChange, volume, dow, dowChange)))
colnames(Data)<- c("stock", "priceChange", "SP500", "rsiVar", "momentum", "SP500Change", "natgas", "oil", "coal", "vix", "coalChange", "oilChange", "natGasChange", "volume", "dow", "dowChange")


#Create Simple Linear, LASSO, and Ridge Regression Models
simpleReg <- lm(priceChange ~ stock+SP500+rsiVar+momentum+SP500Change+natgas+oil+coal+vix+coalChange+oilChange+natGasChange+volume+dow+dowChange, data=Data)
X <- model.matrix(Data$priceChange~.-1,data=Data)
y <- Data$priceChange
fitLasso <- glmnet(X,y)
cvLasso <- cv.glmnet(X,y)
fitRidge <- glmnet(X, y, alpha=0)
cvRidge <- cv.glmnet(X, y, alpha = 0)


#View simple linear regression results
summary(simpleReg)
plot(simpleReg)


#View LASSO results
plot(fitLasso,xvar="lambda",label=TRUE,main="BP LASSO")
plot(cvLasso,main="BP LASSO CV")
summary(cvLasso)
coef(cvLasso)


#View ridge regression results
plot(fitRidge,xvar="lambda",label=TRUE,main="BP RR")
plot(cvRidge,main="BP RR CV")
summary(cvRidge)
coef(cvRidge)
