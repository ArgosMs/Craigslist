# Linear Regression

dataset <- Craigslist_SPSS_Model_Variables_Final_Dataset

str(dataset)

Sale <- dataset$"Craigslist/Oct 9 2018/For Sale/All/Owner/Has Image/Deleted_Less_100"

Age <- dataset$"Median Age"

Education <- dataset$`Educational Attainment: Percent high school graduate or higher`

Income <- dataset$`Median Household Income`

Foreign <- dataset$`Foreign Born Population`

# Graphical Analysis

# Scatter Plot (Grafico de Dispersao): Check for associations that are linear and additive

par(mfrow=c(2, 2))

scatter.smooth(x = Sale, 
               y = Age,
               main = "For Sale - Median Age")

scatter.smooth(x = Sale, 
               y = Education,
               main = "For Sale - Education")

scatter.smooth(x = Sale, 
               y = Income,
               main = "For Sale - Income")

scatter.smooth(x = Sale, 
               y = Foreign,
               main = "For Sale - Foreign Population")

getwd()

# BoxPlot (Diagrama de Caixa): Check for Outliers (Valor Atipico)

par(mfrow=c(1, 5))  # divide graph area in 5 columns # 'par': set or query graphic parameters (definir/consultar)
# mfrow: to create matrix of nrows and ncols

dev.off()

# pars=list(par(mar=c(21,1,3,1))))

par("mar")

par(mfrow=c(5,1), # five lines and one column
    mar=c(2,2,2,2)) # bottom, left, top, right # set margins

# Boxplots without outliers

boxplot(Age, 
        main="Median Age",
        cex.main = 1,
        horizontal=TRUE,
        outline = FALSE)

boxplot(Education, 
        main="Education", 
        cex.main = 1,
        horizontal = T,
        outline = FALSE)

boxplot(Income, 
        main="Income", 
        cex.main = 1,
        horizontal=TRUE,
        outline = FALSE)

boxplot(Foreign, 
        main="Foreign Population", 
        cex.main = 1,
        horizontal=TRUE,
        outline = FALSE)

boxplot(Sale, 
        main="For Sale", 
        cex.main = 1,
        horizontal=TRUE,
        outline = FALSE)

## Density plot (Grafico de Densidade): Check if response variable is close to normality

install.packages("e1071")

library(e1071)

getwd()

par(mfrow=c(1, 1))  # divide graph area in 5 columns

plot(density(Age), main="Median Age", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(dataset$`Median Age`), 2)))  # density plot for 'Median Age'
polygon(density(Age), col="red") # :: to access specific function from specific package

plot(density(Education), main="Education", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(dataset$`Educational Attainment: Percent high school graduate or higher`), 2)))  # density plot for 'Education'
polygon(density(Education), col="red")

plot(density(Income), main="Income", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(dataset$`Median Household Income`), 2)))  # density plot for 'Income'
polygon(density(Income), col="red")

plot(density(Foreign), main="Foreign Population", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(dataset$`Foreign Born Population`), 2)))  # density plot for 'Foreign Population'
polygon(density(Foreign), col="red")

plot(density(Sale), main="Items for Sale", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Sale), 2)))  # density plot for 'For Sale'
polygon(density(Sale), col="gray")

# Skewness: measures asymmetry of the probability distribution of a real-valued random variable about its mean

# If skewness is less than -1 or greater than 1, the distribution is highly skewed.
# If skewness is between -1 and -0.5 or between 0.5 and 1, the distribution is moderately skewed.
# If skewness is between -0.5 and 0.5, the distribution is approximately symmetric.

# Correlation: measures linear dependence between two variables that occur in pair. 

age_cor <- cor(Age, Sale)
edu_cor <- cor(Education, Sale)
inc_cor <- cor(Income, Sale)
for_cor <- cor(Foreign, Sale)

cor_combined <- c(age_cor, edu_cor, inc_cor, for_cor)
cor_names <- c("Age - Sale", "Education - Sale", "Income - Sale", "Foreign - Sale")

cor_table <- rbind(cor_names, cor_combined)

cor_table_combined <- xtable(cor_table)
print.xtable(cor_table_combined, type="html", file="Craigslist_cor_table.html")

# Model

# Correlation can take values between -1 to +1. Strong positive correlation: closer to 1. The opposite is true.
# Value closer to 0 suggests a weak relationship between variables. A low correlation (-0.2 < x < 0.2) 
# probably suggests that much of variation of the response variable (Y) is unexplained by the predictor (X), 
# in which case, we should probably look for better explanatory variables.

# Multiple Linear Regression: explain relationship between one continuous dependent variable 
# and two or more independent variables.  Independent variables can be continuous or categorical 
# (dummy coded as appropriate).

fit <- lm(Sale ~ 
            Age + 
            Education + 
            Income +
            Foreign, data=dataset)

summary(fit)

# model p-value: < 2.2e-16 (model is statistically significant).
# This increases the confidence that predicted values from the model
# are not construed as an event of chance

fit_table <- xtable(fit)
print.xtable(fit_table, type="html", file="Craigslist_model_table.html")

getwd()

vif(fit)

## p-Value: it tells if a relationship is statistically significant. 
# Only when both these p-Values are less that the pre-determined statistical significance level, which is ideally 0.05. 
# This is visually interpreted by the significance stars at the end of the row. The more the stars beside the variable's p-Value, the more significant the variable.

# Pr(>|t|) or p-value is the probability that you get a t-value as high or higher than the observed value when the Null Hypothesis (the beta coefficient is equal to zero or that there is no relationship) is true. 
# So if the Pr(>|t|) is low, the coefficients are significant (significantly different from zero). 
# If the Pr(>|t|) is high, the coefficients are not significant.

# Null Hypothesis: coefficients associated with the variables is equal to zero. 
# Alternate hypothesis: coefficients are not equal to zero (i.e. there exists a relationship between the independent variable in question and the dependent variable).

# t-value: indicates that it is less likely that the coefficient is not equal to zero purely by chance. 
# So, higher the t-value, the better.

# R-Squared: tells the proportion of variation in the dependent (response) variable that has been 
# explained by this model.

# Adj R-Squared: adjusted for the number of predictors in the model. The adjusted R-squared 
# increases only if the new term improves the model more than would be expected by chance.

# Other useful functions

coefficients(fit) # model coefficients

# Formula: For Sale = -1.935502e+02 + 1.081418e+03 * `Education' + 6.849933e-02 * Foreign Population

confint(fit, level=0.95) # CIs for model parameters 

fitted(fit) # predicted values

residuals(fit) # residuals

anova(fit) # anova table 

vcov(fit) # covariance matrix for model parameters 

influence(fit) # regression diagnostics

# Akaike's information criterion (AIC) and Bayesian information criterion (BIC)
# are measures of the goodness of fit of an estimated statistical model and can also be used for model selection.

AIC(fit) # 3058.02 (lower the better)

BIC(fit) # 3077.958 (lower the better)

# Predicting Linear Models (Modelos Lineares de Previsao)
# predicting how the model will perform with new data
# split dataset into 80:20 sample (training:test)
# build model on 80% sample and use it to predict dependent variable on test data

# Create Training and Test data

set.seed(100)  # setting seed to reproduce results of random sampling (amostragem aleatoria)

trainingRowIndex <- sample(1:nrow(dataset), 0.8*nrow(dataset))  # row indices for training data

trainingData <- dataset[trainingRowIndex, ]  # model training data

testData  <- dataset[-trainingRowIndex, ]   # test data

# Build the model on training data 

lmMod <- lm(Sale ~ 
              Age + 
              Education + 
              Income +
              Foreign, data=trainingData)  # build the model

OnSalePred <- predict(lmMod, testData)  # predict On Sale

# Review diagnostic measures

summary (lmMod)

lmMod_table <- xtable(lmMod)
print.xtable(lmMod_table, type="html", file="Craigslist_training_model_table.html")

getwd()

AIC(lmMod) # 2451.485

# model p value and predictor's p value are less than the significance level: statistically significant model. 
# the R-Sq and Adj R-Sq are comparative to the original model built on full data.

# Calculate prediction accuracy and error rates

actuals_preds <- data.frame(cbind(actuals=testData$'Craigslist/Oct 9 2018/For Sale/All/Owner/Has Image/Deleted_Less_100', 
                                  predicteds=OnSalePred))  # make actuals_predicteds dataframe. ## cbind coerce df into matrix

correlation_accuracy <- cor(actuals_preds)  

correlation_accuracy_table <- xtable(correlation_accuracy)
print.xtable(correlation_accuracy_table, type="html", file="Craigslist_correlation_accuracy_table.html")

head(actuals_preds)

# A simple correlation between the actuals and predicted values can be used as a form of accuracy measure (medida de precisao). 
# A higher correlation accuracy implies that the actuals and predicted values have similar directional movement, 
# i.e. when the actuals values increase the predicteds also increase and vice-versa.

#  Calculate Min Max accuracy and MAPE (Mean absolute percentage error): to find out the prediction accuracy of the model

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy # 0.7036012 (Higher the better)

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape # 0.4871067 (Lower the better)

# Other Dependent Variables

fit_household <- lm(dataset$`Craigslist/Oct 10 2018/Household Items/Has Image/By Owner` ~ 
            Age + 
            Education + 
            Income +
            Foreign, data=dataset)

summary(fit_household)

fit_furniture <- lm(dataset$`Furniture/Craigslist/Oct 11 2018/Has Image/By Owner` ~ 
                      Age + 
                      Education + 
                      Income +
                      Foreign, data=dataset)

summary(fit_furniture)

fit_general <- lm(dataset$`General/Craigslist/Oct 11 2018/Has Image/By Owner` ~ 
                      Age + 
                      Education + 
                      Income +
                      Foreign, data=dataset)

summary(fit_general)

fit_tickets <- lm(dataset$`Tickets/Craigslist/Oct 11 2018/Has Image/By Owner` ~ 
                      Age + 
                      Education + 
                      Income +
                      Foreign, data=dataset)

summary(fit_tickets)

fit_vehicles <- lm(dataset$`Cars_Trucks/Craigslist/Oct 11 2018/Has Image/By Owner` ~ 
                      Age + 
                      Education + 
                      Income +
                      Foreign, data=dataset)

summary(fit_vehicles)

fit_electronics <- lm(dataset$`Electronics/Craigslist/Oct 11 2018/Has Image/By Owner` ~ 
                      Age + 
                      Education + 
                      Income +
                      Foreign, data=dataset)

summary(fit_electronics)

# Territory Map with Zip Codes

install.packages('zipcode')
library(zipcode)

data(zipcode)

zipcode

str(zipcode$zip)

zip_TX <- matrix(zipcode)

zip_TX <- zipcode[35130:35553, ]

as.integer(rownames(zip_TX))

zip_TX$zip

zip_TX <- zip_TX[-c(1:2, 11, 53, 98, 101:192, 197, 199, 203:204, 207:211, 213, 217:220, 223:226, 228, 234, 238, 240:242, 246, 
                 248, 255, 259, 262:263, 265, 267:268, 271:274, 276:280, 283:284, 286:288, 290:295, 297:299, 301, 303,
                 306:313, 315, 317:323, 325, 327, 329:330, 334:337, 339:342,
                 344:345, 348:349, 351, 357:358, 361:362, 364,
                 367, 370, 375:376, 379:380, 383, 386:387, 390:391, 393:395, 398, 401, 404, 406, 408, 412,
                 415, 418, 420:423), ]

nrow(zip_TX)

zip_TX$zip

zip_TX

id <- c(1:205)

id

# Add Sales, Foreign, and Education to zip_TX

zip_TX <- cbind(zip_TX, Sale, Education, Foreign, id)

str(zip_TX)

head(zip_TX)

# Map

install.packages('ggalt')
install.packages('ggrepel')
install.packages("ggplot2")

library(ggplot2)
library(maps)
library(tidyverse)
library(rgdal)
library(rgeos)
library(maptools)
library(ggalt)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(raster)
library(sp)

# Prepare the zip poly data for Texas

mydata <- readOGR("C:/Users/aa/Documents/Papers/Paper_Maladaptive_Response/cb_2017_us_state_500k")

str(mydata)

View(mydata)

head(mydata)

mydata$NAME

mydata_TX <- mydata[mydata$NAME == 'Texas', ]

plot(mydata_TX)

head(mydata_TX)

str(mydata_TX)

class(mydata_TX)

# Convert 'SpatialPolygonsDataFrame' into 'data.frame'

# add to data a new column termed "id" composed of the rownames of data
mydata_TX@data$id <- rownames(mydata_TX@data)

# create a data.frame from our spatial object
zipPoints <- fortify(mydata_TX, region = "id")

# merge the "fortified" data with the data from our spatial object
zipDF <- merge(zipPoints, mydata_TX@data, by = "id")

library(plyr)

zipDF <- join(zipDF, mydata_TX@data, by = "id")

# drop first duplicate column 'NAME'

zipDF <- zipDF[, !duplicated(colnames(zipDF), fromLast = TRUE)] 

# Plot Texas

gg1 <- ggplot() +                                               
  geom_polygon(                                          
    data = zipDF,                                    
    aes(x = long, y = lat, group = group), 
    fill = "white", color = "black") + 
  coord_fixed(1.3) 

gg1

# Narrow down TX map

zipDF_Houston <- zipDF[1450:4750, ]
View(zipDF_Houston)

# Plot Houston

gg3 <- ggplot() +                                               
  geom_polygon(                                          
    data = zipDF_Houston,                                    
    aes(x = long, y = lat, group = group), 
    fill = "white", color = "black") + 
  coord_fixed(1) 

gg3

# add ZIP points

gg2 <- gg3 + geom_point(data = zip_TX, aes(x = longitude, y = latitude), 
                 color = 'grey',
                 size = 1) +
  labs(title = 'Suburbs Included') 

gg2

# Zooming in

gg4 <- ggplot() +                                               
  geom_polygon(                                          
    data = zipDF_Houston,                                    
    aes(x = long, y = lat, group = group), 
    fill = "white", color = "black") + 
    coord_cartesian(ylim=c(29,31), xlim=c(-94, -97))

gg4

# add zip to Houston Map

gg5 <- gg4 + geom_point(data = zip_TX, aes(x = longitude, y = latitude), 
                        color = 'grey',
                        size = 1) +
  labs(title = 'Suburbs Included') 

gg5

# Convert Craigslist dataset into data frame

Craiglist_DF <- as.data.frame(Craigslist_SPSS_Model_Variables_Final_Dataset)

class(Craiglist_DF)

# Add id to Craigslist DF

id <- c(1:205)

Craiglist_DF <- cbind(Craiglist_DF, id)

head(Craiglist_DF)

# merge zip_TX and Craiglist_DF by id

zip_Craig_combined <- merge(zip_TX, Craiglist_DF, by = "id")

View(zip_Craig_combined)


# add Sales layer

gg7 <- ggplot() + 
  geom_polygon(                                          
    data = zipDF_Houston,                                    
    aes(x = long, y = lat, group = group), 
    fill = "white", color = "black") + 
  coord_cartesian(ylim=c(29,31), xlim=c(-94.5, -96.5)) +
  geom_point(data = zip_TX, aes(x = longitude, y = latitude), 
             color = 'black',
             size = .1) +
    geom_point(data = zip_Craig_combined, 
             aes(x = longitude, y = latitude,
                 size = Sale,
                 color = Sale),
             alpha = .5) +
             scale_size(range = c(-10, 5)) +
    labs(title = 'Sales per Suburb') 

gg7

# add Foreign layer

gg8 <- ggplot() + 
  geom_polygon(                                          
    data = zipDF_Houston,                                    
    aes(x = long, y = lat, group = group), 
    fill = "white", color = "black") + 
  coord_cartesian(ylim=c(29,31), xlim=c(-94.5, -96.5)) +
  geom_point(data = zip_TX, aes(x = longitude, y = latitude), 
             color = 'black',
             size = .1) +
  geom_point(data = zip_Craig_combined, 
             aes(x = longitude, y = latitude,
                 size = Foreign,
                 color = Foreign),
             alpha = .5) +
  scale_size(range = c(-5, 5)) +
  labs(title = 'Immigrants per Suburb') 

gg8

# add Education layer

gg9 <- ggplot() + 
  geom_polygon(                                          
    data = zipDF_Houston,                                    
    aes(x = long, y = lat, group = group), 
    fill = "white", color = "black") + 
  coord_cartesian(ylim=c(29,31), xlim=c(-94.5, -96.5)) +
  geom_point(data = zip_TX, aes(x = longitude, y = latitude), 
             color = 'black',
             size = .1) +
  geom_point(data = zip_Craig_combined, 
             aes(x = longitude, y = latitude,
                 size = Education,
                 color = Education),
             alpha = .5) +
  scale_size(range = c(-20, 5)) +
  labs(title = 'Higher Education per Suburb') 

gg9

# Test 

library(ggplot2)
library(sf)

houston_zips <- readOGR("C:/Users/aa/Documents/Papers/Paper_Maladaptive_Response/Zip_Codes")

View(houston_zips)

class(houston_zips)

# Convert 'SpatialPolygonsDataFrame' into 'data.frame'

# add to data a new column termed "id" composed of the rownames of data
houston_zips@data$id <- rownames(houston_zips@data)

# create a data.frame from our spatial object
houston_zipPoints <- fortify(houston_zips, region = "id")

# merge the "fortified" data with the data from our spatial object
houston_zipDF <- merge(houston_zipPoints, houston_zips@data, by = "id")

View(houston_zipDF)

class(houston_zipDF)

## Add some features
houston_zipDF <- cbind(houston_zipDF, area = st_area(houston_zipDF),
                      label = sample(letters[1:5], NROW(houston_zipDF), replace = TRUE))

## Now plot
ggplot(houston_zips) + geom_sf()

ggplot(houston_zips, aes(fill = as.double(area))) + geom_sf() ## Need

## to remove unit info before using area for plotting
ggplot(houston_zips, aes(fill = factor(label))) + geom_sf()

# Test: Plot zip codes polygons 

gg6 <- ggplot() +
  geom_polygon (houston_zipDF, aes(x = long, y = lat,
                    fill = order, group = group),
                color = "white") +
  geom_path(zip_TX, aes(x = longitude,y = latitude)) +
  guides(fill = FALSE)

gg6

gg6 <- ggplot() +
  geom_polygon (houston_zipDF, aes(x = long, y = lat,
                                   fill = id, group = group),
                color = "white") 

gg6

# Test

ggplot(data = zipDF_Houston) + 
  geom_polygon(aes(x = long, y = lat, fill = order, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # do this to leave off the color legend



