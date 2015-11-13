#########################################################
#### Maritime Piracy Data Analysis ######################
#########################################################
#### by Laurence Hendry, Cody Koebnick, and BjÃ¶rn Boening

# Import the dataset about piracy attacks into your wd 
# Call libraries we need for the project, make sure you have them installed
library(base)
library(rio) # swiss army knife for imports
library(plyr) # count occurences
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(ggplot2) # nice plots
library(stargazer) # nicer regression output which looks like a real publication
library(car) # scatterplots 
library(httr) # scraping from http sites
library(XML) # Tool for generating XML file
library(WDI) # Scraping Data from the World Bank 
library(countrycode) # provides world bank country codes 
library(gplots)
library(plm)
#attach(shipping)

# set working directories 
try(setwd("/Users/codykoebnick/Documents/MaritimePiracy"))
try(setwd("E:/bjoer/Documents/Google Drive/Universität/Hertie/03_Fall 2015/05_Master Thesis/00_Piracy_2015-16/03_Data/Tennessee"))
getwd()
try(setwd("//Users/laurencehendry/GoogleDrive/Master Thesis - Shared/MPP-E1180 - Introduction to Collaborative Social Science Data Analysis")) 
getwd()

#import data
# empty cells are now coded with NA and can manually be excluded from any function with na.omit command
shipping <- read.csv("shippingraw.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))
# have a look at how the variables are created
str(shipping)

#renaming and dropping some of our columns 
names(shipping)[1] <- 'country'
names(shipping)[2] <- 'year'
shipping$X1 = NULL
names(shipping)[3] <- 'coast/Area ratio (m/km2)'
names(shipping)[4] <- 'GDP per cap'
names(shipping)[5] <- 'attacks/Year'
names(shipping)[6] <- 'successful Attacks/Year'
names(shipping)[7] <- 'success Ratio'

#######################
#Descriptive Statistics
#######################

#Histogram of attack frequnecy
hist(shipping$`attacks/Year`)
#Histogram of successful attacks per year
hist(shipping$`successful Attacks/Year`)
#histogram of attack success over GDP per cap
plot(shipping$`GDP per cap`, shipping$`success Ratio`)

#exploring the data 
coplot(shipping$`success Ratio` ~ year|country, type="l", data=shipping) # Lines
coplot(shipping$`success Ratio` ~ year|country, type="b", data=shipping) # Points and lines

scatterplot(shipping$`success Ratio` ~year|country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=shipping)

# Fixed effects: Heterogeneity across countries (or entities)
# interestingly, attacks in Philipines have the lowest success ratio and attacks in Brazil have the highest.
# plotmeans draw a 95% confidence interval around the means
plotmeans(shipping$`success Ratio` ~ country, main="Heterogeineity across countries", data=shipping)

# Fixed effects: Heterogeneity across years
# plotmeans draw a 95% confidence interval around the means
plotmeans(shipping$`success Ratio` ~ year, main="Heterogeineity across years", data=shipping)

# OLS Regression
# Regular OLS regression does not consider heterogeneity across groups or time.
# In this simple model, the number of attacks has a slightly negative relationship with attack success, however it is not stat. sig. 
ols <-lm(shipping$`success Ratio` ~ shipping$`attacks/Year`, data=shipping)
summary(ols)

# The below plot shows that after attacks in a certain country reach a threshold, approximately 40, their attack success ratio is steadily above .6
yhat <- ols$fitted
plot(shipping$`attacks/Year`, shipping$`success Ratio`, pch=19, xlab="x1", ylab="y")
abline(lm(shipping$`success Ratio`~shipping$`attacks/Year`),lwd=3, col="red")

#attempting fixed effects model - attack success on attacks per year
fixed <- plm(shipping$`success Ratio` ~ shipping$`attacks/Year`, data=shipping, index=c("country", "year"), model="within")
summary(fixed)

############################
# Increasing the complexity of our model
############################

# OLS Regression
# Regular OLS regression does not consider heterogeneity across groups or time.
# In this simple model, the number of attacks has a slightly negative relationship with attack success, however it is not stat. sig. 
# Model 2 - Success ratio on attacks/year + coast
ols2 <-lm(shipping$`success Ratio` ~ shipping$`attacks/Year` + shipping$`coast/Area ratio (m/km2)`, data=shipping)
summary(ols2)

yhat2 <- ols$fitted
plot(shipping$`coast/Area ratio (m/km2)`, shipping$`success Ratio`, pch=19, xlab="x1", ylab="y")
abline(lm(shipping$`success Ratio`~shipping$`coast/Area ratio (m/km2)`),lwd=3, col="red")

### Model 3 - Sucess rati on attacks/year + coast + GDP
ols3 <-lm(shipping$`success Ratio` ~ shipping$`attacks/Year` + shipping$`coast/Area ratio (m/km2)` + shipping$`GDP per cap`, data=shipping)
summary(ols3)

yhat3 <- ols$fitted 
plot(shipping$`GDP per cap`, shipping$`success Ratio`, pch=19, xlab="x1", ylab="y")
abline(lm(shipping$`success Ratio`~shipping$`GDP per cap`),lwd=3, col="red")

#####################
# Making a more complex FE model
######################

# FE model 2, Sucess Ratio on Attacks/Year + GDP per cap 
#attempting fixed effects model
fixed2 <- plm(shipping$`success Ratio` ~ shipping$`attacks/Year` + shipping$`GDP per cap`, data=shipping, index=c("country", "year"), model="within")
summary(fixed2)


#####################
# Regression Diagnostics
######################

#Assessing homoscedasticity (we have met the constant variance assumption if p < 1.95)
ncvTest(ols2)

#Assessing multicollinearity
vif(ols2)

#Assessing outliers
outlierTest(ols2)

