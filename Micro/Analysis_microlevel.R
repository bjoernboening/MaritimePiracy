#########################################################
#### Maritime Piracy Data Analysis ######################
#########################################################
#### by Laurence Hendry, Cody Koebnick, and BjÃ¶rn Boening
#### Micro Level

# We decided to determine what drives maritime piracy on different levels.
# 
# Import the dataset about piracy attacks into your environment 
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
library(knitr)

# set working directories 
try(setwd("/Users/codykoebnick/Documents/MaritimePiracy"))
try(setwd("E:/bjoer/Documents/GitHub/MaritimePiracy"))
try(setwd("/Users/laurencehendry/GitHub/MaritimePiracy")) 
getwd()

#import data
# empty cells are now coded with NA and can manually be excluded from any function with na.omit command
micro <- read.csv("MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = c("", "NA"))
# have a look at how the variables are created
str(micro)

#renaming and dropping some of our columns 
names(micro)[1] <- 'country'
names(micro)[2] <- 'year'
micro$X1 = NULL
names(micro)[3] <- 'coast/Area ratio (m/km2)'
names(micro)[4] <- 'GDP per cap'
names(micro)[5] <- 'attacks/Year'
names(micro)[6] <- 'successful Attacks/Year'
names(micro)[7] <- 'success Ratio'

#######################
#Descriptive Statistics
#######################
# barplot for frequency of attacks by country

#Histogram of attack frequnecy
hist(micro$`attacks/Year`)
#Histogram of successful attacks per year
hist(micro$`successful Attacks/Year`)
#histogram of attack success over GDP per cap
plot(micro$`GDP per cap`, micro$`success Ratio`)

#exploring the data 
coplot(micro$`success Ratio` ~ year|country, type="l", data=micro) # Lines
coplot(micro$`success Ratio` ~ year|country, type="b", data=micro) # Points and lines

scatterplot(micro$`success Ratio` ~year|country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=micro)

# Fixed effects: Heterogeneity across countries (or entities)
# interestingly, attacks in Philipines have the lowest success ratio and attacks in Brazil have the highest.
# plotmeans draw a 95% confidence interval around the means
plotmeans(micro$`success Ratio` ~ country, main="Heterogeineity across countries", data=micro)

# Fixed effects: Heterogeneity across years
# plotmeans draw a 95% confidence interval around the means
plotmeans(micro$`success Ratio` ~ year, main="Heterogeineity across years", data=micro)

# OLS Regression
# Regular OLS regression does not consider heterogeneity across groups or time.
# In this simple model, the number of attacks has a slightly negative relationship with attack success, however it is not stat. sig. 
ols <-lm(micro$`success Ratio` ~ micro$`attacks/Year`, data=micro)
summary(ols)

# The below plot shows that after attacks in a certain country reach a threshold, approximately 40, their attack success ratio is steadily above .6
yhat <- ols$fitted
plot(micro$`attacks/Year`, micro$`success Ratio`, pch=19, xlab="x1", ylab="y")
abline(lm(micro$`success Ratio`~micro$`attacks/Year`),lwd=3, col="red")

#attempting fixed effects model - attack success on attacks per year
fixed <- plm(micro$`success Ratio` ~ micro$`attacks/Year`, data=micro, index=c("country", "year"), model="within")
summary(fixed)

############################
# Increasing the complexity of our model
############################

# OLS Regression
# Regular OLS regression does not consider heterogeneity across groups or time.
# In this simple model, the number of attacks has a slightly negative relationship with attack success, however it is not stat. sig. 
# Model 2 - Success ratio on attacks/year + coast
ols2 <-lm(micro$`success Ratio` ~ micro$`attacks/Year` + micro$`coast/Area ratio (m/km2)`, data=micro)
summary(ols2)

yhat2 <- ols$fitted
plot(micro$`coast/Area ratio (m/km2)`, micro$`success Ratio`, pch=19, xlab="x1", ylab="y")
abline(lm(micro$`success Ratio`~micro$`coast/Area ratio (m/km2)`),lwd=3, col="red")

### Model 3 - Sucess rati on attacks/year + coast + GDP
ols3 <-lm(micro$`success Ratio` ~ micro$`attacks/Year` + micro$`coast/Area ratio (m/km2)` + micro$`GDP per cap`, data=micro)
summary(ols3)

yhat3 <- ols$fitted 
plot(micro$`GDP per cap`, micro$`success Ratio`, pch=19, xlab="x1", ylab="y")
abline(lm(micro$`success Ratio`~micro$`GDP per cap`),lwd=3, col="red")

#####################
# Making a more complex FE model
######################

# FE model 2, Sucess Ratio on Attacks/Year + GDP per cap 
#attempting fixed effects model
fixed2 <- plm(micro$`success Ratio` ~ micro$`attacks/Year` + micro$`GDP per cap`, data=micro, index=c("country", "year"), model="within")
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

############
#manually loaded the UCDP.Prio dataset for conflicts
###########

names(ucdp.prio)[3] <- 'country'
names(ucdp.prio)[1] <- 'year'

ucdp.prio$ConflictId <- NULL
ucdp.prio$SideA <- NULL
ucdp.prio$SideA2nd <- NULL
ucdp.prio$SideB <- NULL
ucdp.prio$SideBID <- NULL
ucdp.prio$SideB2nd <- NULL
ucdp.prio$Incompatibility <- NULL
ucdp.prio$TerritoryName <- NULL
ucdp.prio$CumulativeIntensity <- NULL
ucdp.prio$TypeOfConflict <- NULL
ucdp.prio$StartDate <- NULL
ucdp.prio$StartPrec <- NULL
ucdp.prio$StartDate2 <- NULL
ucdp.prio$StartPrec2 <- NULL
ucdp.prio$EpEnd <- NULL
ucdp.prio$EpEndDate <- NULL
ucdp.prio$EpEndPrec <- NULL
ucdp.prio$GWNoA <- NULL
ucdp.prio$GWNoA2nd <- NULL
ucdp.prio$GWNoB <- NULL
ucdp.prio$GWNoB2nd <- NULL
ucdp.prio$GWNoLoc <- NULL
ucdp.prio$Region <- NULL
ucdp.prio$Version <- NULL

MergedData <- merge 
merged.data <- merge(micro, ucdp.prio, by=c("country", "year"))
mydata <- merge(micro, ucdp.prio, by=c("country","year"), all.x=TRUE) 

deduped.data <- unique( mydata[ , 1:8 ] )
deduped.data2 <- unique( mydata[ , 1:7 ] )
