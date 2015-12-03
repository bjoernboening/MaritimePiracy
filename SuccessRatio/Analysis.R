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
library(knitr)


# set working directories 
try(setwd("/Users/codykoebnick/Documents/MaritimePiracy"))
try(setwd("E:/bjoer/Documents/Google Drive/Universität/Hertie/03_Fall 2015/05_Master Thesis/00_Piracy_2015-16/03_Data/Tennessee"))
getwd()
try(setwd("/Users/laurencehendry/GitHub/MaritimePiracy")) 
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
# barplot for frequency of attacks by country

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
ols1 <-lm(shipping$`success Ratio` ~ shipping$`attacks/Year`, data=shipping)
summary(ols1)

# The below plot shows that after attacks in a certain country reach a threshold, approximately 40, their attack success ratio is steadily above .6
yhat1 <- ols$fitted
plot(shipping$`attacks/Year`, shipping$`success Ratio`, pch=19, xlab="x1", ylab="y")
abline(lm(shipping$`success Ratio`~shipping$`attacks/Year`),lwd=3, col="red")

#attempting fixed effects model - attack success on attacks per year
fixed1 <- plm(shipping$`success Ratio` ~ shipping$`attacks/Year`, data=shipping, index=c("country", "year"), model="within")
summary(fixed1)

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

#import data
# empty cells are now coded with NA and can manually be excluded from any function with na.omit command
ucdp.prio <- read.csv("124920_1ucdpprio-armed-conflict-dataset_v.4-2015.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))
# have a look at how the variables are created

names(ucdp.prio)[3] <- 'country'
names(ucdp.prio)[2] <- 'year'

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
str(shipping)

ucdp.prio <- ddply(ucdp.prio, .(country, year), numcolwise(sum))

ship.ucdp <- merge(shipping, ucdp.prio, by=c("country","year"), all.x=TRUE) 
remove(list=c("shipping"))
remove(list=c("ucdp.prio"))

ship.ucdp <- unique(ship.ucdp[ , 1:8 ] )

ship.ucdp$IntensityLevel[is.na(ship.ucdp$IntensityLevel)] <- 0 

#Histogram of conflict frequnecy in our countries
hist(ship.ucdp$IntensityLevel)

#histogram of success ratio over intensity level 
plot(ship.ucdp$IntensityLevel, ship.ucdp$`success Ratio`)

scatterplot(ship.ucdp$`IntensityLevel` ~ year|country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=ship.ucdp)

#######################
#adding military expenditure numbers
######################
#import data
# empty cells are now coded with NA and can manually be excluded from any function with na.omit command
Military <- read.csv("Military.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, na.strings = c("", "NA"))
# have a look at how the variables are created

names(Military)[1] <- 'country'
names(Military)[2] <- '1992'
names(Military)[3] <- '1993'
names(Military)[4] <- '1994'
names(Military)[5] <- '1995'
names(Military)[6] <- '1996'
names(Military)[7] <- '1997'
names(Military)[8] <- '1998'
names(Military)[9] <- '1999'
names(Military)[10] <- '2000'
names(Military)[11] <- '2001'
names(Military)[12] <- '2002'
names(Military)[13] <- '2003'
names(Military)[14] <- '2004'
names(Military)[15] <- '2005'
names(Military)[16] <- '2006'
names(Military)[17] <- '2007'
names(Military)[18] <- '2008'
names(Military)[19] <- '2009'
names(Military)[20] <- '2010'
names(Military)[21] <- '2011'
names(Military)[22] <- '2012'
names(Military)[23] <- '2013'
names(Military)[24] <- '2014'


library(reshape)
x2 <- melt(Military,id=c("country"),variable_name="Year")
x2[,"Year"] <- as.numeric(gsub("X","",x2[,"Year"]))

names(x2)[2] <- 'year'
names(x2)[3] <- 'Military Expenditure'

ship.ucdp.mil <- merge(ship.ucdp, x2, by=c("country","year"), all.x=TRUE) 
remove(list=c("Military"))
remove(list=c("x2"))
remove(list=c("ship.ucdp"))


#Descriptive Statistics
#######################
# barplot for frequency of attacks by country

#histogram of attack success over GDP per cap
plot(ship.ucdp.mil$IntensityLevel, ship.ucdp.mil$`Military Expenditure`)
plot(ship.ucdp.mil$`Military Expenditure`, ship.ucdp.mil$`success Ratio`)
plot(ship.ucdp.mil$`Military Expenditure`, ship.ucdp.mil$`attacks/Year`)

plot(ship.ucdp.mil$IntensityLevel, ship.ucdp.mil$`success Ratio`)
plot(ship.ucdp.mil$IntensityLevel, ship.ucdp.mil$`attacks/Year`)

###############
## Inferential Statistics
###############

ols10 <-lm(ship.ucdp.mil$`attacks/Year` ~ ship.ucdp.mil$`coast/Area ratio (m/km2)` + ship.ucdp.mil$`GDP per cap` + ship.ucdp.mil$`Military Expenditure`, data=ship.ucdp.mil)
summary(ols10)

ols11 <-lm(ship.ucdp.mil$`success Ratio` ~ ship.ucdp.mil$`coast/Area ratio (m/km2)` + ship.ucdp.mil$`GDP per cap` + ship.ucdp.mil$`Military Expenditure`, data=ship.ucdp.mil)
summary(ols11)

ols4 <-lm(ship.ucdp.mil$`success Ratio` ~ ship.ucdp.mil$`attacks/Year`, data=ship.ucdp.mil)
summary(ols4)

ols5 <-lm(ship.ucdp.mil$`success Ratio` ~ ship.ucdp.mil$IntensityLevel, data=ship.ucdp.mil)
summary(ols5)

ols6 <- lm(ship.ucdp.mil$`success Ratio` ~ ship.ucdp.mil$`Military Expenditure`, data = ship.ucdp.mil, na.action = na.omit)
summary(ols6)

#this shows that military expenditure does not have a sig relationship with the success of an attack, but conflict intensity does. 
ols7 <- lm(ship.ucdp.mil$`success Ratio` ~ ship.ucdp.mil$`Military Expenditure` + ship.ucdp.mil$IntensityLevel, data = ship.ucdp.mil, na.action = na.omit)
summary(ols7)

#this shows that military expenditure does have a sig reelationship with NUMBER of attacks, as does intensity levels. 
ols8 <- lm(ship.ucdp.mil$`attacks/Year` ~ ship.ucdp.mil$`Military Expenditure` + ship.ucdp.mil$IntensityLevel, data = ship.ucdp.mil, na.action = na.omit)
summary(ols8)

# The below plot shows that after attacks in a certain country reach a threshold, approximately 40, their attack success ratio is steadily above .6
plot(ship.ucdp.mil$`Military Expenditure`, ship.ucdp.mil$`attacks/Year`, pch=19, xlab="Military Expenditure", ylab="Attacks per Year")

fixed3 <- plm(ship.ucdp.mil$`success Ratio` ~ ship.ucdp.mil$IntensityLevel, data=ship.ucdp.mil, index=c("country", "year"), model="within",)
summary(fixed3)

fixed4 <- plm(ship.ucdp.mil$`success Ratio` ~ ship.ucdp.mil$IntensityLevel + ship.ucdp.mil$`Military Expenditure`, data=ship.ucdp.mil, index=c("country", "year"), model="within",)
summary(fixed4)

fixed5 <- plm(ship.ucdp.mil$`attacks/Year` ~ ship.ucdp.mil$IntensityLevel, data=ship.ucdp.mil, index=c("country", "year"), model="within",)
summary(fixed5)

fixed6 <- plm(ship.ucdp.mil$`attacks/Year` ~ ship.ucdp.mil$IntensityLevel + ship.ucdp.mil$`Military Expenditure`, data=ship.ucdp.mil, index=c("country", "year"), model="within",)
summary(fixed6)


plotmeans(ship.ucdp.mil$`attacks/Year` ~ ship.ucdp.mil$IntensityLevel, main="Heterogeineity across years", data=ship.ucdp.mil)
plotmeans(ship.ucdp.mil$`success Ratio` ~ ship.ucdp.mil$IntensityLevel, main="Heterogeineity across years", data=ship.ucdp.mil)

plotmeans(ship.ucdp.mil$`attacks/Year` ~ ship.ucdp.mil$`Military Expenditure`, main="Heterogeineity across years", data=ship.ucdp.mil)
plotmeans(ship.ucdp.mil$`success Ratio` ~ ship.ucdp.mil$`Military Expenditure`, main="Heterogeineity across years", data=ship.ucdp.mil)


fixed8 <- plm(ship.ucdp.mil$`attacks/Year` ~ ship.ucdp.mil$`Military Expenditure` + ship.ucdp.mil$`GDP per cap` + ship.ucdp.mil$country, data=ship.ucdp.mil, index=c("country", "year"), model="within",)
summary(fixed8)



