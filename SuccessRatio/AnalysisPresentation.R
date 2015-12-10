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
library(stargazer)


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

hist(shipping$`attacks/Year`, main='Frequency of Attacks per Year in Bins', xlab='Number of Attacks per Year', ylab='Frequency')

plotmeans(shipping$`success Ratio` ~ country, main="Average Success Ratio of Pirates per Country", data=shipping, xlab="Country", ylab="Success Ratio")

plotmeans(shipping$`success Ratio` ~ year, main="Average Success Ratio of Pirates per Year", data=shipping, xlab="Year", ylab="Success Ratio")

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

#histogram of attack success over GDP per cap
plot(ship.ucdp.mil$IntensityLevel, ship.ucdp.mil$`Military Expenditure`, main='Visualizing the Correlation Between Armed Conflict and Military Expenditures', xlab='Conflict Intensity', ylab='Military Expenditures')

plot(ship.ucdp.mil$`Military Expenditure`, ship.ucdp.mil$`attacks/Year`, main='Visualizing the affect of Military Expenditure on Piracy Rates', xlab='Military Expenditures', ylab='Number of Attacks per Year')

###############
## Inferential Statistics
###############

ols1 <-lm(ship.ucdp.mil$`attacks/Year` ~ ship.ucdp.mil$IntensityLevel, data=ship.ucdp.mil)
summary(ols1)

ols2 <-lm(ship.ucdp.mil$`attacks/Year` ~ ship.ucdp.mil$`Military Expenditure`, data=ship.ucdp.mil)
summary(ols2)

ols3 <-lm(ship.ucdp.mil$`attacks/Year` ~ship.ucdp.mil$`Military Expenditure` + ship.ucdp.mil$`coast/Area ratio (m/km2)`, data=ship.ucdp.mil)
summary(ols3)

ols4 <-lm(ship.ucdp.mil$`attacks/Year` ~ ship.ucdp.mil$`Military Expenditure` + ship.ucdp.mil$`coast/Area ratio (m/km2)` + ship.ucdp.mil$`GDP per cap`, data=ship.ucdp.mil)
summary(ols4)

fixed1 <- plm(ship.ucdp.mil$`attacks/Year` ~ ship.ucdp.mil$`Military Expenditure`x, data=ship.ucdp.mil, index=c("country", "year"), model="within",)
summary(fixed1)

fixed2 <- plm(ship.ucdp.mil$`attacks/Year` ~ ship.ucdp.mil$`Military Expenditure` + ship.ucdp.mil$`GDP per cap`, data=ship.ucdp.mil, index=c("country", "year"), model="within",)
summary(fixed2)

stargazer(ols1, ols2, ols3, ols4, fixed1, fixed2, type = "html")


