#########################################################
#### Maritime Piracy Data Analysis ######################
#########################################################
#### by Laurence Hendry, Cody Koebnick, and Bjoern Boening
#### Micro Level

# We decided to determine what drives maritime piracy on different levels.
# This script analyses on a micro level, one row one piracy attack

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
library(Hmisc) # variable labels


######
#IMPORT & DATA WRANGLING
######
# set working directories 
try(setwd("/Users/codykoebnick/Documents/MaritimePiracy"))
try(setwd("E:/bjoer/Documents/GitHub/MaritimePiracy"))
try(setwd("/Users/laurencehendry/GitHub/MaritimePiracy")) 
getwd()
#import data
# empty cells are coded with NA and can manually be excluded from any function with na.omit command
micro <- read.csv("MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = c("", "NA"))
# have a look at how the variables are created
str(micro)
#subsetting (keep) variables
sub <- micro[c(4, 6, 12, 18, 24, 23)]

#renaming and recoding
names(sub)[1] <- 'year'

names(sub)[2] <- 'time'
sub$time <- factor(sub$time,
                    levels = c(1,2,3,4))

names(sub)[3] <- 'state'

names(sub)[4] <- 'type'
sub$type[sub$type==1] <- 222
sub$type[sub$type==5] <- 222
sub$type[sub$type==9] <- 222
sub$type[sub$type==2] <- 111
sub$type[sub$type==3] <- 111
sub$type[sub$type==4] <- 111
sub$type[sub$type==6] <- 111
sub$type[sub$type==7] <- 111
sub$type[sub$type==8] <- 111
sub$type[sub$type==111] <- 1
sub$type[sub$type==222] <- 2

names(sub)[5] <- 'incident'

names(sub)[6] <- 'stat'
sub$stat <- factor(sub$stat,
                    levels = c(1,2,3,4))
sub$status <- recode(sub$stat, "c(1)='1'; c(2,3,4)='2'") # what a bastard this line was arrgg
sub$stat = NULL

# Delete missing values
table(sub$year, useNA = "always")
table(sub$time, useNA = "always")
sub$time[sub$time==-99] <- NA
table(sub$status, useNA = "always")
sub$status[sub$status==-99] <- NA
table(sub$type, useNA = "always")
sub$type[sub$type==-99] <- NA
sub$type[sub$type==22] <- NA
sub$type[sub$type==696] <- NA
sub$type[sub$type==10] <- NA
table(sub$incident, useNA = "always")
sub$incident[sub$incident==-99] <- NA
table(sub$state, useNA = "always")
# Omit NAs # not working must create a new data frame
sub$time <- na.omit(sub$time)
sub$status <- na.omit(sub$status)
sub$type <- na.omit(sub$type)
sub$incident <- na.omit(sub$incident)

######
#DESCRIPTIVE STATS
######
# barplot for frequency of attacks by country
#suc_ratio <- ggplot(na.omit(sub)
#suc_ratio + aes(factor(incident)) + geom_bar()
#suc_ratio <- geom_bar()
#suc_ratio + labs(title = "Success Ratio of Piracy Attacks")
#suc_ratio + ylabs("Total Frequency from 1993 to 2014")
#suc_ratio + xlab("0=attempted 1=actual")
#chi <- table(sub$incident, sub$type)
#prop.table(na.omit(sub)$type)
#chisq.test(na.omit(sub)$type)
#data <- structure(list(W= c(399L, 82L, 29L), 
 #                      X = c(370L, 100L, 25L)), 
  #                .Names = c("Female", "Male"), class = "data.frame", row.names = c(NA, -3L))
#attach(data)
#print(data)

#barplot(as.matrix(data), ylim=c(0,400), main="Workshop reach by sex 2014", ylab = "Numbers of Workshops", cex.lab = 1.2, cex.main = 1.4, beside=TRUE)
#legend("topright", c("One workshop", "Two workshops", "Three workshops"), cex=0.7, bty="n", fill=colours)

#Histogram of ship type
hist(sub$type, xlab = "ship types", main = paste("Histogram of type"))
#Histogram of successful attacks per year
hist(sub$time)
######
#TABLES
######
#table row percentages of type (numeric)
tab <- table(na.omit(sub)$incident,na.omit(sub)$type) # A will be rows, B will be columns 
tab # print table 
margin.table(tab, 1) # A frequencies (summed over B) 
margin.table(tab, 2) # B frequencies (summed over A)
prop.table(tab) # cell percentages
prop.table(tab, 1) # row percentages !
prop.table(tab, 2) # column percentages
#table of status (factor)
tab1 <- table(na.omit(sub)$incident,na.omit(sub)$status) # A will be rows, B will be columns 
tab1 # print table 
margin.table(tab1, 1) # A frequencies (summed over B) 
margin.table(tab1, 2) # B frequencies (summed over A)
prop.table(tab1) # cell percentages
prop.table(tab1, 1) # row percentages !
prop.table(tab1, 2) # column percentages
#table of time (factor)
tab2 <- table(na.omit(sub)$incident,na.omit(sub)$time) # A will be rows, B will be columns 
tab2 # print table 
margin.table(tab2, 1) # A frequencies (summed over B) 
margin.table(tab2, 2) # B frequencies (summed over A)
prop.table(tab2) # cell percentages
prop.table(tab2, 1) # row percentages !
prop.table(tab2, 2) # column percentages


# 3-Way Frequency Table
tab2 <- xtabs(~incident+type, data=sub)
ftable(tab2) # print table 
summary(tab2) # chi-square test of indepedence

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

