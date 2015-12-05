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
library(Amelia) # map missing values 

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
sub$type <- factor(sub$type)

names(sub)[5] <- 'incident'
sub$incident <- factor(sub$incident)

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
#sub$time <- na.omit(sub$time)
#sub$status <- na.omit(sub$status)
#sub$type <- na.omit(sub$type)
#sub$incident <- na.omit(sub$incident)

######
#DESCRIPTIVE STATS
######

# barplot for frequency of attacks by country
suc_ratio <- ggplot(na.omit(sub))
suc_ratio + aes(factor(incident)) + geom_bar()
suc_ratio <- geom_bar()
#suc_ratio + labs(title = "Success Ratio of Piracy Attacks")
#suc_ratio + ylabs("Total Frequency from 1993 to 2014")
#suc_ratio + xlab("0=attempted 1=actual")
#chi <- table(sub$incident)
#chisq.test(na.omit(sub)$type, na.omit(sub)$incident)
#data <- structure(list(W= c(399L, 82L, 29L), 
 #                      X = c(370L, 100L, 25L)), 
  #                .Names = c("Female", "Male"), class = "data.frame", row.names = c(NA, -3L))
#attach(data)
#print(data)
#barplot(as.matrix(chi), main="Frequency plots", ylim = c(0,4000), ylab = "Total Frequency", xlab = "1=type 2=blab", cex.lab = 1.2, cex.main = 1.4, beside=TRUE)
#legend("topright", c("One workshop", "Two workshops"), cex=0.7, bty="n", fill=colours)

#Histogram of ship type
hist(sub$type, xlab = "ship types", main = paste("Histogram of type"))
#Histogram of successful attacks per year
hist(sub$time)
######
#TABLES
######

# ship type (factor)
tab <- table(na.omit(sub)$incident,na.omit(sub)$type) # A will be rows, B will be columns 
tab # print table 
margin.table(tab, 1) # A frequencies (summed over B) 
margin.table(tab, 2) # B frequencies (summed over A)
prop.table(tab) # cell percentages
prop.table(tab, 1) # row percentages !
prop.table(tab, 2) # column percentages

# ship status (factor)
tab1 <- table(na.omit(sub)$incident,na.omit(sub)$status) # A will be rows, B will be columns 
tab1 # print table 
margin.table(tab1, 1) # A frequencies (summed over B) 
margin.table(tab1, 2) # B frequencies (summed over A)
prop.table(tab1) # cell percentages
prop.table(tab1, 1) # row percentages !
prop.table(tab1, 2) # column percentages

# time of day (factor)
tab2 <- table(na.omit(sub)$incident,na.omit(sub)$time) # A will be rows, B will be columns 
tab2 # print table 
margin.table(tab2, 1) # A frequencies (summed over B) 
margin.table(tab2, 2) # B frequencies (summed over A)
prop.table(tab2) # cell percentages
prop.table(tab2, 1) # row percentages !
prop.table(tab2, 2) # column percentages

#stargazer(tab, tab1, tab2, type = "html", out="tabels.htm")

######
#ANALYSIS
######

#loged odds ratio - logistic regression "logit"
model <- glm(incident ~ type,  family=binomial(link='logit'),data=sub)
summary(model)
stargazer(model, digits = 2,  title="Regression Results", align=TRUE, type = "html")

######
#CAVEATS
######

missmap(sub, main = "Too much NAs?")
