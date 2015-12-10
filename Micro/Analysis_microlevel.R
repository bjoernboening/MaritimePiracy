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
#IMPORT
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

######
#DATA WRANGLING
######

#renaming and recoding
names(sub)[1] <- 'year'

names(sub)[2] <- 'time'
sub$time <- factor(sub$time,
                    levels = c(1,2,3,4),
                    labels = c("early", "day", "evening", "night"))
sub$time <- factor(sub$time)

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
sub$type[sub$type==-99] <- NA
sub$type[sub$type==10] <- NA
sub$type[sub$type==22] <- NA
sub$type[sub$type==696] <- NA
sub$type <- factor(sub$type,
                   levels = c(1,2),
                   labels = c("big", "small"))
sub$type <- factor(sub$type)

names(sub)[5] <- 'incident'
sub$incident[sub$incident==-99] <- NA
sub$incident <- factor(sub$incident,
                   levels = c(0,1),
                   labels = c("attempted", "acutal"))
sub$incident <- factor(sub$incident)

names(sub)[6] <- 'stat'
sub$stat[sub$stat==-99] <- NA
sub$stat <- factor(sub$stat,
                    levels = c(1,2,3,4))
sub$status <- recode(sub$stat, "c(1)='1'; c(2,3,4)='2'") # what a bastard this line was arrgg
sub$status <- factor(sub$status,
                   levels = c(1,2),
                   labels = c("moving", "stationary"))
sub$status <- factor(sub$status)
sub$stat = NULL

# Delete missing values through listwise deletion
# might need revision for the analysis
sub <- na.omit(sub)

######
#DESCRIPTIVE STATS
######

# barplot for frequency of attacks
a <- ggplot(data=na.omit(sub), aes(x=incident))
a + geom_bar(stat="bin") +
  xlab("Incidents") +
  ylab("Frequency") +
  ggtitle("Frequency of Attacks")

#barcharts yaxis indicidents and xaxis for predictors
b <- ggplot(sub, aes(type, ..count..)) 
b + geom_bar(aes(fill = incident), position = "dodge") +
  xlab("Type") + 
  ylab("Incidents") + 
  ggtitle("Frequency of Attacks on different ship types")

c <- ggplot(sub, aes(time, ..count..)) 
c + geom_bar(aes(fill = incident), position = "dodge") +
  xlab("Time") + 
  ylab("Incidents") + 
  ggtitle("Frequency of Attacks with time of day")

d <- ggplot(sub, aes(status, ..count..)) 
d + geom_bar(aes(fill = incident), position = "dodge") +
  xlab("Status") + 
  ylab("Incidents") + 
  ggtitle("Frequency of Attacks with ship status")

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
m1 <- glm(incident ~ type,  family=binomial(link='logit'),data=sub)
summary(m1)
stargazer(m1, digits = 2,  title="Regression Results", align=TRUE, type = "html")

m2 <- glm(incident ~ time, family=binomial(link='logit'),data=sub)
summary(m2)
stargazer(model, digits = 2,  title="Regression Results", align=TRUE, type = "html")

######
#CAVEATS
######
# show distribution of NAs in all variables in the data frame
missmap(sub, main = "Too much NAs?")
