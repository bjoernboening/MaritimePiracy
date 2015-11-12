#########################################################
#### Maritime Piracy Data Analysis ######################
#########################################################
#### by Laurence Hendry, Cody Koebnick, and Björn Boening

# Import the dataset about piracy attacks into your wd 
  # Call libraries we need for the project, make sure you have them installed
library(rio) # swiss army knife for imports
library(plyr) # count occurences
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(ggplot2) # nice plots
library(stargazer) # nicer regression output which looks like a real publication
library(car) # scatterplots 
library(httr) # scraping from http sites
library(XML) # Tool for generating XML file
#attach(shipping)

# set working directories 
try(setwd("/Users/codykoebnick/Downloads/Data Set"))
try(setwd("E:/bjoer/Documents/Google Drive/Universität/Hertie/03_Fall 2015/05_Master Thesis/00_Piracy_2015-16/03_Data/Tennessee"))
getwd()
try(setwd("//Users/laurencehendry/GoogleDrive/Master Thesis - Shared/MPP-E1180 - Introduction to Collaborative Social Science Data Analysis")) 
getwd()

#import data
  # empty cells are now coded with NA and can manually be excluded from any function with na.omit command
shipping <- read.csv("MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE, na.strings = c("", "NA"))
  # have a look at how the variables are created
str(shipping)
  # creae unique identifier country year
  # GET external source data

# combine date into one column
unite(shipping, "date", c("year", "month", "day"), sep = "-")

######################################
# Laurence's data scraping - add a variable on country coastline length web-scraped from wikipedia
#####################################

# scraping 
URL <- 'https://en.wikipedia.org/wiki/List_of_countries_by_length_of_coastline'

tables <- URL %>% GET() %>%
  content(as = 'parsed') %>% 
  readHTMLTable()

names(tables)

CoastlineTable <- tables[[1]]

head(CoastlineTable)[, 1:3]

#cleaning the scrape for merge
CoastlineTable$V2 = NULL
CoastlineTable$V3 = NULL
CoastlineTable$V4 = NULL
CoastlineTable$V5 = NULL
CoastlineTable$V6 = NULL
CoastlineTable$V8 = NULL

#renaming, part of scrape
colnames(CoastlineTable)
names(CoastlineTable)
names(CoastlineTable)[1] <- 'closest_coastal_state'
names(CoastlineTable)[2] <- 'Coast/Area ratio (m/km2)'

#merging
#p297 from R for Dummies
allmerge <- merge(shipping, CoastlineTable, all.x=TRUE)

#######################################
## Descriptive Statistics
#######################################
# count countries = 108 total and 13 NAs
count(shipping$closest_coastal_state)


#attempting a probit regression 
shipping$timeofdayrecode <- factor(shipping$timeofdayrecode)
shipping$vessel_type <- factor(shipping$vessel_type)
shipping$vessel_status <- factor(shipping$vessel_status)
shipping$Incident_type_recode <- factor(shipping$Incident_type_recode)

xtabs(~ timeofdayrecode + Incident_type_recode, data=shipping)

myprobit <- glm(Incident_type_recode ~ timeofdayrecode + vessel_type + vessel_status, family=binomial(link="probit"), data=shipping)
summary (myprobit)


#estimate model
logit1 <- glm(notyemen ~ as.factor(shiptype) + as.factor(shipcategory), data = shipping, familiy = "binominal")
lm(logit1)

source()
###############################
### End of script
