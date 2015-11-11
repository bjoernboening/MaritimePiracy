# Import the data about piracy attacks into 
  # Libraries we need for the project, make sure you have them installed
library(rio)
library(dplyr)
library(tidyr)
library(ggplot2)
attach(shipping)

# set working directories 
try(setwd("/Users/codykoebnick/Downloads/Data Set"))
try(setwd("E:/bjoer/Documents/Google Drive/Universität/Hertie/03_Fall 2015/05_Master Thesis/00_Piracy_2015-16/03_Data/Tennessee"))
getwd()

#import data with the rio package the swiss army knife
  # empty cells are now coded with NA and can manually be excluded from any function with na.omit command
shipping <- read.csv("MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE, na.strings = c("", "NA"))
  # have a look at how the variables are created
str(shipping)

# combine date into one column
unite(shipping, "date", c("year", "month", "day"), sep = "-")


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

## End of code