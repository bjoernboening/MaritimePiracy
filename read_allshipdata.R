# Import the data about piracy attacks into 
  # Libraries we need for the project, make sure you have them installed
library(rio)
library(haven)
library(dplyr)

# set working directories 
try(setwd("/Users/codykoebnick/Downloads/Data Set"))
try(setwd("E:/bjoer/Documents/Google Drive/Universität/Hertie/03_Fall 2015/05_Master Thesis/00_Piracy_2015-16/03_Data/Tennessee"))
getwd()

#import data with the rio package the swiss army knife
shipping <- read.csv("MaritimePiracyTennessee.csv", header = TRUE, sep = ";")
  # Cody's import craziness
  # install.packages("haven")
# try to remove missing values from incident_type, so far no success
attacks <- shipping %>% filter(!is.na(incident_type) %>% droplevels()
str(attacks)

# Basic descriptive statistics 


#estimate model
logit1 <- glm(notyemen ~ as.factor(shiptype) + as.factor(shipcategory), data = shipping, familiy = "binominal")
lm(logit1)

## End of code