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
#attach(shipping)

# set working directories 
try(setwd("/Users/codykoebnick/Downloads/Data Set"))
try(setwd("E:/bjoer/Documents/Google Drive/Universität/Hertie/03_Fall 2015/05_Master Thesis/00_Piracy_2015-16/03_Data/Tennessee"))
getwd()
try(setwd("//Users/laurencehendry/GoogleDrive/Master Thesis - Shared/MPP-E1180 - Introduction to Collaborative Social Science Data Analysis")) 
getwd()

#import data
# empty cells are now coded with NA and can manually be excluded from any function with na.omit command
shipping <- read.csv("MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, na.strings = c("", "NA"))
# have a look at how the variables are created
str(shipping)

#renaming and dropping some of our columns 
names(shipping)[1] <- 'Country'
names(shipping)[2] <- 'year'
shipping$X1 = NULL
names(shipping)[3] <- 'Coast/Area ratio (m/km2)'
names(shipping)[4] <- 'GDP per cap'
names(shipping)[5] <- 'Attacks/Year'
names(shipping)[6] <- 'Successful Attacks/Year'
names(shipping)[7] <- 'Success Ratio'


