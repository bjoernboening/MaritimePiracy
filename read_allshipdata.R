#########################################################
#### Maritime Piracy Data Analysis ######################
#########################################################
#### by Laurence Hendry, Cody Koebnick, and Bj?rn Boening

# Import the dataset about piracy attacks into your wd 
  # Call libraries we need for the project, make sure you have them installed
library(rio) # swiss army knife for imports
library(plyr) # count occurences
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(ggplot2) # nice plots
library(stargazer) # nicer regression output which looks like a real publication
library(car) # scatterplots 
library(WDI) # Scraping Data from the World Bank 
library(countrycode)
#attach(shipping)

# set working directories 
try(setwd("/Users/codykoebnick/Downloads/Data Set"))
try(setwd("E:/bjoer/Documents/Google Drive/Universit?t/Hertie/03_Fall 2015/05_Master Thesis/00_Piracy_2015-16/03_Data/Tennessee"))
getwd()

#import data
  # empty cells are now coded with NA and can manually be excluded from any function with na.omit command
shipping <- read.csv("MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE, na.strings = c("", "NA"))
  # have a look at how the variables are created
str(shipping)
  # creae unique identifier country year
  # GET external source data

# combine date into one column
#this aint working yo
unite(shipping, "date", c("year", "month", "day"), sep = "-")


######################################
#Scraping Data
######################################

###Grab GDP per capita data for our 10 key countries

# Define a list of countries for which to pull data (the following should be a complete list of countries.  We can delete this later, but please keep it for now - to add Iran Iraq Italy Ivory  Jamaica Japan Kazakhstan Kenya Liberia Madagascar Malaysia Maldives Malta Mauritania Mauritius Mayotte Mexico Morocco Mozambique Myanmar Netherlands Nigeria Oman Pakistan Panama Papua New Guinea Paracel Islands Peru Philippines Portugal Russia Sao Tome & Principe Saudi Arabia Senegal Seychelles Sierra Leone Singapore Solomon Is. Somalia South Africa South Korea Spratly Islands Sri Lanka St. Lucia Sudan Suriname Taiwan Tanzania Thailand The Congo The Gambia Togo Trinidad & Tobago Turkey United Arab Emirates United Kingdom United States Uruguay Venezuela  Vietnam Yemen)
countries <- c("Indonesia", "Yemen", "Malaysia", "Bangladesh", "Nigeria", "India", "Somalia", "Philippines", "Vietnam", "Brazil")

# Convert the country names to iso2c format used in the World Bank data
iso2cNames <- countrycode(countries, "country.name", "iso2c")

#actual scraping and creating a new dataset 
wdiData2 <- WDI(iso2cNames, indicator='NY.GDP.PCAP.PP.CD', start=1994, end=2014)
#attempt at getting rid of the first columm.  Doesn't work. 
wdiData2$iso2c = NULL

#######################################
#Merging Data
#######################################
names(wdiData2)[1] <- 'closest_coastal_state'
total <- merge(shipping,wdiData2,by=c("closest_coastal_state","year"))


#######################################
## Descriptive Statistics
#######################################
# count countries = 108 total and 13 NAs
count(shipping$closest_coastal_state)
sort(table(shipping$closest_coastal_state), decreasing = TRUE)

#Ploting the GDP per capita change overtime in our 10 countries.
ggplot(wdiData2, aes(year, NY.GDP.PCAP.PP.CD, color=country)) + geom_line() + 
  xlab('Year') + ylab('GDP per capita')

####Creating a new variable for frequency count of attacks per country per year
#data.frame ( table ( data$Group, data$Size ) )
CountYrCtry <- table (shipping$year, shipping$closest_coastal_state)
CountYrCtry
shipping$CountYrCtryVar2  <- table (shipping$year, shipping$closest_coastal_state)


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