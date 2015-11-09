# Import the data about piracy attacks into 
  # Libraries we need for the project, make sure you have them installed
library(rio)
library(aod)


# set working directories 
try(setwd("/Users/codykoebnick/Downloads/Data Set"))
try(setwd("E:/bjoer/Documents/Google Drive/Universit?t/Hertie/03_Fall 2015/05_Master Thesis/00_Piracy_2015-16/03_Data/Traxler"))
getwd()

#import data with the rio package the swiss army knife
library(rio)
shipping <- import('all_shipdata_update.dta')
  # Cody's import craziness
##install.packages("haven")

summary(shipping)
# probit attempt
shipping$shipcategory <- factor(shipping$shipcategory)
shipping$flag <- factor(shipping$flag)
shipping$region <- factor(shipping$region)

xtabs(~ shipcategory + att_suc, data = shipping)

myprobit <- glm(att_suc ~ shipcategory, family=binomial(link="probit"), data=shipping)

## model summary
summary(myprobit)

#estimate model
logit1 <- glm(notyemen ~ as.factor(shiptype) + as.factor(shipcategory), data = shipping, familiy = "binominal")
lm(logit1)



library(knitr)
kable(fitted)
