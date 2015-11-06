# Import the data about piracy attacks into 

# set working directories 
try(setwd("cody"))
try(setwd("bjorns"))

#import data with the rio package the swiss army knife
library(rio)
setwd("E:/bjoer/Documents/Google Drive/Universit?t/Hertie/03_Fall 2015/05_Master Thesis/00_Piracy_2015-16/03_Data/Traxler")
shipping <- import('all_shipdata_update.dta')

#estimate model
logit1 <- glm(notyemen ~ as.factor(shiptype) + as.factor(shipcategory), data = shipping, familiy = "binominal")
lm(logit1)
library(knitr)


kable(fitted)