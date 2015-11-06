#######session 06###

#testing 
#cars <- mtcars
#ols <- lm(mpg ~ hp + am, family = "binominal") # family is the stochastic component and hp am is the 
#lm(ols)

# OLS regression with allshipdatar
#library(foreign)
#setwd("E:/bjoer/Documents/Google Drive/Universität/Hertie/03_Fall 2015/05_Master Thesis/03_Data/Traxler/Daten Traxler")
#read.dta(all_shipdata_update.dta, convert.dates = TRUE, convert.factors = TRUE,
#         missing.type = FALSE,
#         convert.underscore = FALSE, warn.missing.labels = TRUE)

library(rio)
setwd("E:/bjoer/Documents/Google Drive/Universität/Hertie/03_Fall 2015/05_Master Thesis/00_Piracy_2015-16/03_Data/Traxler")
shipping <- import('all_shipdata_update.dta')

#estimate model
logit1 <- glm(notyemen ~ as.factor(shiptype) + as.factor(shipcategory), data = shipping, familiy = "binominal")
lm(logit1)
library(knitr)


kable(fitted)