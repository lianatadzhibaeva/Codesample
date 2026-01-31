library("AER")
library("sandwich")
library("lmtest")
library("car")
library("stargazer")
library("ggplot2")
library("openintro")
library("doBy")
library("plm") #Панельные данные -- основной пакет для сегодняшнего занятия
library(dplyr)
library(devtools)
library(pdynmc)
library(readxl)
library('xtable')
library('robust')
library(knitr)

#########################################################

# function to calculate corrected SEs for OLS regression 
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

# clustered SEs, clustered on "group"... could also cluster on "time" 
# compute Stata-like degrees of freedom adjustment for number of groups
# See http://www.richard-bluhm.com/clustered-ses-in-r-and-stata-2/

clse = function(reg) { 
  # index(reg, "id") returns the id or entity variable vector 
  G = length(unique(index(reg,"id")))
  N = length(index(reg,"id"))
  dfa = (G/(G - 1))   # note Bluhm multiplies this by finite-sample df adjustment
  rob = sqrt(diag(dfa*vcovHC(reg, method="arellano", type = "HC1", 
                             cluster = "group")))
  return(rob)
}

clse = function(reg) { 
  # index(reg, "id") returns the id or entity variable vector 
  G = length(unique(index(reg,"id")))
  N = length(index(reg,"id"))
  dfa = (G/(G - 1))   # note Bluhm multiplies this by finite-sample df adjustment
  rob = sqrt(diag(dfa*vcovHC(reg, method="arellano", type = "HC1", 
                             cluster = "group")))
  return(rob)
}

# corrected SEs for IV regressions
ivse = function(reg) {
  rob = pvcovHC(reg)[,2]
  return(rob)
}

###################################################################

developing <- developing_final_1_
developed <- developed_final_1_

data_developing <- pdata.frame(developing, index=c("Country", "Year"))
data_developed <- pdata.frame(developed, index=c("Country", "Year"))



names(developing)
mydata= data_developing

# reg1: OLS (pooled)
reg1 = plm(log(GDP)~lag(log(GDP), 1:1)+lag(log(FDI), 1:1)+lag(log(Trade), 1:1)+lag(log(RD), 1:1)+lag(log(Population), 1:1)
           +lag(log(Managers), 1:1), 
           data = mydata, 
           index = c("Country","Year"), model="pooling")
# summary(reg1)

# reg2: Модель с фиксированными эффектами

reg2 = plm(log(GDP)~lag(log(GDP), 1:1)+lag(log(FDI), 1:1)+lag(log(Trade), 1:1)+lag(log(RD), 1:1)+lag(log(Population), 1:1)
           +lag(log(Managers), 1:1), 
           data = mydata, 
           index = c("Country","Year"), model="within", effect="individual")
# summary(reg2)

# reg3: Модель со случайными эффектами

reg3 = plm(log(GDP)~lag(log(GDP), 1:1)+lag(log(FDI), 1:1)+lag(log(Trade), 1:1)+lag(log(RD), 1:1)+lag(log(Population), 1:1)
           +lag(log(Managers), 1:1), 
           data = mydata, 
           index = c("Country","Year"), model="random", effect="individual")
# summary(reg3)

# reg4: Модель, оцененная обобщенным методом моментов
# Обратите внимание на то, что в саммари приводятся также результаты
# теста Саргана и тестов на автокорреляцию.
# Каковы выводы из этих тестов в нашем случае?
reg4 <- pgmm(log(GDP)~lag(log(GDP), 1:1)+lag(log(FDI), 1:1)+lag(log(Trade), 1:1)+lag(log(RD), 1:1)+lag(log(Population), 1:1)
             +lag(log(Managers), 1:1)
             |lag(log(FDI), 2:4)+lag(log(Trade), 2:4)+lag(log(RD), 2:4)+lag(log(Population), 2:4)
             +lag(log(Managers), 2:4),
             data = mydata,
             effect="individual",model="twosteps")
summary(reg4)



#Сравним результаты
stargazer(reg1,reg2,reg3,reg4,
          se=list(clse(reg1),clse(reg2),clse(reg3),clse(reg4)),
          title="Модель", type="text", 
          column.labels=c("МНК", "Фиксированные эффекты", "Случайные эффекты", "ОММ"), 
          df=FALSE, digits=3,
          out = "filename.tex")

# Test to compare pooled regression and fixed effects regression:
pooltest(reg1, reg2)

# Another test to compare pooled regression and fixed effects models
# (including two-way fixed effects) is plmtest.
# Its argument is only one model — the pooled model.
plmtest(reg1, effect = "twoways", type = "ghm")

# The 'type' argument allows choosing among several test variants.
# See the documentation for details.

# For completeness, the Breusch–Pagan test,
# which compares the pooled model and the random effects (RE) model
# (estimation of the RE model will be discussed later)
plmtest(reg1, effect = "twoways", type = "bp")

# Hausman test to compare fixed effects (FE) and random effects (RE) models
phtest(reg2, reg3)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Estimations
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

mydata = data_developed

# summary(mydata)
# sum <- kable(summary(mydata), caption = "Описательная статистика данных", longtable = TRUE, row.names = nrow(mydata), format = "latex")
# cat(sum, file = "mytable.tex", sep = "\n")


reg1111 = plm(log(GDP)~lag((FDI), 1:1)+lag((Trade), 1:1)+lag((Savings_rate), 1:1)+lag((RD), 1:1)+lag((Labor), 1:1)+lag((Population), 1:1)
              +lag((Labor_female), 1:1)+lag((Senior), 1:1)+lag((Literacy), 1:1)+lag((Primary_education), 1:1)+lag((Secondary_education), 1:1)
              +lag((Tertiary_education), 1:1)+lag((Life_expect_female), 1:1)+lag((Sex_ratio), 1:1)+lag((Parliament), 1:1)+lag((Ministerial), 1:1), 
              data = mydata, 
              index = c("Country","Year"), model="within", effect="individual")


# Compare results
stargazer(reg1111,
          se=list(clse(reg1111)),
          title="Модель", type="text", 
          df=FALSE, digits=3,
          out = "filename.tex")



reg1 = plm(log(GDP)~ lag((growth_2000), 1:1) + lag((FDI), 1:1)+ lag((Savings_rate), 1:1)+lag((RD), 1:1)+lag((Labor), 1:1)
           +lag((Labor_female), 1:1), 
           data = mydata, 
           index = c("Country","Year"), model="within", effect="individual")

reg2 = plm(log(GDP)~lag((growth_2000), 1:1) + lag((FDI), 1:1)+ lag((Savings_rate), 1:1)+lag((RD), 1:1)+lag((Labor), 1:1)
           +lag((Senior), 1:1), 
           data = mydata, 
           index = c("Country","Year"), model="within", effect="individual")

reg3 = plm(log(GDP)~lag((growth_2000), 1:1) + lag((FDI), 1:1)+ lag((Savings_rate), 1:1)+lag((RD), 1:1)+lag((Labor), 1:1)
           +lag((Literacy), 1:1), 
           data = mydata, 
           index = c("Country","Year"), model="within", effect="individual")

reg4 = plm(log(GDP)~lag((growth_2000), 1:1) + lag((FDI), 1:1)+ lag((Savings_rate), 1:1)+lag((RD), 1:1)+lag((Labor), 1:1)
           +lag((Primary_education), 1:1), 
           data = mydata, 
           index = c("Country","Year"), model="within", effect="individual")

reg5 = plm(log(GDP)~lag((growth_2000), 1:1) + lag((FDI), 1:1)+ lag((Savings_rate), 1:1)+lag((RD), 1:1)+lag((Labor), 1:1)
           +lag((Secondary_education), 1:1), 
           data = mydata, 
           index = c("Country","Year"), model="within", effect="individual")

reg6 = plm(log(GDP)~lag((growth_2000), 1:1) + lag((FDI), 1:1)+ lag((Savings_rate), 1:1)+lag((RD), 1:1)+lag((Labor), 1:1)
           +lag((Tertiary_education), 1:1), 
           data = mydata, 
           index = c("Country","Year"), model="within", effect="individual")

reg7 = plm(log(GDP)~lag((growth_2000), 1:1) + lag((FDI), 1:1)+ lag((Savings_rate), 1:1)+lag((RD), 1:1)+lag((Labor), 1:1)
           +lag((Life_expect_female), 1:1), 
           data = mydata, 
           index = c("Country","Year"), model="within", effect="individual")

reg8 = plm(log(GDP)~lag((growth_2000), 1:1) + lag((FDI), 1:1)+ lag((Savings_rate), 1:1)+lag((RD), 1:1)+lag((Labor), 1:1)
           +lag((Sex_ratio), 1:1), 
           data = mydata, 
           index = c("Country","Year"), model="within", effect="individual")

reg9 = plm(log(GDP)~lag((growth_2000), 1:1) + lag((FDI), 1:1)+ lag((Savings_rate), 1:1)+lag((RD), 1:1)+lag((Labor), 1:1)
           +lag((Parliament), 1:1), 
           data = mydata, 
           index = c("Country","Year"), model="within", effect="individual")


reg10 = plm(log(GDP)~ lag((growth_2000), 1:1) + lag((FDI), 1:1)+ lag((Savings_rate), 1:1)+lag((RD), 1:1)+lag((Labor), 1:1)
            +lag((Ministerial), 1:1), 
            data = mydata, 
            index = c("Country","Year"), model="within", effect="individual")

# Compare results
stargazer(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,reg9,reg10,
          se=list(clse(reg1),clse(reg2),clse(reg3),clse(reg4),clse(reg5),
                  clse(reg6),clse(reg7),clse(reg8),clse(reg9),clse(reg10)),
          title="Models", type="text", 
          df=FALSE, digits=3,
          out = "filename.tex")





