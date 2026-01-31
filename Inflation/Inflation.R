############################################
# Install required packages
############################################
install.packages(c(
  "zoo", "xts", "lubridate", "urca", "dplyr", "ggplot2",
  "forecast", "car", "tseries", "vars", "readxl", "corrplot"
))

library(zoo)
library(xts)
library(lubridate)
library(urca)
library(dplyr)
library(ggplot2)
library(forecast)
library(car)
library(tseries)
library(vars)
library(readxl)
library(corrplot)

############################################
# Robust standard errors
############################################
cse <- function(reg) {
  sqrt(diag(vcovHC(reg, type = "HC1")))
}

clse <- function(reg) {
  # Cluster-robust standard errors at the entity level
  G <- length(unique(index(reg, "id")))
  dfa <- G / (G - 1)
  sqrt(diag(dfa * vcovHC(
    reg, method = "arellano", type = "HC1", cluster = "group"
  )))
}

############################################
# Import data
############################################
data <- read_xlsx("data1.xlsx")
View(data)

############################################
# Create time series objects
############################################
CPI <- ts(data$CPI, start = c(2015, 1), frequency = 12)
i <- ts(data$i, start = c(2015, 1), frequency = 12)
NEER <- ts(data$NEER, start = c(2015, 1), frequency = 12)
output <- ts(data$output, start = c(2015, 1), frequency = 12)
oil <- ts(data$oil, start = c(2015, 1), frequency = 12)
M2 <- ts(data$M2, start = c(2015, 1), frequency = 12)
imp_p <- ts(data$imp_p, start = c(2015, 1), frequency = 12)

############################################
# Seasonal adjustment
############################################
C <- decompose(CPI)
I <- decompose(i)
E <- decompose(NEER)
OUT <- decompose(output)
O <- decompose(oil)
M <- decompose(M2)
IM <- decompose(imp_p)

T_CPI <- CPI - C$seasonal
T_I <- i - I$seasonal
T_NEER <- NEER - E$seasonal
T_OUTPUT <- output - OUT$seasonal
T_OIL <- oil - O$seasonal
T_M2 <- M2 - M$seasonal
T_IMP <- imp_p - IM$seasonal

############################################
# Augmented Dickey–Fuller tests (levels)
############################################
summary(ur.df(T_CPI, type = "none"))
summary(ur.df(T_I, type = "none"))
summary(ur.df(T_NEER, type = "none"))
summary(ur.df(T_OUTPUT, type = "none"))
summary(ur.df(T_M2, type = "none"))
summary(ur.df(T_OIL, type = "none"))
summary(ur.df(T_IMP, type = "none"))

############################################
# First differences
############################################
dCPI <- diff(T_CPI)
dI <- diff(T_I)
dNEER <- diff(T_NEER)
dOUTPUT <- diff(T_OUTPUT)
dM2 <- diff(T_M2)
dOIL <- diff(T_OIL)
dIMP <- diff(T_IMP)

############################################
# ADF tests (first differences; I(1))
############################################
summary(ur.df(dCPI, type = "none"))
summary(ur.df(dI, type = "none"))
summary(ur.df(dNEER, type = "none"))
summary(ur.df(dOUTPUT, type = "none"))
summary(ur.df(dM2, type = "none"))
summary(ur.df(dOIL, type = "none"))
summary(ur.df(dIMP, type = "none"))

############################################
# Dataset of first differences
############################################
Ddata <- data.frame(
  dCPI, dI, dNEER, dOUTPUT, dM2, dOIL, dIMP
)

VARselect(Ddata, lag.max = 1, type = "both")

############################################
# Correlation structure
############################################
corrplot(
  cor(Ddata),
  method = "shade",
  type = "upper",
  tl.col = "black",
  tl.cex = 0.7,
  diag = FALSE
)

############################################
# VAR modeling
############################################
varm <- SVAR(Ddata, p = 1, type = "const")
summary(varm)

############################################
# Stability check
############################################
roots(varm)  # Model stability

############################################
# Diagnostic tests
############################################
serial.test(varm, type = "PT.adjusted")  # No residual autocorrelation
arch.test(varm)                          # No ARCH effects

############################################
# Impulse response functions
############################################
irf_neer <- irf(
  varm,
  impulse = "dNEER",
  response = c("dCPI", "dOUTPUT"),
  n.ahead = 12,
  boot = TRUE,
  ci = 0.9
)
plot(irf_neer)

############################################
# Cumulative impulse response functions
############################################
irf_neer_cum <- irf(
  varm,
  impulse = "dNEER",
  response = c("dCPI", "dOUTPUT"),
  n.ahead = 12,
  boot = TRUE,
  cumulative = TRUE,
  ci = 0.9
)
plot(irf_neer_cum)
