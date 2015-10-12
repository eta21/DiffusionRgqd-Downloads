
rm(list=ls(all=TRUE))
#=====================================================================
# Author        : Etienne A.D. Pienaar
# Ver           : 1.0
# Tests         : Succesfull run on R 3.2.1 using DiffusionRgqd 0.1.0
#                 Succesfull run on R 3.2.1 using DiffusionRgqd 0.1.0
# Notes         :-This example requires the 'Quandl' package. Try
#                 install.packages('Quandl',dep=TRUE)
#                -Last test used  Quandl 2.6.0  Quandl 2.7.0
#                 If  Qunandl fails the data can be sourced from
#
#=====================================================================

library(Quandl)
library(DiffusionRgqd)
#**
# Source data for the S&P500 index (SPX).
quandldata1 <- Quandl("YAHOO/INDEX_GSPC", collapse="weekly",
start_date="1990-01-01",end_date="2015-01-01", type="raw")
St <- rev(quandldata1[,names(quandldata1)=='Close'])
time1 <-rev(quandldata1[,names(quandldata1)=='Date'])
# Source data for the volatility index (VIX).
quandldata2 <- Quandl("YAHOO/INDEX_VIX", collapse="weekly",
start_date="1990-01-01",end_date="2015-01-01", type="raw")
Vt <- rev(quandldata2[,names(quandldata2)=='Close'])
time2 <- rev(quandldata2[,names(quandldata2)=='Date'])
#**

GQD.remove() # Remove the previous model coefficients

# R_t coefficients:
a00 <- function(t){theta[1]}
a01 <- function(t){-0.5*theta[2]*theta[2]}
c01 <- function(t){theta[2]*theta[2]}
d01 <- function(t){theta[2]*theta[5]*theta[6]}
# V_t coefficients:
b00 <- function(t){theta[3]}
b01 <- function(t){-theta[4]}
e01 <- function(t){theta[2]*theta[5]*theta[6]}
f01 <- function(t){theta[5]*theta[5]}

# Create data matrix and numerical time vector :
X <- cbind(log(St),(Vt/100)^2)
time <- cumsum(c(0,diff(as.Date(time1))*(1/365)))

# Some starting parameters for the optimization routine:
theta.start <- c(0,1,1,0.5,1,0)
# Calculate MLEs of the parameter vector:
model_1 <- BiGQD.mle(X,time,mesh=10,theta=theta.start)

# Retreve parameter estimates and appr. 95% CIs:
GQD.estimates(model_1)

GQD.remove() # Remove the previous model coefficients
# R_t coefficients:
a00 <- function(t){theta[1]}
a02 <- function(t){-0.5*theta[2]*theta[2]}
c02 <- function(t){theta[2]*theta[2]}
d02 <- function(t){theta[2]*theta[5]*theta[6]}
# V_t coefficients:
b00 <- function(t){theta[3]}
b01 <- function(t){-theta[4]}
e02 <- function(t){theta[2]*theta[5]*theta[6]}
f02 <- function(t){theta[5]*theta[5]}

theta.start <- c(0,1,1,1,1,0)
model_2 <- BiGQD.mle(X,time,mesh=10,theta=theta.start)

# Compare AIC and BIC vlaues for models 1 and 2:
GQD.aic(list(model_1,model_2))

GQD.remove()
# R_t coefficients:
a02 <- function(t){-0.5*theta[1]*theta[1]}
c02 <- function(t){theta[1]*theta[1]}
d02 <- function(t){theta[1]*theta[4]*theta[5]}
# V_t coefficients:
b00 <- function(t){theta[2]}
b01 <- function(t){-theta[3]}
e02 <- function(t){theta[1]*theta[4]*theta[5]}
f02 <- function(t){theta[4]*theta[4]}

theta.start <- c(1,1,1,1,0)
model_3 <- BiGQD.mle(X,time,mesh=10,theta=theta.start)

GQD.remove()
# R_t coefficients:
a00 <- function(t){theta[1]}
a10 <- function(t){theta[7]}
a02 <- function(t){-0.5*theta[2]*theta[2]}
c02 <- function(t){theta[2]*theta[2]}
d02 <- function(t){theta[2]*theta[5]*theta[6]}
# V_t coefficients:
b00 <- function(t){theta[3]}
b01 <- function(t){-theta[4]}
e02 <- function(t){theta[2]*theta[5]*theta[6]}
f02 <- function(t){theta[5]*theta[5]}

theta.start <- c(0,1,1,1,1,0,0)
model_4 <- BiGQD.mle(X,time,mesh=10,theta=theta.start)

GQD.remove()
# R_t coefficients:
a00 <- function(t){theta[1]}
a02 <- function(t){-0.5*theta[2]*theta[2]}
c02 <- function(t){theta[2]*theta[2]}
d02 <- function(t){theta[2]*theta[5]*theta[6]}
# V_t coefficients:
b00 <- function(t){theta[3]}
b10 <- function(t){theta[7]}
b01 <- function(t){-theta[4]}
e02 <- function(t){theta[2]*theta[5]*theta[6]}
f02 <- function(t){theta[5]*theta[5]}
theta.start <- c(0,1,1,1,1,0,0)
model_5 <- BiGQD.mle(X,time,mesh=10,theta=theta.start)

GQD.aic(list(model_1,model_2,model_3,model_4,model_5))

GQD.estimates(model_4)

