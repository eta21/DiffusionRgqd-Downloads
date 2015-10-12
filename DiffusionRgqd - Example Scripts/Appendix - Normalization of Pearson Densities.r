rm(list=ls(all=TRUE))
library(DiffusionRgqd)
GQD.remove()

G0 <- function(t){2*(10+sin(2*pi*(t-0.5)))}
G1 <- function(t){-2}
Q1 <- function(t){0.25*(1+0.75*(sin(4*pi*t)))}

states <- seq(5,15,1/10)
initial <- 8
Tmax <- 5
Tstart <- 1
increment <- 1/100

M <- GQD.density(Xs=initial,Xt=states,s=Tstart,t=Tmax,delt=increment)

M1 <- GQD.density(Xs=initial,Xt=states,s=Tstart,t=Tmax,delt=increment,
Dtype='Normal', P = 100,alpha=1,lower = 1,upper = 20)
# Normalization regime no. 2:
M2 <- GQD.density(Xs=initial,Xt=states,s=Tstart,t=Tmax,delt=increment,
Dtype='Normal', P = 200,alpha=3,lower = 1,upper = 20)

plot(1,1,type= 'n',xlim=c(1,5),ylim=c(1,20),xlab='Time (t)',ylab = 'Mesh')
for(i in 1:100)
{
lines(M1$mesh[i,]~M1$time)
}
plot(1,1,type= 'n',xlim=c(1,5),ylim=c(1,20),xlab='Time (t)',ylab = 'Mesh')
for(i in 1:200)
{
lines(M2$mesh[i,]~M1$time)
}