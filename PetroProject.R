library(vars)
rm(list=ls(all=TRUE))
ls()

filepath <- 'D:/My Stuffs/Daily Stuffs/University/Master/Petroleum Economics/Assignments/Article-v2'
setwd(filepath)

set.seed(123)

data0<-read.table("Dataset1.csv",sep =",",header = TRUE)
prod<-100*(data0$prod[12:575]/mean(data0$prod[12:575]) - 1)
rea<-data0$rea[12:575]
rpo<- 100*(data0$rpo[12:575]/mean(data0$rpo[12:575]) - 1)
vardata<-ts(cbind(prod,rea,rpo),start = 1975 ,frequency = 12)


###### Price changes prediction (VAR) #######


acf(vardata,lag.max = 12)


model0 <- VAR(vardata,p=24,type = "const")
summary(model0)

roots(model0)

# predic1 = predict(model0,n.ahead=8,ci=.95)

# rpo.predic <- ts(rbind(rep(vardata[564,3],3),predic1$fcst$rpo[,1:3]),start = c(2021,12), frequency = 12)

# ts.plot(rpo.predic,col="blue",lty=c(1,2,2),ylim=c(-54,55),xlim=c(2021,2023),
#        main= "Prediction of changes in the real price of oil ")
# lines(vardata[,3])


###### Causality (SVAR) #######


irfprod1 <- irf(model0,n.ahead = 20,impulse = "prod",ci=0.90)
plot(irfprod1)

irfrea1 <- irf(model0,n.ahead = 20,impulse = "rea",ci=0.90)
plot(irfrea1)

irfrpo1 <- irf(model0,n.ahead = 20,impulse = "rpo",ci=0.90)
plot(irfrpo1)

fevdprod <- fevd(model0,n.ahead = 16)
plot(fevdprod)


###### Causality (SVAR) #######


model1 <- lm(rpo ~ prod+rea, data = vardata )
summary(model1)

