##import all the necessary libraries
library(doBy)
library(ggplot2)
library(forecast)
## use just the US data
fd<-dplyr::filter(train,Country_Region=="US")
cases<-dplyr::filter(fd,Target=="ConfirmedCases")
deaths<-dplyr::filter(fd,Target=="Fatalities")


## collapse the data by date and state
d<-summaryBy(TargetValue~Province_State+Date,FUN=c(sum),data=deaths)
c<-summaryBy(TargetValue~Province_State+Date,FUN=c(sum),data=cases)



## I selected the top 4 states with the most covid cases
## I separated the data by cases and deaths
cases_ca<-dplyr::filter(c,Province_State=="California")
cases_tx<-dplyr::filter(c,Province_State=="Texas")
cases_fl<-dplyr::filter(c,Province_State=="Florida")
cases_ny<-dplyr::filter(c,Province_State=="New York")

deaths_ca<-dplyr::filter(d,Province_State=="California")
deaths_tx<-dplyr::filter(d,Province_State=="Texas")
deaths_fl<-dplyr::filter(d,Province_State=="Florida")
deaths_ny<-dplyr::filter(d,Province_State=="New York")


cca<-cases_ca$TargetValue.sum
ctx<-cases_tx$TargetValue.sum
cfl<-cases_fl$TargetValue.sum
cny<-cases_ny$TargetValue.sum

dca<-deaths_ca$TargetValue.sum
dtx<-deaths_tx$TargetValue.sum
dfl<-deaths_fl$TargetValue.sum
dny<-deaths_ny$TargetValue.sum


## create ts format
cca<-ts(cca,start=c(2020,23),frequency=365)
ctx<-ts(ctx,start=c(2020,23),frequency=365)
cfl<-ts(cfl,start=c(2020,23),frequency=365)
cny<-ts(cny,start=c(2020,23),frequency=365)


dca<-ts(dca,start=c(2020,23),frequency=365)
dtx<-ts(dtx,start=c(2020,23),frequency=365)
dfl<-ts(dfl,start=c(2020,23),frequency=365)
dny<-ts(dny,start=c(2020,23),frequency=365)


## separate training and test data
## training
trcca<-ts(cca[1:120],start=c(2020,23),frequency=365)
trctx<-ts(ctx[1:120],start=c(2020,23),frequency=365)
trcfl<-ts(cfl[1:120],start=c(2020,23),frequency=365)
trcny<-ts(cny[1:120],start=c(2020,23),frequency=365)


trdca<-ts(dca[1:120],start=c(2020,23),frequency=365)
trdtx<-ts(dtx[1:120],start=c(2020,23),frequency=365)
trdfl<-ts(dfl[1:120],start=c(2020,23),frequency=365)
trdny<-ts(dny[1:120],start=c(2020,23),frequency=365)


##test
ttcca<-ts(cca[121:140],start=c(2020,143),frequency=365)
ttctx<-ts(ctx[121:140],start=c(2020,143),frequency=365)
ttcfl<-ts(cfl[121:140],start=c(2020,143),frequency=365)
ttcny<-ts(cny[121:140],start=c(2020,143),frequency=365)


ttdca<-ts(dca[121:140],start=c(2020,143),frequency=365)
ttdtx<-ts(dtx[121:140],start=c(2020,143),frequency=365)
ttdfl<-ts(dfl[121:140],start=c(2020,143),frequency=365)
ttdny<-ts(dny[121:140],start=c(2020,143),frequency=365)


## train models and forecast
## Holt & Holt damped
#California
hcca<-holt(trcca,h=20,damped=FALSE)
hdcca<-holt(trcca,h=20,damped=TRUE)
hdca<-holt(trdca,h=20,damped=FALSE)
hddca<-holt(trdca,h=20,damped=TRUE)
##Texas
hctx<-holt(trctx,h=20,damped=FALSE)
hdctx<-holt(trctx,h=20,damped=TRUE)
hdtx<-holt(trdtx,h=20,damped=FALSE)
hddtx<-holt(trdtx,h=20,damped=TRUE)
##Florida
hcfl<-holt(trcfl,h=20,damped=FALSE)
hdcfl<-holt(trcfl,h=20,damped=TRUE)
hdfl<-holt(trdfl,h=20,damped=FALSE)
hddfl<-holt(trdfl,h=20,damped=TRUE)
##New York
hcny<-holt(trcny,h=20,damped=FALSE)
hdcny<-holt(trcny,h=20,damped=TRUE)
hdny<-holt(trdny,h=20,damped=FALSE)
hddny<-holt(trdny,h=20,damped=TRUE)

##auto arima
acca<-auto.arima(trcca,approximation = FALSE,stepwise = FALSE)
adca<-auto.arima(trdca,approximation = FALSE,stepwise = FALSE)

actx<-auto.arima(trctx,approximation = FALSE,stepwise = FALSE)
adtx<-auto.arima(trdtx,approximation = FALSE,stepwise = FALSE)


acfl<-auto.arima(trcfl,approximation = FALSE,stepwise = FALSE)
adfl<-auto.arima(trdfl,approximation = FALSE,stepwise = FALSE)


acny<-auto.arima(trcny,approximation = FALSE,stepwise = FALSE)
adny<-auto.arima(trdny,approximation = FALSE,stepwise = FALSE)

## arima's forecast
a_cca<-forecast(acca,h=20)
a_dca<-forecast(adca,h=20)

a_ctx<-forecast(actx,h=20)
a_dtx<-forecast(adtx,h=20)

a_cfl<-forecast(acfl,h=20)
a_dfl<-forecast(adfl,h=20)

a_cny<-forecast(acny,h=20)
a_dny<-forecast(adny,h=20)

## test accuracy
#california
accuracy(hcca,ttcca)
accuracy(hdcca,ttcca)
accuracy(a_cca,ttcca)

accuracy(hdca,ttdca)
accuracy(hddca,ttdca)
accuracy(a_dca,ttdca)

# texas
accuracy(hctx,ttctx)
accuracy(hdctx,ttctx)
accuracy(a_ctx,ttctx)

accuracy(hdtx,ttdtx)
accuracy(hddtx,ttdtx)
accuracy(a_dtx,ttdtx)

# florida
accuracy(hcfl,ttcfl)
accuracy(hdcfl,ttcfl)
accuracy(a_cfl,ttcfl)

accuracy(hdfl,ttdfl)
accuracy(hddfl,ttdfl)
accuracy(a_dfl,ttdfl)

## new york

accuracy(hcny,ttcny)
accuracy(hdcny,ttcny)
accuracy(a_cny,ttcny)

accuracy(hdny,ttdny)
accuracy(hddny,ttdny)
accuracy(a_dny,ttdny)

## daily cases
autoplot(cca,series="California")+autolayer(ctx,series="Texas")+autolayer(cfl,series="Florida")+autolayer(cny,series="New York")+ylab("Daily Cases")


## daily deaths
autoplot(dca,series="California")+autolayer(dtx,series="Texas")+autolayer(dfl,series="Florida")+autolayer(dny,series="New York")+ylab("Daily Deaths")

## average cases and deaths per day
mean(cca)
mean(ctx)
mean(cfl)
mean(cny)

mean(dca)
mean(dtx)
mean(dfl)
mean(dny)

