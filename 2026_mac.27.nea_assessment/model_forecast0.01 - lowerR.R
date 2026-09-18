

#-------------------------------------------------------------------------------
# initialise
#-------------------------------------------------------------------------------

library(icesTAF)
taf.library(stockassessment)
library(ggplot2)
#taf.bootstrap()

mkdir("output")
mkdir("output/model diags")
mkdir("output/model diags/explorations/")




# load model run
model <- "0.01"
load(paste0("./model/fit_and_diagnostics/model_",model,"_full_output.RData"))


# Config years and reference values
ay<- max(fit$data$years)   # current year, also final data year as we have in-year data
ady <- ay + 1
Ave.yrs <- ay - (1:3)      # no biological input data for the curreny year
Rec.yrs <- 2017:ay         # take all recruitments since 2017, incl. current year
# simulation numbers
nsim <- 20000
cat("! change back to 20000 sims")
#ref points
MSYBtrigger <- 4119337
Blim        <- 3067017
Fmsy        <- 0.191
# catch estimate for 2026
IYC <- 398186
# current year advice
Cur.Adv  <- 174357

save(ay,ady,Ave.yrs,Rec.yrs,MSYBtrigger,Blim,Fmsy,IYC,Cur.Adv,file=("./model/STFconf.RData"))


#### FC : F2027 = Fmsy
set.seed(123)
FC1<- forecast(
  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
  catchval.exact = c(IYC,NA,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,Fmsy,NA),                                                          #  a vector of target f values. See details.
  nosim = nsim,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = "F2027 = Fmsy",                                                                 #  optional label to appear in short table
  processNoiseF = FALSE,                                                         #  option to turn off process noise in F
  savesim = TRUE
  )

SSBs <- attr(FC1, "tab")[, "ssb:median"]




#### FC2 : F2027 = Fmsy reduced 

Fred <- Fmsy * SSBs[["2026"]] / MSYBtrigger

set.seed(123)
FC2<- forecast(
  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
  catchval.exact = c(IYC,NA,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,Fred,NA),                                                          #  a vector of target f values. See details.
  nosim = nsim,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = "F2027 = Fred",                                                               #  optional label to appear in short table
  processNoiseF = FALSE,                                                         #  option to turn off process noise in F
  savesim = T
)

FC2
SSBs <- attr(FC2, "tab")[, "ssb:median"]
Fbars <- attr(FC2, "tab")[, "fbar:median"]
ssbs <- FC2[[1]]$ssb
pBlim2026<-sum(FC2[[1]]$ssb < Blim)/length(FC2[[1]]$ssb)
pBlim2028<-sum(FC2[[3]]$ssb < Blim)/length(FC2[[3]]$ssb)



#### FC3 : F2027 = 0
set.seed(123)

FC3<- forecast(
  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
  catchval.exact = c(IYC,NA,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,0,NA),                                                          #  a vector of target f values. See details.
  nosim = nsim,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = "F2027 = 0",                                                               #  optional label to appear in short table
  processNoiseF = FALSE,                                                         #  option to turn off process noise in F
  savesim = T
)


FC3

FC[[3]]$year
pBlim<-sum(FC3[[3]]$ssb < Blim)/length(FC3[[3]]$ssb)


#### FC4 : F2027 = F2026
set.seed(123)

FC4<- forecast(
  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,1,1),                                                        #  a vector of f-scales. See details.
  catchval.exact = c(IYC,NA,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,NA,NA),                                                          #  a vector of target f values. See details.
  nosim = nsim,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = "F2027 = F2026",                                                               #  optional label to appear in short table
  processNoiseF = FALSE,                                                         #  option to turn off process noise in F
  savesim = T
)


FC4

#### FC5 : Catch2027 = Catch2026
set.seed(123)

FC5<- forecast(
  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
  catchval.exact = c(IYC,IYC,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,NA,NA),                                                          #  a vector of target f values. See details.
  nosim = nsim,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = "Catch2027 = Catch2026",                                                               #  optional label to appear in short table
  processNoiseF = FALSE,                                                         #  option to turn off process noise in F
  savesim = T
)


FC5


#### FC6 : Catch2026 = Catch2025 +25%
set.seed(123)

FC6<- forecast(
  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
  catchval.exact = c(IYC,1.25 * IYC,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,NA,NA),                                                          #  a vector of target f values. See details.
  nosim = nsim,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = "Catch2027 = Catch2026 +25%",                                                               #  optional label to appear in short table
  processNoiseF = FALSE,                                                         #  option to turn off process noise in F
  savesim = T
)


FC6

#### FC7 : Catch2026 = Catch2025 -20%
set.seed(123)

FC7<- forecast(
  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
  catchval.exact = c(IYC,0.8 * IYC,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,NA,NA),                                                          #  a vector of target f values. See details.
  nosim = nsim,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = "Catch2027 = Catch2026 -20%",                                                               #  optional label to appear in short table
  processNoiseF = FALSE,                                                         #  option to turn off process noise in F
  savesim = T
)


FC7


#### FC8 : SSB2027 - Blim
#
#FC8<- forecast(

#  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
#  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
#  catchval.exact = c(IYC,NA,NA),                                                        #  a vector of target catches which will be met without noise. See details.
#  fval     = c(NA,NA,NA),                                                          #  a vector of target f values. See details.
#  nextssb = c(NA,Blim,NA),
#  nosim = nsim,                                                                 #  number of simulations default is 1000
#  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
#  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
#  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
#  label = "SSB2027 - Blim",                                                               #  optional label to appear in short table
#  processNoiseF = FALSE,                                                         #  option to turn off process noise in F
#  savesim = T
#)
#
#
#FC8

cat("nextssb option only available if SSB is calculated in the beginning of the year")





####### run a sequence of Ftargat values to find the F p(SSB<Blim)=0.5
Frange  <- seq(0.29,0.31,by=0.001)
pBlim <-c()
pBtrig <-c()
FCss<-list()

for (i in 1:length(Frange))
{

ftarg <- Frange[i]
set.seed(123)

fc<- forecast(
  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
# catchval = c(550000,NA,NA),                                                   #  a vector of target catches. See details.
  catchval.exact = c(IYC,NA,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,ftarg,NA),                                                          #  a vector of target f values. See details.
  nosim = nsim,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = paste("F2027 =", ftarg)  ,
     savesim = T                                                               #  optional label to appear in short table
)

FCss[[i]]<-fc

pBlim[i] <-sum(fc[[3]]$ssb < Blim)/length(fc[[3]]$ssb)
pBtrig[i]<-sum(fc[[3]]$ssb < MSYBtrigger)/length(fc[[3]]$ssb)
}


plot(Frange,pBlim)
plot(Frange,pBtrig)       # not possible to get for p(SSB<MSYBtrigger) =0.5 even for F=0


data.frame(F=Frange, pBlim = pBlim , less50percent = pBlim < 0.50)


# pick manually the last value that leads to pBlim<50%
idx <- 20

pBlim[idx]

FC8<-FCss[[idx]]


########################################################################################################################################                                                                                       
#################################  special for the 2026 advice
#################################  options for the rebuilding HCR option going to 0 at SSB=Blim (special request)
#################################  
#######################################################################################################################



####  FC9 : rebuilding HCR with Flow=0
Flow  <- 0 
Fhcr  <-  Fmsy * (SSBs[["2026"]] - Blim) / ( MSYBtrigger - Blim)
Fhcr <- max(Flow,Fhcr)



set.seed(123)
FC9<- forecast(
  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
  catchval.exact = c(IYC,NA,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,Fhcr,NA),                                                          #  a vector of target f values. See details.
  nosim = nsim,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = "F2027 = HCR_Flow=0",                                                               #  optional label to appear in short table
  processNoiseF = FALSE,                                                         #  option to turn off process noise in F
  savesim = T
)

FC9


####  FC10 : rebuilding HCR with Flow=0.025
Flow  <- 0.025 
Fhcr  <-  Fmsy * (SSBs[["2026"]] - Blim) / ( MSYBtrigger - Blim)
Fhcr <- max(Flow,Fhcr)
   


set.seed(123)
FC10<- forecast(
  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
  catchval.exact = c(IYC,NA,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,Fhcr,NA),                                                          #  a vector of target f values. See details.
  nosim = nsim,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = "F2027 = HCR_Flow=0.025",                                                               #  optional label to appear in short table
  processNoiseF = FALSE,                                                         #  option to turn off process noise in F
  savesim = T
)

FC10


####  FC11 : rebuilding HCR with Flow=0.05
Flow  <- 0.05 
Fhcr  <-  Fmsy * (SSBs[["2026"]] - Blim) / ( MSYBtrigger - Blim)
Fhcr <- max(Flow,Fhcr)
   


set.seed(123)
FC11<- forecast(
  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
  catchval.exact = c(IYC,NA,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,Fhcr,NA),                                                          #  a vector of target f values. See details.
  nosim = nsim,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = "F2027 = HCR_Flow=0.05",                                                               #  optional label to appear in short table
  processNoiseF = FALSE,                                                         #  option to turn off process noise in F
  savesim = T
)

FC11




####  FC12 : rebuilding HCR with Flow=0.075
Flow  <- 0.075 
Fhcr  <-  Fmsy * (SSBs[["2026"]] - Blim) / ( MSYBtrigger - Blim)
Fhcr <- max(Flow,Fhcr)
   


set.seed(123)
FC12<- forecast(
  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
  catchval.exact = c(IYC,NA,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,Fhcr,NA),                                                          #  a vector of target f values. See details.
  nosim = nsim,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = "F2027 = HCR_Flow=0.075",                                                               #  optional label to appear in short table
  processNoiseF = FALSE,                                                         #  option to turn off process noise in F
  savesim = T
)

FC12



####  FC13 : rebuilding HCR with Flow=0.1
Flow  <- 0.1 
Fhcr  <-  Fmsy * (SSBs[["2026"]] - Blim) / ( MSYBtrigger - Blim)
Fhcr <- max(Flow,Fhcr)
   


set.seed(123)
FC13<- forecast(
  fit,                                                                          #  an assessment object of type sam, as returned from the function sam.fit
  fscale   = c(NA,NA,1),                                                        #  a vector of f-scales. See details.
  catchval.exact = c(IYC,NA,NA),                                                        #  a vector of target catches which will be met without noise. See details.
  fval     = c(NA,Fhcr,NA),                                                          #  a vector of target f values. See details.
  nosim = nsim,                                                                 #  number of simulations default is 1000
  year.base = ay,                                              #  starting year default last year in assessment. Currently it is only supported to use last assessment year or the year before
  ave.years = Ave.yrs ,                                     #  vector of years to average for weights, maturity, M and such
  rec.years = Rec.yrs ,                                     #  vector of years to use to resample recruitment from
  label = "F2027 = HCR_Flow=0.1",                                                               #  optional label to appear in short table
  processNoiseF = FALSE,                                                         #  option to turn off process noise in F
  savesim = T
)

FC13


#### save all runs

save(FC1,FC2,FC3,FC4,FC5,FC6,FC7,FC8,FC9,FC10,FC11,FC12,FC13, Frange, pBlim , file ="./model/STF_lowR.RData")
