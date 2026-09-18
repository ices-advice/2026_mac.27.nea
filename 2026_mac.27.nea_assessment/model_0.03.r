## Run analysis, write model results

#-------------------------------------------------------------------------------
# initialise
#-------------------------------------------------------------------------------
library(icesTAF)
taf.library(stockassessment)
source("./boot/software/utils/utility.r")
mkdir("model")
mkdir("model/fit_and_diagnostics")

# new model name
modName <- "0.03"


#-------------------------------------------------------------------------------
# Data
#-------------------------------------------------------------------------------


source("data0.03.R")

#-------------------------------------------------------------------------------
# Model configuration
#-------------------------------------------------------------------------------

conf<-defcon(dat)


####### Fishing mortality process
#states fully decoupled
conf$keyLogFsta[1,]  <- c(0:9,9)

# F random walk variance different for young and for old ages
conf$keyVarF[1,]    <- c( 0,0,1,1,1,1,1,1,1,2,2)

# correlation in the random walks
conf$corFlag <- 2

# Fbar age range
conf$fbarRange <- c(4,8)


####### Observation variances
conf$keyVarObs[1,]  <-c(0,0,1,1,1,1,1,1,1,2,2)             # catches
conf$keyVarObs[2,1] <-  3                                  # egg survey
conf$keyVarObs[3,]  <- c(-1,4,5,5,5,5,5,5,6,6,-1)
conf$keyVarObs[4,]  <- c(7,8,9,9,9,9,9,9,9,9,-1)
conf$keyVarObs[5,1]  <- 10
#for the downweight catches
conf$fixVarToWeight<-1

# correlation structure in errors
conf$obsCorStruct[] <- c( "ID", "ID", "AR",  "AR", "ID","ID")
conf$keyCorObs[3,]<-c( -1, 0  ,0 , 0,  0,  0,  0,   0 ,  0 , -1)       ### the first -1 was 0 in Niels fit
conf$keyCorObs[4,]<-c( 1, 1  ,1 , 1,  1,  1,  1,   1 ,  1 , -1)


####### Catchabilities

    #conf$keyLogFpar[3,2:10] <- c(1,2,3,4,5,5,5,5,5)        # was fully decoupled in Niels fit
#    conf$keyLogFpar[4,1:10]     <-  c(6:14,14)
#    conf$keyLogFpar[5,1]     <-    15
#

#- Set default starting parameters
par<-defpar(dat,conf)

#- save data file
save(surveys,cn,cw,dw,lf,lw,mo,nm,pf,pm,sw,dat,recap,conf,par,file=paste0("data/data_",modName,".RData"))

#-------------------------------------------------------------------------------
# fit  model
#-------------------------------------------------------------------------------

fit <-sam.fit(dat,conf,par)
save(fit, file=paste0("model/fit_model_",modName,".RData"))

save(fit,file=paste0("model/fit_and_diagnostics/model_",modName,"_full_output.RData"))
