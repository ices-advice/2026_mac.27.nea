## Extract results of interest, write TAF output tables

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

#-------------------------------------------------------------------------------
# load model runs
#-------------------------------------------------------------------------------

model <- "0.01"


load(paste0("./model/fit_and_diagnostics/model_",model,"_full_output.RData"))
#load(paste0("./model/fit_model_",model,".RData"))

# create FLR objects for first model    
assess.name  <- paste0("model",model)
modname      <- model
source('./boot/software/utils/SAM2FLRtmb.r') 
      # this is a self coded SAM2FLR function... there is surely a more elegant way to do this

save(Mac, file="./output/Mac_FLStock.inp.RData")


## create a second object with the model estimates for catch.n
cmod<-caytable(fit)
Mac.mod<-Mac

catch.n(Mac.mod)[] <-  t(cmod)

save(Mac.mod, file="./output/Mac_FLStock.mod.RData")


#STF
# load config
load("./model/STFconf.RData")
# load STF results
load("./model/STF_lowR.RData")
