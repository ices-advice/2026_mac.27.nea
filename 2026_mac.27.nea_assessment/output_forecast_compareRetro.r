

#-------------------------------------------------------------------------------
# initialise
#-------------------------------------------------------------------------------

library(icesTAF)
taf.library(stockassessment)
library(ggplot2)     
#taf.bootstrap()
library(FLCore)



mkdir("output")
mkdir("output/model diags")
mkdir("output/model diags/explorations/")

set.seed(123)


# load model run
model <- "0.01"
load(paste0("./model/fit_and_diagnostics/model_",model,"_full_output.RData"))
# load config
load("./model/STFconf.RData")
# load STF results
load("./model/STF_lowR.RData")


# FC2 basis for advice 
res<- as.data.frame(attr(FC2,"shorttab"))
res2025<- c(summary(fit)["2025",] [c(7,1,4)]  , # same order as in shorttab
            catchtable(fit)["2025","Estimate"]  )
res<-cbind(res2025,res)
names(res)[1] <- 2025
res$quant <- c("Fbar","rec","SSB","catch")
res$run <- "WGWIDE2026"
res1 <- tidyr::gather(res , key=year,value=data,1:4)
res1$source <- "STF"
res1$source[8] <- "IY assumption"
res1$source[c(1:4,6)] <- "SAM"

# load STF results  from previous forecast
load("../2025_mac.27.nea_assessment/model/STF_lowR.RData")
load("../2025_mac.27.nea_assessment/model/fit_model_0.01.RData")
res<- as.data.frame(attr(FC8,"shorttab"))
res2024<- c(summary(fit)["2024",] [c(7,1,4)]  , # same order as in shorttab
            catchtable(fit)["2024","Estimate"]  )
res<-cbind(res2024,res)
names(res)[1] <- 2024
res$quant <- c("Fbar","rec","SSB","catch")
res$run <- "WGWIDE2025"
res2 <- tidyr::gather(res , key=year,value=data,1:4)
res2$source <- "STF"
res2$source[8] <- "IY assumption"
res2$source[c(1:4,6)] <- "SAM"


res<-rbind(res1,res2)



library(ggplot2)
ggplot(res , aes(year,data,grou=run)) + 
              geom_point(aes(colour=run,shape= source),size=2)  + 
              geom_line(aes(colour=run))  +
              facet_grid(quant~. ,scales ="free")
ggsave(file="./output/STFcomparison/comparison_results.png")              
              
              
              
res<-tidyr::spread(res , key=run , value = data)              



