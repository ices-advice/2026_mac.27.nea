

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
# load FLStock
load("./output/Mac_FLStock 0.01 .RData")

# load config
load("./model/STFconf.RData")
# load STF results
load("./model/STF_lowR.RData")
FCs <- list(FC1,FC2,FC3,FC4,FC5,FC6,FC7,FC8,FC9,FC10,FC11,FC12,FC13)


####
# check forcasted recruitments
Rec <-as.data.frame(rec(Mac)[,ac(2017:2026)])
Rec<-sort(Rec$data)
recplot(FC2)    
abline(Rec[1],0,lty="dotted",col="red",lwd= 2) 
abline(Rec[10],0,lty="dotted",col="red",lwd= 2)
abline(Rec[5],0,col="red",lwd= 2)
abline(Rec[6],0,col="red",lwd= 2)
      
simRecs<-FC1[[2]]$rec
table(simRecs)
median(simRecs)



#####

tab <- lapply(FCs , function(fc) 
{
res <- attr(fc,"shorttab")
res<-as.data.frame(res)
pBlim <- c()
for (fy in 1:3)  pBlim[fy] <- sum(fc[[fy]]$ssb < Blim)/length(fc[[fy]]$ssb)
res[paste(attr(fc,"label"),"pBlim",sep=":"),] <-  pBlim
return(res)
})

tab <- do.call("rbind",tab)
ref <- do.call("rbind",strsplit(row.names(tab),":"))
ref<-as.data.frame(ref)
names(ref) <- c("option","quant")

tab<-cbind(ref,tab)
row.names(tab) <- NULL



# table for intermediate year
imy <- subset(tab[,1:3] , option == unique(option)[[1]])
R <- rectable(fit)[,1]
R <- R[as.character(2017:(ay))]      ### carefull here : that is the alternative pessimistic option
R_geoMean <- exp(mean(log(R))) # geomean is reported in advice sheet
### watch out! this is quite different from the median of the resampling
median(sample(R,20000,replace=T))

imy$'2027' [imy$quant == "rec"]  <- R_geoMean


# catch option table
  opts             <- unique(tab$option)
  catch2027        <- subset(tab, quant == "catch" )$'2027'
  fbar20272028     <- subset(tab, quant == "fbar" )$'2027'
  ssb2027          <- subset(tab, quant == "ssb" )$'2027'
  ssb2028          <- subset(tab, quant == "ssb" )$'2028'
  chgssb2028_27    <- round( 100 *(ssb2028 - ssb2027) / ssb2027,2 )
  chcatch          <- round( 100 *(catch2027 - IYC) / IYC, 2 )
  chAdv            <- round( 100 *(catch2027 - Cur.Adv) / Cur.Adv, 2 )
  pblim            <- subset(tab, quant == "pBlim" )$'2028'
  
  
options <- data.frame(options = opts, 
                      'Total Catch 2027'= catch2027,
                      'Fage4-8'=    fbar20272028,
                      'SSB 2027'= ssb2027,
                      'SSB 2028'= ssb2028,
                      '% change SSB'=   chgssb2028_27,
                      '% change Catch'= chcatch ,
                      '% change Advice' = chAdv,
                      'p(SSB(2028)<Blim)' = pblim)
                      
write.csv(options , file = "./output/catchoptions_lowR.csv") 





# look at stability
st <- tab[,1:3]
st <- tidyr::spread(st,key = quant , value = '2026')
st <- st[,-5]
                     
R2027 <- data.frame(year = 2027 , R= FCs[[1]][[2]]$rec  )
R2028 <- data.frame(year = 2028 , R= FCs[[1]][[3]]$rec )
  
Rs<-rbind(R2027,R2028)  

library(ggplot2)
ggplot(Rs , aes(R)) + geom_density(aes(colour = factor(year)))





##  look at the age composition of SSB and catch in advice

fc <- FCs[[2]]

sims<-lapply(as.list(1:3), function(y)

{ # year to extract
dim(fc[[y]]$sim) # all iterations logN and logF at age with rows=iterations, columns=logN and then logF
fit <- attr(fc, "fit") # the fit of the model was saved as attribute, can be used to get the model configuration
idx <- (fit$conf$minAge: fit$conf$maxAge)-1 # ages to extract logN
logN <- fc[[y]]$sim[,idx] # ages 0 to 8+
logF <- fc[[y]]$sim[,(max(idx)+1):ncol(fc[[y]]$sim)] # ages 0 to 7+
# note only 8 ages for logF because the last 2 ages were configured to have the same F:
fit$conf$keyLogFsta[1,]
# natural scale :
caa <- fc[[y]]$catchatage

logN<-as.data.frame(logN)
names(logN)<-2:12
N<-exp(logN)
N<-apply(N,2,median)

logF<-as.data.frame(logF)
names(logF)<-2:11
F.<-exp(logF)
F.<-apply(F.,2,median)

Caa <- apply(caa,1,median)

res <- list(N=N,F.=F.,caa=Caa)

})


Ns <- rbind(sims[[1]]$N,rbind(sims[[2]]$N,sims[[3]]$N))
rownames(Ns) <- as.character(2026:2028)
Ns<- FLQuant(t(Ns),dimnames=list(year= 2026:2028,age=2:12))



Fs <- rbind(sims[[1]]$F.,rbind(sims[[2]]$F.,sims[[3]]$F.))
Fs <- cbind(Fs, Fs[,10])
rownames(Fs) <- as.character(2026:2028)
Fs<- FLQuant(t(Fs),dimnames=list(year= 2026:2028,age=2:12))


caa   <- rbind(sims[[1]]$caa,rbind(sims[[2]]$caa,sims[[3]]$caa))
rownames(caa) <- as.character(2026:2028)
caa<- FLQuant(t(caa),dimnames=list(year= 2026:2028,age=2:12))


library(FLash)
library(FLAssess)

Mac.stf <- stf(window(Mac,end = 2025), nyears=3,wts.nyears=3,arith.mean=TRUE,na.rm=TRUE,fbar.nyears=5)
stock.n(Mac.stf)[,ac(2026:2028)] <- Ns
harvest(Mac.stf)[,ac(2026:2028)] <- Fs
catch.n(Mac.stf)[,ac(2026:2028)] <- caa
Mac.stf<-window(Mac.stf, start = 2022)



SSBatage <- stock.wt(Mac.stf) * stock.n(Mac.stf) * exp(- m.spwn(Mac.stf) * m(Mac.stf) - harvest.spwn(Mac.stf)*harvest(Mac.stf)) * mat(Mac.stf)
Catage  <- catch.n(Mac.stf) * catch.wt(Mac.stf)


SSBatage <- as.data.frame(SSBatage,drop = T)
SSBatage$quant <- "SSB"
Catage<-  as.data.frame(Catage, drop=T)
Catage$quant <- "Catches"
res<-rbind(SSBatage,Catage)
res$source <- "SAM"
res$source[res$year == "2027" & res$age == 2] <- "R2027"
res$source[res$year == "2028" & res$age == 3] <- "R2027"
res$source[res$year == "2028" & res$age == 2] <- "R2028"


#catch 2027 plot 
c2027 <- subset(res , quant == "Catches" & year == 2027)

c2027$p <- 100*c2027$data/sum(c2027$data)
c2027$ypos <-  cumsum((c2027$p))  -0.5 * c2027$p
c2027$plab <- paste0("age",c2027$age,": ",round(c2027$p,1),"%")

 

ggplot(c2027 , aes(x="" ,y= p , fill = source)) + 
                      geom_bar(stat= "identity",width = 1 , color= "white") + 
                      coord_polar( "y", start=0 )  +
                      theme_void() + 
                      theme(plot.title = element_text(hjust = 0.5, vjust = -2)) +                       
                      geom_text(aes(label = plab), color = "black", size=3,position = position_stack(vjust = .4))   +
#                      geom_text(aes(y = ypos, label = plab), color = "black", size=3)   +
                      ggtitle("2027 Catches")
                      
#SSB 2028 plot 

ssb28 <- subset(res , quant == "SSB" & year == 2028)

ssb28$p <- 100*ssb28$data/sum(ssb28$data)
ssb28$ypos <-  cumsum((ssb28$p)) - 0.5 * ssb28$p
ssb28$plab <- paste0("age",ssb28$age,": ",round(ssb28$p,1),"%")

ggplot(ssb28 , aes(x="" ,y= p , fill = source)) + 
                      geom_bar(stat= "identity",width = 1 , color= "white") + 
                      coord_polar( "y", start= 0 )  +
                      theme_void() +   
                      theme(plot.title = element_text(hjust = 0.5, vjust = -2)) + 
                      geom_text(aes( label = plab), color = "black", size=3,position = position_stack(vjust = .4))   +
                      ggtitle("2028 SSB")                      
                      
                      