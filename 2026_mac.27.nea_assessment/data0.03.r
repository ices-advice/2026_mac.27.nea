## Preprocess data, write TAF data tables

## Before:
## After:
 library(icesTAF)


mkdir("data")
taf.library(stockassessment)

read.ices.taf <- function(...) {
  read.ices(taf.data.path(...))
}


data.path <- "input_data_2026WGWIDE"

cn<-read.ices.taf(paste0(data.path,"./cn.dat"))
cw<-read.ices.taf(paste0(data.path,"./cw.dat"))
dw<-read.ices.taf(paste0(data.path,"./dw.dat"))
lf<-read.ices.taf(paste0(data.path,"./lf.dat"))
lw<-read.ices.taf(paste0(data.path,"./lw.dat"))
mo<-read.ices.taf(paste0(data.path,"./mo.dat"))
nm<-read.ices.taf(paste0(data.path,"./nm.dat"))
pf<-read.ices.taf(paste0(data.path,"./pf.dat"))
pm<-read.ices.taf(paste0(data.path,"./pm.dat"))
sw<-read.ices.taf(paste0(data.path,"./sw.dat"))
surveys<-read.ices.taf(paste0(data.path,"./survey.dat"))


# update the sw table with the values not based on MEGS samples
sw[c('2025'),]  <- c(0.227902637,	0.246357896,	0.287991028,	0.321211354,	0.344464952,	0.390293291,	0.403353339,	0.404241389,	0.424754393	,0.44663066	,0.479781642)
sw[c('2026'),]  <- c(0.227902637,	0.246357896,	0.287991028,	0.321211354,	0.344464952,	0.390293291,	0.403353339,	0.404241389,	0.424754393	,0.44663066	,0.479781642)



# the rest is normal
recap<-read.table(taf.data.path(paste0(data.path,"./tag_steel.dat")), header=TRUE)
recap<-recap[recap$Type==1 & recap$RecaptureY<=2006,]
recap<-recap[recap[,1]>=min(as.numeric(rownames(sw))), ]

age<- recap$ReleaseY - recap$Yearclass
recap  <- recap[age>1 & age<12,]     # possibly test id <13 ok???

#### Remove the tags recapture in the release year
recap <- recap[recap$ReleaseY!=recap$RecaptureY,]

# remove scans of catch in the current Y
recap <- recap[recap$RecaptureY!=2025,]     
recap$R <- as.numeric(recap$R)

#- Se weights
W<-matrix(NA,nrow=nrow(cn), ncol=ncol(cn))
W[as.numeric(rownames(cn))<2000]<-10
attr(cn,"weight")<-W


#recap$splitPhi=rep(1,nrow(recap))    # that is actually part of model configuration, but not included in the conf file

dat<-setup.sam.data(surveys=surveys,
                    residual.fleet=cn, 
                    prop.mature=mo, 
                    stock.mean.weight=sw, 
                    catch.mean.weight=cw, 
                    dis.mean.weight=dw, 
                    land.mean.weight=lw,
                    prop.f=pf, 
                    prop.m=pm, 
                    natural.mortality=nm, 
                    land.frac=lf,
                    recapture=recap)

save(dat,file=paste0("./data/",data.path,".RData"))
