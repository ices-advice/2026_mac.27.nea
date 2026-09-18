

library(mse)

load("./output/Mac_FLStock 0.01 .RData")




###### additional part on change point in recruitment
R<-rec(Mac)

plot(R) + geom_vline(xintercept = c(2014,2017),linetype= 2)

SSB<-ssb(Mac)

RSSB<-R[,ac(2000:2026)] / SSB[,ac(1998:2024)]



library(patchwork)
q<-plot(R)    + ylab("Rec") + geom_point(aes(colour=year>=2004,size=3))
f<-plot(RSSB)    + ylab("R/SSB") + geom_point(aes(colour=year>=2004,size=3))

q/f



SR<-as.FLSR(window(Mac,start = 2015))
model(SR) <- segreg()
SRf<-fmle (SR, fixed = list(b = 3067017))
plot(SRf)



# look at change in mean value
rec<-as.data.frame(rec(Mac) [,ac(2004:2026)])
rps<-as.data.frame(RSSB [,ac(2004:2026)])




rec$part <- "before"
rec$break.year<-1

res<-data.frame(year=NA,meanbefore=NA,meanafter=NA,p.val=NA)
alldats<-subset(rec , year == 2000)


for (yr in c(2010:2020))

{
before <- subset(rec,year<yr)
after  <- subset(rec,year>=yr)
dt<-rec
dt$part[dt$year>=yr] <- "after"
dt$break.year <- yr
alldats<-rbind(alldats,dt)
test <- t.test(before$data,after$data,var.equal=T)

res<-rbind(res, c(yr, test$estimate[1]     , test$estimate[2] , test$p.value))
}

res<-res[-1,]

res$mean.diff <- res[,3]  - res[,4]

ggplot(rec, aes(year,data)) + geom_line()
ggplot(res , aes(year , mean.diff)) + geom_line()
ggplot(res , aes(year , p.val)) + geom_line() + geom_point(aes(colour = p.val<0.05))



library(patchwork)
f<- ggplot(res , aes(year , p.val)) +
              geom_line() +
              geom_point(aes(colour = p.val<0.05))+
              scale_x_continuous(breaks = 2010:2020) +
              ggtitle("p.value t test")   +
              xlab('break point year')
              
g <- ggplot(alldats , aes(factor(break.year),data,fill=part)) +
            geom_boxplot()  +
            xlab('break point year')
            
f/g


library(changepoint)
#Binary Segmentation#
#Binary Segmentation is a recursive algorithm for detecting multiple changes:
#
#It searches the whole series for the strongest evidence of a change in mean.
#It splits the series at that changepoint.
#It searches each resulting segment for another changepoint.
#It continues recursively until the stopping criterion is reached.

cp<-cpt.mean(rec$data,method="BinSeg")
cpts(cp)
plot(cp)
summary(cp)
rec$year[c(13,3,10,8,5)]  # last year of the first segment

cp<-cpt.mean(rps$data,method="BinSeg")
cpts(cp)
plot(cp)
summary(cp)
rec$year[c(10)]  # last year of the first segment



 
cp.amoc <- cpt.mean(rec$data, method = "AMOC")   # finds the single best change point
plot(cp.amoc)


cp.amoc <- cpt.mean(rps$data, method = "AMOC")   # finds the single best change point
plot(cp.amoc)

cp.pelt  <- cpt.mean(rec$data, method = "PELT")  # Finds multiple changepoints efficiently using a penalised likelihood
plot(cp.pelt)
cp.pelt  <- cpt.mean(rps$data, method = "PELT")  # Finds multiple changepoints efficiently using a penalised likelihood
plot(cp.pelt)

cp.sgn  <- cpt.mean(rec$data, method = "SegNeigh", penalty = "BIC")
plot(cp.sgn)
summary(cp.sgn)
rec$year[c(3, 8, 10, 13)]

cp.sgn  <- cpt.mean(rps$data, method = "SegNeigh", penalty = "BIC")
plot(cp.sgn)



library(Rbeast)
out=beast(rec$data, season='none')
plot(out)
print(out)
cp.years<- rec$year[out$trend$cp]

 out$trend$cpOccPr


library(Rbeast)
out=beast(rps$data, season='none')
plot(out)
print(out)
cp.years<- rec$year[out$trend$cp]

 out$trend$cpOccPr



library(zoo)
ggplot(rec, aes(year, data)) +
  geom_point(position=position_jitter(1,3), pch=21, fill="#FF0000AA") +
  geom_line(aes(y=rollmean(data, 3, na.pad=TRUE))) +
  theme_bw()   +
  geom_vline(xintercept = c(2014,2017))
  
  
  
install.packages("bcp")
library(bcp)

res <- bcp(rec$data)
plot(res)
res

rec$year[c(10,8,3,13)] +1

res <- bcp(rps$data)
plot(res)
res
rps$year[c(10,5,1,3)] +1
