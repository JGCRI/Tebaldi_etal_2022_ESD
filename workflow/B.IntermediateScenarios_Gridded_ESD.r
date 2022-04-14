# This script assumes the existence of pairs of netCDF files of monthly values for tas/pr/psl for scenario SSP2-4.5 or SSP3-7.0;
#both the stitched and the target realizations contain only the 2015-2100 period.

remove(list=ls())
library(maps)
library(class)
library(ncdf4)
library(RColorBrewer)
library(fields)
library(xtable)

"%&%"<-function(x,y)paste0(x,y)
tim.colors.onlyhot<-function(n,alpha=1){
    return(tim.colors(2*n, alpha=alpha)[n+1:n])
}

taper<-function(x,q1=0.0,q2=0.999){
    x1<-quantile(x,q1)
    x2<-quantile(x,q2)
    x[x<x1]<-x1
    x[x>x2]<-x2
    return(x)}

#psl
inputdir<-"../paper_output/GRIDDED/tas_psl_pr/"
inputdirstitched<-inputdir%&%"stitched/"  #emulated trajectories
inputdirtarget<-inputdir%&%"comparison/"  #target data
stitched<-system("ls "%&%inputdirstitched,intern=TRUE)
target<-system("ls "%&%inputdirtarget,intern=TRUE)

ss<-grep("245",stitched)
tt<-grep("ssp245",target)
stitched.ssp245<-stitched[ss][c(1:4,7,10,5,8,11,6,9,12)]
target.ssp245<-target[tt][-c(4:6)]
ss<-grep("ssp370",stitched)
tt<-grep("ssp370",target)
stitched.ssp370<-stitched[ss][c(1:4,7,10,5,8,11,6,9,12)]
target.ssp370<-target[tt][-c(4:6)]
stitched<-c(stitched.ssp245,stitched.ssp370)
target<-c(target.ssp245,target.ssp370)


pslfiles<-grep("psl",stitched)
pslfiles.stitched<-stitched[pslfiles]
pslfiles.target<-target[pslfiles]
cbind(pslfiles.stitched,pslfiles.target)
counter<--1
for(file in pslfiles.stitched){
    model<-substr(file,10,10)
    model<-ifelse(model=="M","MIROC6","CAMS")
    scenario<-ifelse(length(grep("ssp245",file))>0,"ssp245","ssp370")
    if(model=="MIROC6")counter<-counter+1
        ncfile<-nc_open(inputdirstitched%&%file)

    assign("lat."%&%model,ncvar_get(ncfile,"lat"))
    assign("lon."%&%model,ncvar_get(ncfile,"lon"))
    temp<-ncvar_get(ncfile,"psl")
    dd<-dim(temp)
    assign("psl."%&%paste(model, scenario,ifelse(model=="MIROC6",(counter%%3+1)%&%".stitched","stitched"),sep="."),
           matrix(aperm(temp,c(3,1,2)),dd[3],dd[1]*dd[2]))
    nc_close(ncfile)
    print("psl."%&%paste(model, scenario,ifelse(model=="MIROC6",(counter%%3+1)%&%".stitched","stitched"),sep="."))}


counter<--1
for(file in pslfiles.target){
    model<-substr(file,19,19)
    model<-ifelse(model=="M","MIROC6","CAMS")
    scenario<-ifelse(length(grep("ssp245",file))>0,"ssp245","ssp370")
    if(model=="MIROC6")counter<-counter+1
    ncfile<-nc_open(inputdirtarget%&%file)

    temp<-ncvar_get(ncfile,"psl")
    dd<-dim(temp)
    assign("psl."%&%paste(model, scenario,ifelse(model=="MIROC6",(counter%%3+1)%&%".target","target"),sep="."),
           matrix(aperm(temp,c(3,1,2)),dd[3],dd[1]*dd[2]))
    nc_close(ncfile)
    print("psl."%&%paste(model, scenario,ifelse(model=="MIROC6",(counter%%3+1)%&%".target","target"),sep="."))}





collect.model.scen<-c("CAMS.ssp245","CAMS.ssp370",
                      "MIROC6.ssp245.1","MIROC6.ssp245.2","MIROC6.ssp245.3",
                      "MIROC6.ssp370.1","MIROC6.ssp370.2","MIROC6.ssp370.3")


tahiti.lat<--17.40
tahiti.lon<--149.25+360
darwin.lon<-130.50
darwin.lat<--12.26

grid.CAMS<-expand.grid(list(lon=lon.CAMS,lat=lat.CAMS))
tahiti.point.CAMS<-as.numeric(knn1(grid.CAMS,c(tahiti.lon,tahiti.lat),1:nrow(grid.CAMS)))
darwin.point.CAMS<-as.numeric(knn1(grid.CAMS,c(darwin.lon,darwin.lat),1:nrow(grid.CAMS)))

grid.MIROC6<-expand.grid(list(lon=lon.MIROC6,lat=lat.MIROC6))
tahiti.point.MIROC6<-as.numeric(knn1(grid.MIROC6,c(tahiti.lon,tahiti.lat),1:nrow(grid.MIROC6)))
darwin.point.MIROC6<-as.numeric(knn1(grid.MIROC6,c(darwin.lon,darwin.lat),1:nrow(grid.MIROC6)))


for(coup in collect.model.scen){
    model<-ifelse(substr(coup,1,1)=="M","MIROC6","CAMS")
    tahiti.point<-get("tahiti.point."%&%model)
    darwin.point<-get("darwin.point."%&%model)
    assign("psl.tahitidarwin."%&%coup%&%".stitched",get("psl."%&%coup%&%".stitched")[,c(tahiti.point,darwin.point)])
    assign("psl.tahitidarwin."%&%coup%&%".target",get("psl."%&%coup%&%".target")[,c(tahiti.point,darwin.point)])}


compute.soi<-function(pslts){
    nn<-nrow(pslts)
    mm<-apply(pslts,2,mean)
    sders<-apply(pslts,2,sd)*(nn-1)/nn
    standardizedtahiti<-(pslts[,1]-mm[1])/sders[1]
    standardizeddarwin<-(pslts[,2]-mm[2])/sders[2]
    msd<-sqrt((1/nrow(pslts))*sum((standardizedtahiti-standardizeddarwin)^2))

    return(list(tahiti=standardizedtahiti,darwin=standardizeddarwin,soi=(standardizedtahiti-standardizeddarwin)/msd))}

(oo<-objects(pattern="psl.tahitidarwin"))

for(obj in oo){
    newname<-gsub("psl","soi",obj)
    print(newname)
    assign(newname,compute.soi(get(obj)))
}


pickmonths<-function(period,subperiod){

    monthlyperiod<-cbind(1:(12*length(period)),rep(1:12,length(period)),rep(period,rep(12,length(period))))
    submonthlyperiod<-monthlyperiod[,1][monthlyperiod[,3]>=subperiod[1]&monthlyperiod[,3]<=rev(subperiod)[1]]
    return(submonthlyperiod)
}



ylim=c(-3,3)
picdir<-"../paperpics/"


model<-"CAMS"

soi.ssp245.target<-get("soi.tahitidarwin."%&%model%&%".ssp245.target")$soi
soi.ssp245.stitched<-get("soi.tahitidarwin."%&%model%&%".ssp245.stitched")$soi
soi.ssp370.target<-get("soi.tahitidarwin."%&%model%&%".ssp370.target")$soi
soi.ssp370.stitched<-get("soi.tahitidarwin."%&%model%&%".ssp370.stitched")$soi


subperiods<-list(subperiod1=2015:2034,subperiod2=2035:2054,subperiod3=2081:ifelse(model=="CAMS",2099,2100))

for(i in 1:3){

    subperiod<-subperiods[[i]]
    xindex<-rep(subperiod,rep(12,length(subperiod)))+rep(seq(12)/13,length(subperiod))
    subset.stitched<-pickmonths(2015:ifelse(model=="CAMS",2099,2100),subperiod)
    subset.target<-pickmonths(2015:ifelse(model=="CAMS",2099,2100),subperiod)

    periodtext<-paste(range(subperiod),collapse="-")

    jpeg(picdir%&%"soi_targetandstitched_"%&%model%&%"_1_"%&%periodtext%&%".jpg",quality=100,width=800,height=800)
    par(mfrow=c(2,2),cex.axis=2,cex.lab=1.5,cex.main=2)

    plot(xindex,soi.ssp245.target[subset.target],type="n",las=1,xlab="",ylab="SOI",ylim=ylim,
         main="SOI in "%&%model%&%", SSP2-4.5 target\n"%&%periodtext)
    abline(h=0)
    segments(xindex,0,xindex,soi.ssp245.target[subset.target],lwd=2,
             col=ifelse(soi.ssp245.target[subset.target]>0,"royalblue1","red1"))

    plot(xindex,soi.ssp245.stitched[subset.stitched],type="n",las=1,xlab="",ylab="SOI",ylim=ylim,
         main="SOI in "%&%model%&%", SSP2-4.5 stitched\n"%&%periodtext)
    abline(h=0)
    segments(xindex,0,xindex,soi.ssp245.stitched[subset.stitched],lwd=2,
             col=ifelse(soi.ssp245.stitched[subset.stitched]>0,"royalblue1","red1"))



    plot(xindex,soi.ssp370.target[subset.target],type="n",las=1,xlab="",ylab="SOI",ylim=ylim,
         main="SOI in "%&%model%&%", SSP3-7.0 target \n"%&%periodtext)
    abline(h=0)
    segments(xindex,0,xindex,soi.ssp370.target[subset.target],lwd=2,
             col=ifelse(soi.ssp370.target[subset.target]>0,"royalblue1","red1"))

    plot(xindex,soi.ssp370.stitched[subset.stitched],type="n",las=1,xlab="",ylab="SOI",ylim=ylim,
         main="SOI in "%&%model%&%", SSP3-7.0 stitched\n"%&%periodtext)
    abline(h=0)
    segments(xindex,0,xindex,soi.ssp370.stitched[subset.stitched],lwd=2,
             col=ifelse(soi.ssp370.stitched[subset.stitched]>0,"royalblue1","red1"))


    dev.off()

}




jpeg(picdir%&%"soi_acf_pacf_targetandstitched_"%&%model%&%".jpg",quality=100,height=1600,width=800)
par(mfrow=c(4,2),cex.axis=2,cex.lab=1.5,cex.main=2)
acf(soi.ssp245.target,main="SOI ACF in "%&%model%&%", SSP2-4.5, target",las=1,xlim=c(0,30),ylim=c(-0.5,1))
acf(soi.ssp245.stitched,main="SOI ACF in "%&%model%&%", SSP2-4.5, stitched",las=1,xlim=c(0,30),ylim=c(-0.5,1))

pacf(soi.ssp245.target,main="SOI PACF in "%&%model%&%", SSP2-4.5, target",las=1,xlim=c(0,30),ylim=c(-0.2,0.8))
pacf(soi.ssp245.stitched,main="SOI PACF in "%&%model%&%", SSP2-4.5, stitched",las=1,xlim=c(0,30),ylim=c(-0.2,0.8))

acf(soi.ssp370.target,main="SOI ACF in "%&%model%&%", SSP3-7.0, target",las=1,xlim=c(0,30),ylim=c(-0.5,1))
acf(soi.ssp370.stitched,main="SOI ACF in "%&%model%&%", SSP3-7.0, stitched",las=1,xlim=c(0,30),ylim=c(-0.5,1))

pacf(soi.ssp370.target,main="SOI PACF in "%&%model%&%", SSP3-7.0, target",las=1,xlim=c(0,30),ylim=c(-0.2,0.8))
pacf(soi.ssp370.stitched,main="SOI PACF in "%&%model%&%", SSP3-7.0, stitched",las=1,xlim=c(0,30),ylim=c(-0.2,0.8))


dev.off()

model<-"MIROC6"

soi.ssp245.target<-get("soi.tahitidarwin."%&%model%&%".ssp245.1.target")$soi
soi.ssp245.stitched<-get("soi.tahitidarwin."%&%model%&%".ssp245.1.stitched")$soi
soi.ssp370.target<-get("soi.tahitidarwin."%&%model%&%".ssp370.1.target")$soi
soi.ssp370.stitched<-get("soi.tahitidarwin."%&%model%&%".ssp370.1.stitched")$soi


subperiods<-list(subperiod1=2015:2034,subperiod2=2035:2054,subperiod3=2081:ifelse(model=="CAMS",2099,2100))

for(i in 1:3){

subperiod<-subperiods[[i]]
xindex<-rep(subperiod,rep(12,length(subperiod)))+rep(seq(12)/13,length(subperiod))
subset.stitched<-pickmonths(2015:ifelse(model=="CAMS",2099,2100),subperiod)
subset.target<-pickmonths(2015:ifelse(model=="CAMS",2099,2100),subperiod)

periodtext<-paste(range(subperiod),collapse="-")

jpeg(picdir%&%"soi_targetandstitched_"%&%model%&%"_1_"%&%periodtext%&%".jpg",quality=100,width=800,height=800)
par(mfrow=c(2,2),cex.axis=2,cex.lab=1.5,cex.main=2)

plot(xindex,soi.ssp245.target[subset.target],type="n",las=1,xlab="",ylab="SOI",ylim=ylim,
     main="SOI in "%&%model%&%", SSP2-4.5 target\n"%&%periodtext)
abline(h=0)
segments(xindex,0,xindex,soi.ssp245.target[subset.target],lwd=2,
         col=ifelse(soi.ssp245.target[subset.target]>0,"royalblue1","red1"))

plot(xindex,soi.ssp245.stitched[subset.stitched],type="n",las=1,xlab="",ylab="SOI",ylim=ylim,
     main="SOI in "%&%model%&%", SSP2-4.5 stitched\n"%&%periodtext)
abline(h=0)
segments(xindex,0,xindex,soi.ssp245.stitched[subset.stitched],lwd=2,
         col=ifelse(soi.ssp245.stitched[subset.stitched]>0,"royalblue1","red1"))



plot(xindex,soi.ssp370.target[subset.target],type="n",las=1,xlab="",ylab="SOI",ylim=ylim,
     main="SOI in "%&%model%&%", SSP3-7.0 target \n"%&%periodtext)
abline(h=0)
segments(xindex,0,xindex,soi.ssp370.target[subset.target],lwd=2,
         col=ifelse(soi.ssp370.target[subset.target]>0,"royalblue1","red1"))

plot(xindex,soi.ssp370.stitched[subset.stitched],type="n",las=1,xlab="",ylab="SOI",ylim=ylim,
     main="SOI in "%&%model%&%", SSP3-7.0 stitched\n"%&%periodtext)
abline(h=0)
segments(xindex,0,xindex,soi.ssp370.stitched[subset.stitched],lwd=2,
         col=ifelse(soi.ssp370.stitched[subset.stitched]>0,"royalblue1","red1"))


dev.off()

}




jpeg(picdir%&%"soi_acf_pacf_targetandstitched_"%&%model%&%"_1.jpg",quality=100,height=1600,width=800)
par(mfrow=c(4,2),cex.axis=2,cex.lab=1.5,cex.main=2)
acf(soi.ssp245.target,main="SOI ACF in "%&%model%&%", SSP2-4.5, target",las=1,xlim=c(0,30),ylim=c(-0.6,1))
acf(soi.ssp245.stitched,main="SOI ACF in "%&%model%&%", SSP2-4.5, stitched",las=1,xlim=c(0,30),ylim=c(-0.6,1))

pacf(soi.ssp245.target,main="SOI PACF in "%&%model%&%", SSP2-4.5, target",las=1,xlim=c(0,30),ylim=c(-0.3,0.8))
pacf(soi.ssp245.stitched,main="SOI PACF in "%&%model%&%", SSP2-4.5, stitched",las=1,xlim=c(0,30),ylim=c(-0.3,0.8))

acf(soi.ssp370.target,main="SOI ACF in "%&%model%&%", SSP3-7.0, target",las=1,xlim=c(0,30),ylim=c(-0.6,1))
acf(soi.ssp370.stitched,main="SOI ACF in "%&%model%&%", SSP3-7.0, stitched",las=1,xlim=c(0,30),ylim=c(-0.6,1))

pacf(soi.ssp370.target,main="SOI PACF in "%&%model%&%", SSP3-7.0, target",las=1,xlim=c(0,30),ylim=c(-0.3,0.8))
pacf(soi.ssp370.stitched,main="SOI PACF in "%&%model%&%", SSP3-7.0, stitched",las=1,xlim=c(0,30),ylim=c(-0.3,0.8))


dev.off()


model<-"MIROC6"

soi.ssp245.target<-get("soi.tahitidarwin."%&%model%&%".ssp245.2.target")$soi
soi.ssp245.stitched<-get("soi.tahitidarwin."%&%model%&%".ssp245.2.stitched")$soi
soi.ssp370.target<-get("soi.tahitidarwin."%&%model%&%".ssp370.2.target")$soi
soi.ssp370.stitched<-get("soi.tahitidarwin."%&%model%&%".ssp370.2.stitched")$soi


subperiods<-list(subperiod1=2015:2034,subperiod2=2035:2054,subperiod3=2081:ifelse(model=="CAMS",2099,2100))

for(i in 1:3){

    subperiod<-subperiods[[i]]
    xindex<-rep(subperiod,rep(12,length(subperiod)))+rep(seq(12)/13,length(subperiod))
    subset.stitched<-pickmonths(2015:ifelse(model=="CAMS",2099,2100),subperiod)
    subset.target<-pickmonths(2015:ifelse(model=="CAMS",2099,2100),subperiod)

    periodtext<-paste(range(subperiod),collapse="-")

    jpeg(picdir%&%"soi_targetandstitched_"%&%model%&%"_2_"%&%periodtext%&%".jpg",quality=100,width=800,height=800)
    par(mfrow=c(2,2),cex.axis=2,cex.lab=1.5,cex.main=2)

    plot(xindex,soi.ssp245.target[subset.target],type="n",las=1,xlab="",ylab="SOI",ylim=ylim,
         main="SOI in "%&%model%&%", SSP2-4.5 target\n"%&%periodtext)
    abline(h=0)
    segments(xindex,0,xindex,soi.ssp245.target[subset.target],lwd=2,
             col=ifelse(soi.ssp245.target[subset.target]>0,"royalblue1","red1"))

    plot(xindex,soi.ssp245.stitched[subset.stitched],type="n",las=1,xlab="",ylab="SOI",ylim=ylim,
         main="SOI in "%&%model%&%", SSP2-4.5 stitched\n"%&%periodtext)
    abline(h=0)
    segments(xindex,0,xindex,soi.ssp245.stitched[subset.stitched],lwd=2,
             col=ifelse(soi.ssp245.stitched[subset.stitched]>0,"royalblue1","red1"))



    plot(xindex,soi.ssp370.target[subset.target],type="n",las=1,xlab="",ylab="SOI",ylim=ylim,
         main="SOI in "%&%model%&%", SSP3-7.0 target \n"%&%periodtext)
    abline(h=0)
    segments(xindex,0,xindex,soi.ssp370.target[subset.target],lwd=2,
             col=ifelse(soi.ssp370.target[subset.target]>0,"royalblue1","red1"))

    plot(xindex,soi.ssp370.stitched[subset.stitched],type="n",las=1,xlab="",ylab="SOI",ylim=ylim,
         main="SOI in "%&%model%&%", SSP3-7.0 stitched\n"%&%periodtext)
    abline(h=0)
    segments(xindex,0,xindex,soi.ssp370.stitched[subset.stitched],lwd=2,
             col=ifelse(soi.ssp370.stitched[subset.stitched]>0,"royalblue1","red1"))


    dev.off()

}




jpeg(picdir%&%"soi_acf_pacf_targetandstitched_"%&%model%&%"_2.jpg",quality=100,height=1600,width=800)
par(mfrow=c(4,2),cex.axis=2,cex.lab=1.5,cex.main=2)
acf(soi.ssp245.target,main="SOI ACF in "%&%model%&%", SSP2-4.5, target",las=1,xlim=c(0,30),ylim=c(-0.6,1))
acf(soi.ssp245.stitched,main="SOI ACF in "%&%model%&%", SSP2-4.5, stitched",las=1,xlim=c(0,30),ylim=c(-0.6,1))

pacf(soi.ssp245.target,main="SOI PACF in "%&%model%&%", SSP2-4.5, target",las=1,xlim=c(0,30),ylim=c(-0.3,0.8))
pacf(soi.ssp245.stitched,main="SOI PACF in "%&%model%&%", SSP2-4.5, stitched",las=1,xlim=c(0,30),ylim=c(-0.3,0.8))

acf(soi.ssp370.target,main="SOI ACF in "%&%model%&%", SSP3-7.0, target",las=1,xlim=c(0,30),ylim=c(-0.6,1))
acf(soi.ssp370.stitched,main="SOI ACF in "%&%model%&%", SSP3-7.0, stitched",las=1,xlim=c(0,30),ylim=c(-0.6,1))

pacf(soi.ssp370.target,main="SOI PACF in "%&%model%&%", SSP3-7.0, target",las=1,xlim=c(0,30),ylim=c(-0.3,0.8))
pacf(soi.ssp370.stitched,main="SOI PACF in "%&%model%&%", SSP3-7.0, stitched",las=1,xlim=c(0,30),ylim=c(-0.3,0.8))


dev.off()


model<-"MIROC6"

soi.ssp245.target<-get("soi.tahitidarwin."%&%model%&%".ssp245.3.target")$soi
soi.ssp245.stitched<-get("soi.tahitidarwin."%&%model%&%".ssp245.3.stitched")$soi
soi.ssp370.target<-get("soi.tahitidarwin."%&%model%&%".ssp370.3.target")$soi
soi.ssp370.stitched<-get("soi.tahitidarwin."%&%model%&%".ssp370.3.stitched")$soi


subperiods<-list(subperiod1=2015:2034,subperiod2=2035:2054,subperiod3=2081:ifelse(model=="CAMS",2099,2100))

for(i in 1:3){

    subperiod<-subperiods[[i]]
    xindex<-rep(subperiod,rep(12,length(subperiod)))+rep(seq(12)/13,length(subperiod))
    subset.stitched<-pickmonths(2015:ifelse(model=="CAMS",2099,2100),subperiod)
    subset.target<-pickmonths(2015:ifelse(model=="CAMS",2099,2100),subperiod)

    periodtext<-paste(range(subperiod),collapse="-")

    jpeg(picdir%&%"soi_targetandstitched_"%&%model%&%"_3_"%&%periodtext%&%".jpg",quality=100,width=800,height=800)
    par(mfrow=c(2,2),cex.axis=2,cex.lab=1.5,cex.main=2)

    plot(xindex,soi.ssp245.target[subset.target],type="n",las=1,xlab="",ylab="SOI",ylim=ylim,
         main="SOI in "%&%model%&%", SSP2-4.5 target\n"%&%periodtext)
    abline(h=0)
    segments(xindex,0,xindex,soi.ssp245.target[subset.target],lwd=2,
             col=ifelse(soi.ssp245.target[subset.target]>0,"royalblue1","red1"))

    plot(xindex,soi.ssp245.stitched[subset.stitched],type="n",las=1,xlab="",ylab="SOI",ylim=ylim,
         main="SOI in "%&%model%&%", SSP2-4.5 stitched\n"%&%periodtext)
    abline(h=0)
    segments(xindex,0,xindex,soi.ssp245.stitched[subset.stitched],lwd=2,
             col=ifelse(soi.ssp245.stitched[subset.stitched]>0,"royalblue1","red1"))



    plot(xindex,soi.ssp370.target[subset.target],type="n",las=1,xlab="",ylab="SOI",ylim=ylim,
         main="SOI in "%&%model%&%", SSP3-7.0 target \n"%&%periodtext)
    abline(h=0)
    segments(xindex,0,xindex,soi.ssp370.target[subset.target],lwd=2,
             col=ifelse(soi.ssp370.target[subset.target]>0,"royalblue1","red1"))

    plot(xindex,soi.ssp370.stitched[subset.stitched],type="n",las=1,xlab="",ylab="SOI",ylim=ylim,
         main="SOI in "%&%model%&%", SSP3-7.0 stitched\n"%&%periodtext)
    abline(h=0)
    segments(xindex,0,xindex,soi.ssp370.stitched[subset.stitched],lwd=2,
             col=ifelse(soi.ssp370.stitched[subset.stitched]>0,"royalblue1","red1"))


    dev.off()

}




jpeg(picdir%&%"soi_acf_pacf_targetandstitched_"%&%model%&%"_3.jpg",quality=100,height=1600,width=800)
par(mfrow=c(4,2),cex.axis=2,cex.lab=1.5,cex.main=2)
acf(soi.ssp245.target,main="SOI ACF in "%&%model%&%", SSP2-4.5, target",las=1,xlim=c(0,30),ylim=c(-0.6,1))
acf(soi.ssp245.stitched,main="SOI ACF in "%&%model%&%", SSP2-4.5, stitched",las=1,xlim=c(0,30),ylim=c(-0.6,1))

pacf(soi.ssp245.target,main="SOI PACF in "%&%model%&%", SSP2-4.5, target",las=1,xlim=c(0,30),ylim=c(-0.3,0.8))
pacf(soi.ssp245.stitched,main="SOI PACF in "%&%model%&%", SSP2-4.5, stitched",las=1,xlim=c(0,30),ylim=c(-0.3,0.8))

acf(soi.ssp370.target,main="SOI ACF in "%&%model%&%", SSP3-7.0, target",las=1,xlim=c(0,30),ylim=c(-0.6,1))
acf(soi.ssp370.stitched,main="SOI ACF in "%&%model%&%", SSP3-7.0, stitched",las=1,xlim=c(0,30),ylim=c(-0.6,1))

pacf(soi.ssp370.target,main="SOI PACF in "%&%model%&%", SSP3-7.0, target",las=1,xlim=c(0,30),ylim=c(-0.3,0.8))
pacf(soi.ssp370.stitched,main="SOI PACF in "%&%model%&%", SSP3-7.0, stitched",las=1,xlim=c(0,30),ylim=c(-0.3,0.8))


dev.off()

###trends

compare.trends<-function(target,stitched){
    lillot<-lm(target~seq(length(target)))
    trendt<-summary(lillot)$coef[2,1:2]
    lot<-trendt[1]-2*trendt[2]
    upt<-trendt[1]+2*trendt[2]
    lillos<-lm(stitched~seq(length(stitched)))
    trends<-summary(lillos)$coef[2,1:2]
    init<-trends[1]>=lot&trends[1]<=upt
    los<-trends[1]-2*trends[2]
    ups<-trends[1]+2*trends[2]
    nooverlap<-(los-upt)>0|(lot-ups)>0
    overlap<-!nooverlap
    temp<-c(trendt[1],trends[1],init,overlap)
    names(temp)<-c("target.trend","stitched.trend","in.it","overlapping")
    return(temp)}


tascolors<-brewer.pal(9,"YlOrRd")
prcolors<-brewer.pal(9,"YlGnBu")
for(member in c("r1")){

stitchedfile<-"stitched_CAMS-CSM1-0_tas_ssp370~"%&%member%&%"i1p1f1~1.nc"
ncfile<-nc_open(inputdirstitched%&%stitchedfile)
lat<-ncvar_get(ncfile,"lat")
lon<-ncvar_get(ncfile,"lon")
tas.stitched<-ncvar_get(ncfile,"tas")
nc_close(ncfile)
grid.CAMS<-expand.grid(list(lon=lon,lat=lat))

targetfile<-"comparison_ssp370_CAMS-CSM1-0_"%&%member%&%"i1p1f1_tas.nc"
ncfile<-nc_open(inputdirtarget%&%targetfile)
tas.target<-ncvar_get(ncfile,"tas")
nc_close(ncfile)

dim(tas.target)
dim(tas.stitched)


trendcomparison.tas.ssp370.CAMS<-array(dim=c(length(lon),length(lat),4))
for(j in seq(length(lon))){
    print(j)
    for(i in seq(length(lat)))
    trendcomparison.tas.ssp370.CAMS[j,i,]<-compare.trends(tas.target[j,i,],tas.stitched[j,i,])
}

jpeg(picdir%&%"tas_trendcomparison_CAMS_"%&%member%&%"_ssp370.jpg",quality=100,width=800,height=600)
temp<-trendcomparison.tas.ssp370.CAMS
image.plot(lon,lat,abs(temp[,,1]-temp[,,2])*12*10,xlab="",ylab="",
           col=tascolors,nlevel=9,
           main="CAMS, SSP370 "%&%member%&%", TAS: abs. differences in trends (C/decade)\n between stitched and target",axis.args=list(cex.axis=1.5),cex.main=2)
points(grid.CAMS,pch=ifelse(c(temp[,,4])==0,"+",""),cex=0.75)
map("world2",add=TRUE,interior=F)
dev.off()

##
stitchedfile<-"stitched_CAMS-CSM1-0_tas_ssp245~"%&%member%&%"i1p1f1~1.nc"
ncfile<-nc_open(inputdirstitched%&%stitchedfile)
lat<-ncvar_get(ncfile,"lat")
lon<-ncvar_get(ncfile,"lon")
tas.stitched<-ncvar_get(ncfile,"tas")
nc_close(ncfile)

targetfile<-"comparison_ssp245_CAMS-CSM1-0_"%&%member%&%"i1p1f1_tas.nc"
ncfile<-nc_open(inputdirtarget%&%targetfile)
tas.target<-ncvar_get(ncfile,"tas")
nc_close(ncfile)

dim(tas.target)
dim(tas.stitched)

trendcomparison.tas.ssp245.CAMS<-array(dim=c(length(lon),length(lat),4))
for(j in seq(length(lon))){
    print(j)
    for(i in seq(length(lat)))
        trendcomparison.tas.ssp245.CAMS[j,i,]<-compare.trends(tas.target[j,i,],tas.stitched[j,i,])
}




###pr

stitchedfile<-"stitched_CAMS-CSM1-0_pr_ssp245~"%&%member%&%"i1p1f1~1.nc"
ncfile<-nc_open(inputdirstitched%&%stitchedfile)
lat<-ncvar_get(ncfile,"lat")
lon<-ncvar_get(ncfile,"lon")
pr.stitched<-ncvar_get(ncfile,"pr")
nc_close(ncfile)

targetfile<-"comparison_ssp245_CAMS-CSM1-0_"%&%member%&%"i1p1f1_pr.nc"
ncfile<-nc_open(inputdirtarget%&%targetfile)
pr.target<-ncvar_get(ncfile,"pr")
nc_close(ncfile)

dim(pr.target)
dim(pr.stitched)



trendcomparison.pr.ssp245.CAMS<-array(dim=c(length(lon),length(lat),4))
for(j in seq(length(lon))[-c(1:177)]){
    print(j)
    for(i in seq(length(lat)))
        trendcomparison.pr.ssp245.CAMS[j,i,]<-compare.trends(pr.target[j,i,],pr.stitched[j,i,])
}

jpeg(picdir%&%"pr_trendcomparison_CAMS_"%&%member%&%"_ssp245.jpg",quality=100,width=800,height=600)
temp<-trendcomparison.pr.ssp245.CAMS
image.plot(lon,lat,86400*abs(temp[,,1]-temp[,,2])*12*10,xlab="",ylab="",
           col=prcolors,nlevel=9,
           main="CAMS, SSP245 "%&%member%&%", pr: abs. differences in trends (mm/day/decade)\n between stitched and target",axis.args=list(cex.axis=1.5),cex.main=2)
points(grid.CAMS,pch=ifelse(c(temp[,,4])==0,"+",""),cex=0.75)
map("world2",add=TRUE,interior=F)
dev.off()



stitchedfile<-"stitched_CAMS-CSM1-0_pr_ssp370~"%&%member%&%"i1p1f1~1.nc"
ncfile<-nc_open(inputdirstitched%&%stitchedfile)
lat<-ncvar_get(ncfile,"lat")
lon<-ncvar_get(ncfile,"lon")
pr.stitched<-ncvar_get(ncfile,"pr")
nc_close(ncfile)

targetfile<-"comparison_ssp370_CAMS-CSM1-0_"%&%member%&%"i1p1f1_pr.nc"
ncfile<-nc_open(inputdirtarget%&%targetfile)
pr.target<-ncvar_get(ncfile,"pr")
nc_close(ncfile)

dim(pr.target)
dim(pr.stitched)


trendcomparison.pr.ssp370.CAMS<-array(dim=c(length(lon),length(lat),4))
for(j in seq(length(lon))){
    print(j)
    for(i in seq(length(lat)))
        trendcomparison.pr.ssp370.CAMS[j,i,]<-compare.trends(pr.target[j,i,],pr.stitched[j,i,])
}

jpeg(picdir%&%"pr_trendcomparison_CAMS_"%&%member%&%"_ssp370.jpg",quality=100,width=800,height=600)
temp<-trendcomparison.pr.ssp370.CAMS
image.plot(lon,lat,86400*abs(temp[,,1]-temp[,,2])*12*10,xlab="",ylab="",
           col=prcolors,nlevel=9,
           main="CAMS, SSP370 "%&%member%&%", PR: abs. differences in trends (mm/day/decade)\n between stitched and target",axis.args=list(cex.axis=1.5),cex.main=2)
points(grid.CAMS,pch=ifelse(c(temp[,,4])==0,"+",""),cex=0.75)
map("world2",add=TRUE,interior=F)
dev.off()
}

#MIROC6
for(member in c("r1","r2","r3")){

stitchedfile<-"stitched_MIROC6_tas_ssp370~"%&%member%&%"i1p1f1~1.nc"
ncfile<-nc_open(inputdirstitched%&%stitchedfile)
lat<-ncvar_get(ncfile,"lat")
lon<-ncvar_get(ncfile,"lon")
tas.stitched<-ncvar_get(ncfile,"tas")
nc_close(ncfile)
grid.MIROC6<-expand.grid(list(lon=lon,lat=lat))

targetfile<-"comparison_ssp370_MIROC6_"%&%member%&%"i1p1f1_tas.nc"
ncfile<-nc_open(inputdirtarget%&%targetfile)
tas.target<-ncvar_get(ncfile,"tas")
nc_close(ncfile)

dim(tas.target)
dim(tas.stitched)


trendcomparison.tas.ssp370.MIROC6<-array(dim=c(length(lon),length(lat),4))
for(j in seq(length(lon))){
    print(j)
    for(i in seq(length(lat)))
        trendcomparison.tas.ssp370.MIROC6[j,i,]<-compare.trends(tas.target[j,i,],tas.stitched[j,i,])
}

jpeg(picdir%&%"tas_trendcomparison_MIROC6_"%&%member%&%"_ssp370.jpg",quality=100,width=800,height=600)
temp<-trendcomparison.tas.ssp370.MIROC6
image.plot(lon,lat,abs(temp[,,1]-temp[,,2])*12*10,xlab="",ylab="",
           col=tascolors,nlevel=9,
           main="MIROC6, SSP370 "%&%member%&%", TAS: abs. differences in trends (C/decade)\n between stitched and target",axis.args=list(cex.axis=1.5),cex.main=2)
points(grid.MIROC6,pch=ifelse(c(temp[,,4])==0,"+",""),cex=0.75)
map("world2",add=TRUE,interior=F)
dev.off()
assign("trendcomparison.tas.ssp370.MIROC6."%&%member,temp)

stitchedfile<-"stitched_MIROC6_tas_ssp245~"%&%member%&%"i1p1f1~1.nc"
ncfile<-nc_open(inputdirstitched%&%stitchedfile)
lat<-ncvar_get(ncfile,"lat")
lon<-ncvar_get(ncfile,"lon")
tas.stitched<-ncvar_get(ncfile,"tas")
nc_close(ncfile)

targetfile<-"comparison_ssp245_MIROC6_"%&%member%&%"i1p1f1_tas.nc"
ncfile<-nc_open(inputdirtarget%&%targetfile)
tas.target<-ncvar_get(ncfile,"tas")
nc_close(ncfile)

dim(tas.target)
dim(tas.stitched)



trendcomparison.tas.ssp245.MIROC6<-array(dim=c(length(lon),length(lat),4))
for(j in seq(length(lon))){
    print(j)
    for(i in seq(length(lat)))
        trendcomparison.tas.ssp245.MIROC6[j,i,]<-compare.trends(tas.target[j,i,],tas.stitched[j,i,])
}

jpeg(picdir%&%"tas_trendcomparison_MIROC6_"%&%member%&%"_ssp245.jpg",quality=100,width=800,height=600)
temp<-trendcomparison.tas.ssp245.MIROC6
image.plot(lon,lat,abs(temp[,,1]-temp[,,2])*12*10,xlab="",ylab="",
           col=tascolors,nlevel=9,
           main="MIROC6, SSP245 "%&%member%&%", TAS: abs. differences in trends (C/decade)\n between stitched and target",axis.args=list(cex.axis=1.5),cex.main=2)
points(grid.MIROC6,pch=ifelse(c(temp[,,4])==0,"+",""),cex=0.75)
map("world2",add=TRUE,interior=F)
dev.off()
assign("trendcomparison.tas.ssp245.MIROC6."%&%member,temp)


###pr

stitchedfile<-"stitched_MIROC6_pr_ssp370~"%&%member%&%"i1p1f1~1.nc"
ncfile<-nc_open(inputdirstitched%&%stitchedfile)
lat<-ncvar_get(ncfile,"lat")
lon<-ncvar_get(ncfile,"lon")
pr.stitched<-ncvar_get(ncfile,"pr")
nc_close(ncfile)

targetfile<-"comparison_ssp370_MIROC6_"%&%member%&%"i1p1f1_pr.nc"
ncfile<-nc_open(inputdirtarget%&%targetfile)
pr.target<-ncvar_get(ncfile,"pr")
nc_close(ncfile)

dim(pr.target)
dim(pr.stitched)

#pr.stitched<-pr.stitched[,,1980+1:1032]



trendcomparison.pr.ssp370.MIROC6<-array(dim=c(length(lon),length(lat),4))
for(j in seq(length(lon))){
    print(j)
    for(i in seq(length(lat)))
        trendcomparison.pr.ssp370.MIROC6[j,i,]<-compare.trends(pr.target[j,i,],pr.stitched[j,i,])
}

jpeg(picdir%&%"pr_trendcomparison_MIROC6_"%&%member%&%"_ssp370.jpg",quality=100,width=800,height=600)
temp<-trendcomparison.pr.ssp370.MIROC6
image.plot(lon,lat,86400*abs(temp[,,1]-temp[,,2])*12*10,xlab="",ylab="",
           col=prcolors,nlevel=9,
           main="MIROC6, SSP370 "%&%member%&%", PR: abs. differences in trends (mm/day/decade)\n between stitched and target",axis.args=list(cex.axis=1.5),cex.main=2)
points(grid.MIROC6,pch=ifelse(c(temp[,,4])==0,"+",""),cex=0.75)
map("world2",add=TRUE,interior=F)
dev.off()
assign("trendcomparison.pr.ssp370.MIROC6."%&%member,temp)

stitchedfile<-"stitched_MIROC6_pr_ssp245~"%&%member%&%"i1p1f1~1.nc"
ncfile<-nc_open(inputdirstitched%&%stitchedfile)
lat<-ncvar_get(ncfile,"lat")
lon<-ncvar_get(ncfile,"lon")
pr.stitched<-ncvar_get(ncfile,"pr")
nc_close(ncfile)

targetfile<-"comparison_ssp245_MIROC6_"%&%member%&%"i1p1f1_pr.nc"
ncfile<-nc_open(inputdirtarget%&%targetfile)
pr.target<-ncvar_get(ncfile,"pr")
nc_close(ncfile)

dim(pr.target)
dim(pr.stitched)


trendcomparison.pr.ssp245.MIROC6<-array(dim=c(length(lon),length(lat),4))
for(j in seq(length(lon))){
    print(j)
    for(i in seq(length(lat)))
        trendcomparison.pr.ssp245.MIROC6[j,i,]<-compare.trends(pr.target[j,i,],pr.stitched[j,i,])
}

jpeg(picdir%&%"pr_trendcomparison_MIROC6_"%&%member%&%"_ssp245.jpg",quality=100,width=800,height=600)

temp<-trendcomparison.pr.ssp245.MIROC6
image.plot(lon,lat,86400*abs(temp[,,1]-temp[,,2])*12*10,xlab="",ylab="",
           col=prcolors,nlevel=9,
           main="MIROC6, SSP245 "%&%member%&%", PR: abs. differences in trends (mm/day/decade)\n between stitched and target",axis.args=list(cex.axis=1.5),cex.main=2)
points(grid.MIROC6,pch=ifelse(c(temp[,,4])==0,"+",""),cex=0.75)
map("world2",add=TRUE,interior=F)
dev.off()
assign("trendcomparison.pr.ssp245.MIROC6."%&%member,temp)
}


###now var test

zlimtas<-c(0,2)
zlimpr<-c(0,2)
breaks<-c(0,0.5,0.8,0.9,1,1.1,1.2,1.5,1.8,2)
colors<-brewer.pal(11,"Spectral")[c(1,2,4,5,6,7,8,10,11)]
for(member in c("r1")){

    stitchedfile<-"stitched_CAMS-CSM1-0_tas_ssp370~"%&%member%&%"i1p1f1~1.nc"
    ncfile<-nc_open(inputdirstitched%&%stitchedfile)
    lat<-ncvar_get(ncfile,"lat")
    lon<-ncvar_get(ncfile,"lon")
    tas.stitched<-ncvar_get(ncfile,"tas")
    nc_close(ncfile)
    grid.CAMS<-expand.grid(list(lon=lon,lat=lat))

    targetfile<-"comparison_ssp370_CAMS-CSM1-0_"%&%member%&%"i1p1f1_tas.nc"
    ncfile<-nc_open(inputdirtarget%&%targetfile)
    tas.target<-ncvar_get(ncfile,"tas")
    nc_close(ncfile)

    dim(tas.target)
    dim(tas.stitched)


    varcomparison.tas.ssp370.CAMS<-array(NA,c(length(lon),length(lat),2))
    for(j in seq(length(lon))){
        print(j)
        for(i in seq(length(lat))){
            tgt<-lm(tas.target[j,i,]~seq(length(tas.target[j,i,])))$res
            stc<-lm(tas.stitched[j,i,]~seq(length(tas.stitched[j,i,])))$res
            varcomparison.tas.ssp370.CAMS[j,i,]<-unlist(var.test(stc,tgt)[c("estimate","p.value")])
    }}

    jpeg(picdir%&%"tasres_varcomparison_CAMS_"%&%member%&%"_ssp370.jpg",quality=100,width=800,height=600)
    temp<-varcomparison.tas.ssp370.CAMS
    image.plot(lon,lat,matrix(taper(c(temp[,,1])),length(lon),length(lat)),zlim=zlimtas,xlab="",ylab="",
               breaks=breaks,lab.breaks=breaks,col=colors,
               main="CAMS, SSP370 "%&%member%&%", TAS:\n ratio of monthly variances (stitched/target)",
               axis.args=list(cex.axis=1.5),cex.main=2)
    #points(grid.CAMS,pch=ifelse(c(temp[,,1])<0.8|temp[,,1]>1.2,"x",""),cex=0.75,col="white")
    map("world2",add=TRUE,interior=F)
    dev.off()

    ##
    stitchedfile<-"stitched_CAMS-CSM1-0_tas_ssp245~"%&%member%&%"i1p1f1~1.nc"
    ncfile<-nc_open(inputdirstitched%&%stitchedfile)
    lat<-ncvar_get(ncfile,"lat")
    lon<-ncvar_get(ncfile,"lon")
    tas.stitched<-ncvar_get(ncfile,"tas")
    nc_close(ncfile)

    targetfile<-"comparison_ssp245_CAMS-CSM1-0_"%&%member%&%"i1p1f1_tas.nc"
    ncfile<-nc_open(inputdirtarget%&%targetfile)
    tas.target<-ncvar_get(ncfile,"tas")
    nc_close(ncfile)

    dim(tas.target)
    dim(tas.stitched)

    #tas.stitched<-tas.stitched[,,1980+1:1020]

    varcomparison.tas.ssp245.CAMS<-array(NA,c(length(lon),length(lat),2))
    for(j in seq(length(lon))){
        print(j)
        for(i in seq(length(lat))){
            tgt<-lm(tas.target[j,i,]~seq(length(tas.target[j,i,])))$res
            stc<-lm(tas.stitched[j,i,]~seq(length(tas.stitched[j,i,])))$res
            varcomparison.tas.ssp245.CAMS[j,i,]<-unlist(var.test(stc,tgt)[c("estimate","p.value")])
        }}


    jpeg(picdir%&%"tasres_varcomparison_CAMS_"%&%member%&%"_ssp245.jpg",quality=100,width=800,height=600)
    temp<-varcomparison.tas.ssp245.CAMS
    image.plot(lon,lat,matrix(taper(c(temp[,,1])),length(lon),length(lat)),zlim=zlimtas,xlab="",ylab="",
               breaks=breaks,lab.breaks=breaks,col=colors,
               main="CAMS, SSP245 "%&%member%&%", TAS:\n ratio of monthly variances (stitched/target)",
               axis.args=list(cex.axis=1.5),cex.main=2)
    #points(grid.CAMS,pch=ifelse(c(temp[,,1])<0.8|temp[,,1]>1.2,"x",""),col="white")
    map("world2",add=TRUE,interior=F)
    dev.off()
    ###pr

    stitchedfile<-"stitched_CAMS-CSM1-0_pr_ssp245~"%&%member%&%"i1p1f1~1.nc"
    ncfile<-nc_open(inputdirstitched%&%stitchedfile)
    lat<-ncvar_get(ncfile,"lat")
    lon<-ncvar_get(ncfile,"lon")
    pr.stitched<-ncvar_get(ncfile,"pr")
    nc_close(ncfile)

    targetfile<-"comparison_ssp245_CAMS-CSM1-0_"%&%member%&%"i1p1f1_pr.nc"
    ncfile<-nc_open(inputdirtarget%&%targetfile)
    pr.target<-ncvar_get(ncfile,"pr")
    nc_close(ncfile)

    dim(pr.target)
    dim(pr.stitched)

    #pr.stitched<-pr.stitched[,,1980+1:1020]

    varcomparison.pr.ssp245.CAMS<-array(NA,c(length(lon),length(lat),2))
    for(j in seq(length(lon))){
        print(j)
        for(i in seq(length(lat))){
            tgt<-lm(pr.target[j,i,]~seq(length(pr.target[j,i,])))$res
            stc<-lm(pr.stitched[j,i,]~seq(length(pr.stitched[j,i,])))$res
            varcomparison.pr.ssp245.CAMS[j,i,]<-unlist(var.test(stc,tgt)[c("estimate","p.value")])
        }}


    jpeg(picdir%&%"prres_varcomparison_CAMS_"%&%member%&%"_ssp245.jpg",quality=100,width=800,height=600)
    temp<-varcomparison.pr.ssp245.CAMS
    image.plot(lon,lat,matrix(taper(c(temp[,,1])),length(lon),length(lat)),zlim=zlimpr,xlab="",ylab="",
               breaks=breaks,lab.breaks=breaks,col=colors,
               main="CAMS, SSP245 "%&%member%&%", PR:\n ratio of monthly variances (stitched/target)",
               axis.args=list(cex.axis=1.5),cex.main=2)
    #points(grid.CAMS,pch=ifelse(c(temp[,,1])<0.8|temp[,,1]>1.2,"x",""),col="white")
    map("world2",add=TRUE,interior=F)
    dev.off()



    stitchedfile<-"stitched_CAMS-CSM1-0_pr_ssp370~"%&%member%&%"i1p1f1~1.nc"
    ncfile<-nc_open(inputdirstitched%&%stitchedfile)
    lat<-ncvar_get(ncfile,"lat")
    lon<-ncvar_get(ncfile,"lon")
    pr.stitched<-ncvar_get(ncfile,"pr")
    nc_close(ncfile)

    targetfile<-"comparison_ssp370_CAMS-CSM1-0_"%&%member%&%"i1p1f1_pr.nc"
    ncfile<-nc_open(inputdirtarget%&%targetfile)
    pr.target<-ncvar_get(ncfile,"pr")
    nc_close(ncfile)

    dim(pr.target)
    dim(pr.stitched)

    #pr.stitched<-pr.stitched[,,1980+1:1020]

    varcomparison.pr.ssp370.CAMS<-array(NA,c(length(lon),length(lat),2))
    for(j in seq(length(lon))){
        print(j)
        for(i in seq(length(lat))){
            tgt<-lm(pr.target[j,i,]~seq(length(pr.target[j,i,])))$res
            stc<-lm(pr.stitched[j,i,]~seq(length(pr.stitched[j,i,])))$res
            varcomparison.pr.ssp370.CAMS[j,i,]<-unlist(var.test(stc,tgt)[c("estimate","p.value")])
        }}

    jpeg(picdir%&%"prres_varcomparison_CAMS_"%&%member%&%"_ssp370.jpg",quality=100,width=800,height=600)
    temp<-varcomparison.pr.ssp370.CAMS
    image.plot(lon,lat,matrix(taper(c(temp[,,1])),length(lon),length(lat)),zlim=zlimpr,xlab="",ylab="",
               breaks=breaks,lab.breaks=breaks,col=colors,
               main="CAMS, SSP370 "%&%member%&%", pr:\n ratio of monthly variances (stitched/target)",
               axis.args=list(cex.axis=1.5),cex.main=2)
    #points(grid.CAMS,pch=ifelse(c(temp[,,1])<0.8|temp[,,1]>1.2,"x",""),col="white")
    map("world2",add=TRUE,interior=F)
    dev.off()
}

#MIROC6
for(member in c("r1","r2","r3")){

    stitchedfile<-"stitched_MIROC6_tas_ssp370~"%&%member%&%"i1p1f1~1.nc"
    ncfile<-nc_open(inputdirstitched%&%stitchedfile)
    lat<-ncvar_get(ncfile,"lat")
    lon<-ncvar_get(ncfile,"lon")
    tas.stitched<-ncvar_get(ncfile,"tas")
    nc_close(ncfile)
    grid.MIROC6<-expand.grid(list(lon=lon,lat=lat))

    targetfile<-"comparison_ssp370_MIROC6_"%&%member%&%"i1p1f1_tas.nc"
    ncfile<-nc_open(inputdirtarget%&%targetfile)
    tas.target<-ncvar_get(ncfile,"tas")
    nc_close(ncfile)

    dim(tas.target)
    dim(tas.stitched)

    #tas.stitched<-tas.stitched[,,1980+1:1032]

    varcomparison.tas.ssp370.MIROC6<-array(NA,c(length(lon),length(lat),2))
    for(j in seq(length(lon))){
        print(j)
        for(i in seq(length(lat))){
            tgt<-lm(tas.target[j,i,]~seq(length(tas.target[j,i,])))$res
            stc<-lm(tas.stitched[j,i,]~seq(length(tas.stitched[j,i,])))$res
            varcomparison.tas.ssp370.MIROC6[j,i,]<-unlist(var.test(stc,tgt)[c("estimate","p.value")])
        }}


    jpeg(picdir%&%"tasres_varcomparison_MIROC6_"%&%member%&%"_ssp370.jpg",quality=100,width=800,height=600)
    temp<-get("varcomparison.tas.ssp370.MIROC6."%&%member)
    image.plot(lon,lat,matrix(taper(c(temp[,,1])),length(lon),length(lat)),zlim=zlimtas,xlab="",ylab="",
               breaks=breaks,lab.breaks=breaks,col=colors,
               main="MIROC6, SSP370 "%&%member%&%", TAS:\n ratio of monthly variances (stitched/target)",
               axis.args=list(cex.axis=1.5),cex.main=2)
    #points(grid.MIROC6,pch=ifelse(c(temp[,,1])<0.8|temp[,,1]>1.2,"x",""),col="white")
    map("world2",add=TRUE,interior=F)
    dev.off()

    assign("varcomparison.tas.ssp370.MIROC6."%&%member,temp)

    stitchedfile<-"stitched_MIROC6_tas_ssp245~"%&%member%&%"i1p1f1~1.nc"
    ncfile<-nc_open(inputdirstitched%&%stitchedfile)
    lat<-ncvar_get(ncfile,"lat")
    lon<-ncvar_get(ncfile,"lon")
    tas.stitched<-ncvar_get(ncfile,"tas")
    nc_close(ncfile)

    targetfile<-"comparison_ssp245_MIROC6_"%&%member%&%"i1p1f1_tas.nc"
    ncfile<-nc_open(inputdirtarget%&%targetfile)
    tas.target<-ncvar_get(ncfile,"tas")
    nc_close(ncfile)

    dim(tas.target)
    dim(tas.stitched)


    #tas.stitched<-tas.stitched[,,1980+1:1032]


    varcomparison.tas.ssp245.MIROC6<-array(NA,c(length(lon),length(lat),2))
    for(j in seq(length(lon))){
        print(j)
        for(i in seq(length(lat))){
            tgt<-lm(tas.target[j,i,]~seq(length(tas.target[j,i,])))$res
            stc<-lm(tas.stitched[j,i,]~seq(length(tas.stitched[j,i,])))$res
            varcomparison.tas.ssp245.MIROC6[j,i,]<-unlist(var.test(stc,tgt)[c("estimate","p.value")])
        }}


    jpeg(picdir%&%"tasres_varcomparison_MIROC6_"%&%member%&%"_ssp245.jpg",quality=100,width=800,height=600)
    temp<-get("varcomparison.tas.ssp245.MIROC6."%&%member)
    image.plot(lon,lat,matrix(taper(c(temp[,,1])),length(lon),length(lat)),zlim=zlimtas,xlab="",ylab="",
               breaks=breaks,lab.breaks=breaks,col=colors,
               main="MIROC6, SSP245 "%&%member%&%", TAS:\n ratio of monthly variances (stitched/target)",
               axis.args=list(cex.axis=1.5),cex.main=2)
    #points(grid.MIROC6,pch=ifelse(c(temp[,,1])<0.8|temp[,,1]>1.2,"x",""),col="white")
    map("world2",add=TRUE,interior=F)
    dev.off()

    assign("varcomparison.tas.ssp245.MIROC6."%&%member,temp)


    ###pr

    stitchedfile<-"stitched_MIROC6_pr_ssp370~"%&%member%&%"i1p1f1~1.nc"
    ncfile<-nc_open(inputdirstitched%&%stitchedfile)
    lat<-ncvar_get(ncfile,"lat")
    lon<-ncvar_get(ncfile,"lon")
    pr.stitched<-ncvar_get(ncfile,"pr")
    nc_close(ncfile)

    targetfile<-"comparison_ssp370_MIROC6_"%&%member%&%"i1p1f1_pr.nc"
    ncfile<-nc_open(inputdirtarget%&%targetfile)
    pr.target<-ncvar_get(ncfile,"pr")
    nc_close(ncfile)

    dim(pr.target)
    dim(pr.stitched)

    #pr.stitched<-pr.stitched[,,1980+1:1032]


    varcomparison.pr.ssp370.MIROC6<-array(NA,c(length(lon),length(lat),2))
    for(j in seq(length(lon))){
        print(j)
        for(i in seq(length(lat))){
            tgt<-lm(pr.target[j,i,]~seq(length(pr.target[j,i,])))$res
            stc<-lm(pr.stitched[j,i,]~seq(length(pr.stitched[j,i,])))$res
            varcomparison.pr.ssp370.MIROC6[j,i,]<-unlist(var.test(stc,tgt)[c("estimate","p.value")])
        }}



    jpeg(picdir%&%"prres_varcomparison_MIROC6_"%&%member%&%"_ssp370.jpg",quality=100,width=800,height=600)
    temp<-get("varcomparison.pr.ssp370.MIROC6."%&%member)
    image.plot(lon,lat,matrix(taper(c(temp[,,1])),length(lon),length(lat)),zlim=zlimpr,xlab="",ylab="",
               breaks=breaks,lab.breaks=breaks,col=colors,
               main="MIROC6, SSP370 "%&%member%&%", PR:\n ratio of monthly variances (stitched/target)",
               axis.args=list(cex.axis=1.5),cex.main=2)
    #points(grid.MIROC6,pch=ifelse(c(temp[,,1])<0.8|temp[,,1]>1.2,"x",""),col="white")
    map("world2",add=TRUE,interior=F)
    dev.off()

    assign("varcomparison.pr.ssp370.MIROC6."%&%member,temp)

    stitchedfile<-"stitched_MIROC6_pr_ssp245~"%&%member%&%"i1p1f1~1.nc"
    ncfile<-nc_open(inputdirstitched%&%stitchedfile)
    lat<-ncvar_get(ncfile,"lat")
    lon<-ncvar_get(ncfile,"lon")
    pr.stitched<-ncvar_get(ncfile,"pr")
    nc_close(ncfile)

    targetfile<-"comparison_ssp245_MIROC6_"%&%member%&%"i1p1f1_pr.nc"
    ncfile<-nc_open(inputdirtarget%&%targetfile)
    pr.target<-ncvar_get(ncfile,"pr")
    nc_close(ncfile)

    dim(pr.target)
    dim(pr.stitched)

    #pr.stitched<-pr.stitched[,,1980+1:1032]


    varcomparison.pr.ssp245.MIROC6<-array(NA,c(length(lon),length(lat),2))
    for(j in seq(length(lon))){
        print(j)
        for(i in seq(length(lat))){
            tgt<-lm(pr.target[j,i,]~seq(length(pr.target[j,i,])))$res
            stc<-lm(pr.stitched[j,i,]~seq(length(pr.stitched[j,i,])))$res
            varcomparison.pr.ssp245.MIROC6[j,i,]<-unlist(var.test(stc,tgt)[c("estimate","p.value")])
        }}


    jpeg(picdir%&%"prres_varcomparison_MIROC6_"%&%member%&%"_ssp245.jpg",quality=100,width=800,height=600)
    temp<-get("varcomparison.pr.ssp245.MIROC6."%&%member)
    image.plot(lon,lat,matrix(taper(c(temp[,,1])),length(lon),length(lat)),zlim=zlimpr,xlab="",ylab="",
               breaks=breaks,lab.breaks=breaks,col=colors,
               main="MIROC6, SSP245 "%&%member%&%", PR:\n ratio of monthly variances (stitched/target)",
               axis.args=list(cex.axis=1.5),cex.main=2)
    #points(grid.MIROC6,pch=ifelse(c(temp[,,1])<0.8|temp[,,1]>1.2,"x",""),col="white")
    map("world2",add=TRUE,interior=F)
    dev.off()

    assign("varcomparison.pr.ssp245.MIROC6."%&%member,temp)
}

save(list=objects(pattern="comparison"),file="Rdatasets/RData_trends_var_comparison_frommonthlyresiduals_gridded")




