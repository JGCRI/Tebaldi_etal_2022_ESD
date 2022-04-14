remove(list=ls())
library(RColorBrewer)
library(fields)

"%&%"<-function(x,y)paste0(x,y)


tim.colors.onlyhot<-function(n,alpha=1){
    return(tim.colors(2*n, alpha=alpha)[n+1:n])
}

tim.colors.onlycold<-function(n,alpha=1){
    return(tim.colors(2*n,alpha=alpha)[n:1])
}
megapalette<-c("yellow","mediumpurple4","lightsteelblue1","lightskyblue4","royalblue4","darkseagreen1","aquamarine","aquamarine4",
               "darkgreen","olivedrab1","seagreen1","seagreen3","thistle1","thistle4","purple1","slateblue4",
               "plum1","orchid1","darkmagenta","pink","palevioletred2","deeppink4","ivory2","khaki3",
               "yellow4","darkolivegreen","tan","tan1","tan4","white","gray66","gray28")
colors<-c("skyblue","goldenrod","firebrick1","seagreen1","magenta","palevioletred2")
transparent_col <- function(color, percent = 50, name = NULL) {
    #      color = color name
    #    percent = % transparency
    #       name = an optional name for the color

    ## Get RGB values for named color
    rgb.val <- col2rgb(color)

    ## Make new color using input color as base and alpha set by transparency
    t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
                 max = 255,
                 alpha = (100 - percent) * 255 / 100,
                 names = name)

    ## Save the color
    invisible(t.col)
}

colorpalette<-brewer.pal(12,"Paired")
tcolorpalette<-character(length(colorpalette))
for(i in seq(length(colorpalette)))tcolorpalette[i]<-transparent_col(colorpalette[i])

stitcheddir<-"~/Desktop/STITCHES/paper_output/GSAT/stitched/"
realdir<-gsub("stitched","real",stitcheddir)

realfiles<-system("ls "%&%realdir,intern=TRUE)[1]
stitchedfiles<-system("ls "%&%stitcheddir,intern=TRUE)

intermediate.real<-read.csv(realdir%&%realfiles[1])
intermediate.real[1:5,]
#variable experiment ensemble      model year      value
#1      tas     ssp245 r1i1p1f1 ACCESS-CM2 1850 -0.5150906
#2      tas     ssp245 r1i1p1f1 ACCESS-CM2 1851 -0.5055997
#3      tas     ssp245 r1i1p1f1 ACCESS-CM2 1852 -0.3480420
#4      tas     ssp245 r1i1p1f1 ACCESS-CM2 1853 -0.4516364
#5      tas     ssp245 r1i1p1f1 ACCESS-CM2 1854 -0.3814256
#zstore
#1 gs://cmip6/CMIP6/ScenarioMIP/CSIRO-ARCCSS/ACCESS-CM2/historical/r1i1p1f1/Amon/tas/gn/v20191108/
#    2 gs://cmip6/CMIP6/ScenarioMIP/CSIRO-ARCCSS/ACCESS-CM2/historical/r1i1p1f1/Amon/tas/gn/v20191108/
#    3 gs://cmip6/CMIP6/ScenarioMIP/CSIRO-ARCCSS/ACCESS-CM2/historical/r1i1p1f1/Amon/tas/gn/v20191108/
#    4 gs://cmip6/CMIP6/ScenarioMIP/CSIRO-ARCCSS/ACCESS-CM2/historical/r1i1p1f1/Amon/tas/gn/v20191108/
#    5 gs://cmip6/CMIP6/ScenarioMIP/CSIRO-ARCCSS/ACCESS-CM2/historical/r1i1p1f1/Amon/tas/gn/v20191108/

allmodels<-unique(intermediate.real$model)
temp.45<-intermediate.real[intermediate.real$experiment=="ssp245",]

allmodels45<-unique(temp.45$model)
for(model in allmodels45){
    temp<-as.matrix(temp.45[temp.45$model==model,c("year","value")])
    dimnames(temp)<-list(temp.45$ensemble[temp.45$model==model],c("year","GTAS"))
    experiments<-unique(rownames(temp))
    for(experiment in experiments){
        assign(paste(model,experiment,"ssp245",sep="."),temp[rownames(temp)==experiment,])}}

temp.70<-intermediate.real[intermediate.real$experiment=="ssp370",]

allmodels70<-unique(temp.70$model)
for(model in allmodels70){
    temp<-as.matrix(temp.70[temp.70$model==model,c("year","value")])
    dimnames(temp)<-list(temp.70$ensemble[temp.70$model==model],c("year","GTAS"))
    experiments<-unique(rownames(temp))
    for(experiment in experiments){
        assign(paste(model,experiment,"ssp370",sep="."),temp[rownames(temp)==experiment,])}}

#filling in MIROC6 that was re-run at a later date

miroc6.real<-read.csv(realdir%&%"comparison_data_MIROC6.csv")
miroc6.real[1:5,]
#variable experiment ensemble  model year      value
#1      tas     ssp245 r1i1p1f1 MIROC6 1850 -0.6791866
#2      tas     ssp245 r1i1p1f1 MIROC6 1851 -0.7174711
#3      tas     ssp245 r1i1p1f1 MIROC6 1852 -0.6942947
#4      tas     ssp245 r1i1p1f1 MIROC6 1853 -0.6418054
#5      tas     ssp245 r1i1p1f1 MIROC6 1854 -0.3767872
#zstore
#1 gs://cmip6/CMIP6/ScenarioMIP/MIROC/MIROC6/historical/r1i1p1f1/Amon/tas/gn/v20190627/
#    2 gs://cmip6/CMIP6/ScenarioMIP/MIROC/MIROC6/historical/r1i1p1f1/Amon/tas/gn/v20190627/
#    3 gs://cmip6/CMIP6/ScenarioMIP/MIROC/MIROC6/historical/r1i1p1f1/Amon/tas/gn/v20190627/
#    4 gs://cmip6/CMIP6/ScenarioMIP/MIROC/MIROC6/historical/r1i1p1f1/Amon/tas/gn/v20190627/
#    5 gs://cmip6/CMIP6/ScenarioMIP/MIROC/MIROC6/historical/r1i1p1f1/Amon/tas/gn/v20190627/


temp.45<-miroc6.real[miroc6.real$experiment=="ssp245",]
dim(miroc6.real)
#[1] 2008    7
dim(temp.45)
#[1] 1255    7



temp<-as.matrix(temp.45[,c("year","value")])
dimnames(temp)<-list(temp.45$ensemble,c("year","GTAS"))
experiments<-unique(rownames(temp))
for(experiment in experiments){
assign(paste("MIROC6",experiment,"ssp245",sep="."),temp[rownames(temp)==experiment,])}

temp.70<-miroc6.real[miroc6.real$experiment=="ssp370",]


temp<-as.matrix(temp.70[,c("year","value")])
dimnames(temp)<-list(temp.70$ensemble,c("year","GTAS"))
experiments<-unique(rownames(temp))
for(experiment in experiments){
assign(paste("MIROC6",experiment,"ssp370",sep="."),temp[rownames(temp)==experiment,])}



for(file in stitchedfiles){
    print(file)
    temp<-read.csv(stitcheddir%&%file)
    labels<-unique(temp[,4])
    if(length(labels)>1)stop("file contains more than one realization")
    else{
        model<-gsub("stitched_","",file)
        model<-gsub("_GSAT_","",model)
        model<-gsub(paste0(labels,".csv"),"",model)

        scenario<-substr(labels,1,6)
        em<-gsub(paste0(scenario,"~"),"",labels)
        objectlabel<-gsub("~",".",em)
        temp<-cbind(temp$year,temp$value)
        dimnames(temp)<-list(rep(objectlabel,nrow(temp)),c("year","GTAS"))
        assign(paste("stitched",model,objectlabel,scenario,sep="."),temp)
    }
}

model<-allmodels[1]


for(model in allmodels){
    print(model)
    subset<-objects(pattern=paste0(model,".r"))
    subset.245<-subset[grep("ssp245",subset)]

    if(length(subset.245)>0){
        subset.245.stitched<-subset.245[grep("stitched",subset.245)]
        if(length(subset.245.stitched)>0)
            subset.245.real<-subset.245[-grep("stitched",subset.245)]
        else subset.245.real<-subset.245
    }

    subset.370<-subset[grep("ssp370",subset)]

    if(length(subset.370)>0){
        subset.370.stitched<-subset.370[grep("stitched",subset.370)]
        if(length(subset.370.stitched)>0)
            subset.370.real<-subset.370[-grep("stitched",subset.370)]
        else subset.370.real<-subset.370
    }



    if(length(subset.245)>0){
        print("ssp245")

        allyears<-1850:2101
        altogether<-matrix(NA,length(allyears),length(subset.245))

        index<-0
        columnnames<-"year"
        for(i in 1:length(subset.245.real)){
            index<-index+1
            temp<-get(subset.245.real[i])
            altogether[match(temp[,"year"],allyears),index]<-temp[,"GTAS"]
            columnnames<-c(columnnames,rownames(temp)[1])
        }
        if(length(subset.245.stitched)>0){

            for(i in 1:length(subset.245.stitched)){
                index<-index+1
                temp<-get(subset.245.stitched[i])
                altogether[match(temp[,"year"],allyears),index]<-temp[,"GTAS"]
                columnnames<-c(columnnames,paste0("stitched.",rownames(temp)[1]))
            }
        }
        altogether<-cbind(allyears,altogether)
        dimnames(altogether)<-list(NULL, columnnames)
        assign(paste(model,"altogether","ssp245",sep="."),altogether)
    }

    if(length(subset.370)>0){
        print("ssp370")
        allyears<-1850:2101
        altogether<-matrix(NA,length(allyears),length(subset.370))

        index<-0
        columnnames<-"year"
        for(i in 1:length(subset.370.real)){
            index<-index+1
            temp<-get(subset.370.real[i])
            altogether[match(temp[,"year"],allyears),index]<-temp[,"GTAS"]
            columnnames<-c(columnnames,rownames(temp)[1])
        }

        if(length(subset.370.stitched)>0){

            for(i in 1:length(subset.370.stitched)){
                index<-index+1
                temp<-get(subset.370.stitched[i])
                altogether[match(temp[,"year"],allyears),index]<-temp[,"GTAS"]
                columnnames<-c(columnnames,paste0("stitched.",rownames(temp)[1]))
            }
        }
        altogether<-cbind(allyears,altogether)
        dimnames(altogether)<-list(NULL, columnnames)
        assign(paste(model,"altogether","ssp370",sep="."),altogether)
    }
}

#these altogether objects are in essence matrices that collect for each of the two scenarios, for each model, time series of target and stitched GSAT



pdf("Real_vs_Stitched_GSAT_timeseries_commonrange.pdf")  #contains individual panels for Figures 2,3,4 and Appendix A1, A2, A3, A4
par(mar=c(5,5,5,2))
for(model in allmodels){
    print(model)
    temp<-try(get(paste(model,"altogether","ssp245",sep=".")))
    if(class(temp)!="try-error"){
        years<-temp[,"year"]
        stitched<-grep("stitched",dimnames(temp)[[2]])


        matplot(years,temp[,-1],xlab="",ylab="GSAT",las=1,type="l",lty=1,col=1,main=model%&%" SSP2-4.5", ylim=c(-2,6),cex.axis=1.7,cex.main=2,cex.lab=2)
        if(length(stitched)>0){
            matlines(years,temp[,stitched],lty=1,col=colorpalette[1:length(stitched)])
        }
    }
    temp<-try(get(paste(model,"altogether","ssp370",sep=".")))
    if(class(temp)!="try-error"){
        years<-temp[,"year"]
        stitched<-grep("stitched",dimnames(temp)[[2]])

        matplot(years,temp[,-1],xlab="",ylab="GSAT",las=1,type="l",lty=1,col=1,main=model%&%" SSP3-7.0", ylim=c(-2,6),cex.axis=1.7,cex.main=2,cex.lab=2)
        if(length(stitched)>0){
            matlines(years,temp[,stitched],lty=1,col=colorpalette[1:length(stitched)])
        }
    }}
dev.off()

allmodels<-c( "ACCESS-CM2" ,     "ACCESS-ESM1-5" ,  "AWI-CM-1-1-MR" ,  "BCC-CSM2-MR"  ,   "BCC-ESM1"  ,      "CAMS-CSM1-0"  ,   "CAS-ESM2-0" ,
              "CESM2-FV2"  ,     "CESM2-WACCM-FV2" ,"CESM2-WACCM" ,    "CESM2"  ,         "CMCC-CM2-HR4" ,   "CMCC-CM2-SR5"  ,  "CMCC-ESM2"      ,
              "CanESM5"  ,       "FGOALS-g3" ,     "FIO-ESM-2-0" ,    "GISS-E2-1-G"   ,  "GISS-E2-1-H"  ,   "HadGEM3-GC31-LL" ,"HadGEM3-GC31-MM",
              "IITM-ESM"   ,     "MCM-UA-1-0"  ,    "MIROC-ES2L" ,     "MIROC6"  ,        "MPI-ESM-1-2-HAM" ,"MPI-ESM1-2-HR"  , "MPI-ESM1-2-LR" ,
              "MRI-ESM2-0"  ,    "NESM3"  ,         "NorCPM1"  ,       "NorESM2-LM"  ,    "NorESM2-MM" ,    "SAM0-UNICON"   ,  "TaiESM1" ,
              "UKESM1-0-LL" )

for(model in allmodels){  #here we compute statistics like trends, standard deviations, ACFs and PACFs
    print(model)
    temp<-try(get(paste(model,"altogether","ssp245",sep=".")))
    if(class(temp)!="try-error"){
        years<-temp[,"year"]
        stitched<-grep("stitched",dimnames(temp)[[2]])

        if(length(stitched)>0){
            tempreal<-temp[,-c(1,stitched),drop=F]
            tempstitched<-temp[,stitched,drop=F]

            nr<-ncol(tempreal)
            ns<-ncol(tempstitched)

            alltrends<-array(NA,dim=c(2,nr+ns,2))
            dimnames(alltrends)<-list(c("hist","future"),c(paste0("R",seq(nr)),paste0("S",seq(ns))),c("Estimate","StdError"))
            alliasds<-matrix(NA,2,nr+ns)
            dimnames(alliasds)<-list(c("hist","future"),c(paste0("R",seq(nr)),paste0("S",seq(ns))))
            allacfs<-array(NA,dim=c(2,nr+ns,2,15))
            dimnames(allacfs)<-list(c("hist","future"),c(paste0("R",seq(nr)),paste0("S",seq(ns))),c("ACF","PACF"),NULL)


            index<-0
            for(i in seq(nr)){
                index<-index+1
                fillin<-try(summary(lm(tempreal[,i]~seq(length(years)),subset=years<2015))$coeff[2,1:2])
                if(class(fillin)!="try-error")alltrends["hist",index,]<-fillin
                fillin<-try(summary(lm(tempreal[,i]~seq(length(years)),subset=years>2014&years<=2100))$coeff[2,1:2])
                if(class(fillin)!="try-error")alltrends["future",index,]<-fillin
                fillin<-try(sd(lm(tempreal[,i]~seq(length(years)),subset=years<2015)$res,na.rm=TRUE))
                if(class(fillin)!="try-error")alliasds["hist",index]<-fillin
                fillin<-try(sd(lm(tempreal[,i]~seq(length(years)),subset=years>2014&years<=2100)$res,na.rm=TRUE))
                if(class(fillin)!="try-error")alliasds["future",index]<-fillin
                fillin<-try(acf(lm(tempreal[,i]~seq(length(years)),subset=years<2015)$res,na.action=na.omit)$acf[1:15,,])
                if(class(fillin)!="try-error")allacfs["hist",index,"ACF",]<-fillin
                fillin<-try(pacf(lm(tempreal[,i]~seq(length(years)),subset=years<2015)$res,na.action=na.omit)$acf[1:15,,])
                if(class(fillin)!="try-error")allacfs["hist",index,"PACF",]<-fillin
                fillin<-try(acf(lm(tempreal[,i]~seq(length(years)),subset=years>2014&years<=2100)$res,na.action=na.omit)$acf[1:15,,])
                if(class(fillin)!="try-error")allacfs["future",index,"ACF",]<-fillin
                fillin<-try(pacf(lm(tempreal[,i]~seq(length(years)),subset=years>2014&years<=2100)$res,na.action=na.omit)$acf[1:15,,])
                if(class(fillin)!="try-error")allacfs["future",index,"PACF",]<-fillin
            }
            for(i in seq(ns)){
                index<-index+1
                fillin<-try(summary(lm(tempstitched[,i]~seq(length(years)),subset=years<2015))$coeff[2,1:2])
                if(class(fillin)!="try-error")alltrends["hist",index,]<-fillin
                fillin<-try(summary(lm(tempstitched[,i]~seq(length(years)),subset=years>2014&years<=2100))$coeff[2,1:2])
                if(class(fillin)!="try-error")alltrends["future",index,]<-fillin
                fillin<-try(sd(lm(tempstitched[,i]~seq(length(years)),subset=years<2015)$res,na.rm=TRUE))
                if(class(fillin)!="try-error")alliasds["hist",index]<-fillin
                fillin<-try(sd(lm(tempstitched[,i]~seq(length(years)),subset=years>2014&years<=2100)$res,na.rm=TRUE))
                if(class(fillin)!="try-error")alliasds["future",index]<-fillin
                fillin<-try(acf(lm(tempstitched[,i]~seq(length(years)),subset=years<2015)$res,na.action=na.omit)$acf[1:15,,])
                if(class(fillin)!="try-error")allacfs["hist",index,"ACF",]<-fillin
                fillin<-try(pacf(lm(tempstitched[,i]~seq(length(years)),subset=years<2015)$res,na.action=na.omit)$acf[1:15,,])
                if(class(fillin)!="try-error")allacfs["hist",index,"PACF",]<-fillin
                fillin<-try(acf(lm(tempstitched[,i]~seq(length(years)),subset=years>2014&years<=2100)$res,na.action=na.omit)$acf[1:15,,])
                if(class(fillin)!="try-error")allacfs["future",index,"ACF",]<-fillin
                fillin<-try(pacf(lm(tempstitched[,i]~seq(length(years)),subset=years>2014&years<=2100)$res,na.action=na.omit)$acf[1:15,,])
                if(class(fillin)!="try-error")allacfs["future",index,"PACF",]<-fillin
            }
        }
        assign(paste(model,"alltrends","ssp245",sep="."),alltrends)
        assign(paste(model,"alliasds","ssp245",sep="."),alliasds)
        assign(paste(model,"allacfs","ssp245",sep="."),allacfs)
    }


    temp<-try(get(paste(model,"altogether","ssp370",sep=".")))
    if(class(temp)!="try-error"){
        years<-temp[,"year"]
        stitched<-grep("stitched",dimnames(temp)[[2]])

        if(length(stitched)>0){
            tempreal<-temp[,-c(1,stitched),drop=F]
            tempstitched<-temp[,stitched,drop=F]

            nr<-ncol(tempreal)
            ns<-ncol(tempstitched)

            alltrends<-array(NA,dim=c(2,nr+ns,2))
            dimnames(alltrends)<-list(c("hist","future"),c(paste0("R",seq(nr)),paste0("S",seq(ns))),c("Estimate","StdError"))
            alliasds<-matrix(NA,2,nr+ns)
            dimnames(alliasds)<-list(c("hist","future"),c(paste0("R",seq(nr)),paste0("S",seq(ns))))
            allacfs<-array(NA,dim=c(2,nr+ns,2,15))
            dimnames(allacfs)<-list(c("hist","future"),c(paste0("R",seq(nr)),paste0("S",seq(ns))),c("ACF","PACF"),NULL)


            index<-0
            for(i in seq(nr)){
                index<-index+1
                fillin<-try(summary(lm(tempreal[,i]~seq(length(years)),subset=years<2015))$coeff[2,1:2])
                if(class(fillin)!="try-error")alltrends["hist",index,]<-fillin
                fillin<-try(summary(lm(tempreal[,i]~seq(length(years)),subset=years>2014&years<=2100))$coeff[2,1:2])
                if(class(fillin)!="try-error")alltrends["future",index,]<-fillin
                fillin<-try(sd(lm(tempreal[,i]~seq(length(years)),subset=years<2015)$res,na.rm=TRUE))
                if(class(fillin)!="try-error")alliasds["hist",index]<-fillin
                fillin<-try(sd(lm(tempreal[,i]~seq(length(years)),subset=years>2014&years<=2100)$res,na.rm=TRUE))
                if(class(fillin)!="try-error")alliasds["future",index]<-fillin
                fillin<-try(acf(lm(tempreal[,i]~seq(length(years)),subset=years<2015)$res,na.action=na.omit)$acf[1:15,,])
                if(class(fillin)!="try-error")allacfs["hist",index,"ACF",]<-fillin
                fillin<-try(pacf(lm(tempreal[,i]~seq(length(years)),subset=years<2015)$res,na.action=na.omit)$acf[1:15,,])
                if(class(fillin)!="try-error")allacfs["hist",index,"PACF",]<-fillin
                fillin<-try(acf(lm(tempreal[,i]~seq(length(years)),subset=years>2014&years<=2100)$res,na.action=na.omit)$acf[1:15,,])
                if(class(fillin)!="try-error")allacfs["future",index,"ACF",]<-fillin
                fillin<-try(pacf(lm(tempreal[,i]~seq(length(years)),subset=years>2014&years<=2100)$res,na.action=na.omit)$acf[1:15,,])
                if(class(fillin)!="try-error")allacfs["future",index,"PACF",]<-fillin
            }
            for(i in seq(ns)){
                index<-index+1
                fillin<-try(summary(lm(tempstitched[,i]~seq(length(years)),subset=years<2015))$coeff[2,1:2])
                if(class(fillin)!="try-error")alltrends["hist",index,]<-fillin
                fillin<-try(summary(lm(tempstitched[,i]~seq(length(years)),subset=years>2014&years<=2100))$coeff[2,1:2])
                if(class(fillin)!="try-error")alltrends["future",index,]<-fillin
                fillin<-try(sd(lm(tempstitched[,i]~seq(length(years)),subset=years<2015)$res,na.rm=TRUE))
                if(class(fillin)!="try-error")alliasds["hist",index]<-fillin
                fillin<-try(sd(lm(tempstitched[,i]~seq(length(years)),subset=years>2014&years<=2100)$res,na.rm=TRUE))
                if(class(fillin)!="try-error")alliasds["future",index]<-fillin
                fillin<-try(acf(lm(tempstitched[,i]~seq(length(years)),subset=years<2015)$res,na.action=na.omit)$acf[1:15,,])
                if(class(fillin)!="try-error")allacfs["hist",index,"ACF",]<-fillin
                fillin<-try(pacf(lm(tempstitched[,i]~seq(length(years)),subset=years<2015)$res,na.action=na.omit)$acf[1:15,,])
                if(class(fillin)!="try-error")allacfs["hist",index,"PACF",]<-fillin
                fillin<-try(acf(lm(tempstitched[,i]~seq(length(years)),subset=years>2014&years<=2100)$res,na.action=na.omit)$acf[1:15,,])
                if(class(fillin)!="try-error")allacfs["future",index,"ACF",]<-fillin
                fillin<-try(pacf(lm(tempstitched[,i]~seq(length(years)),subset=years>2014&years<=2100)$res,na.action=na.omit)$acf[1:15,,])
                if(class(fillin)!="try-error")allacfs["future",index,"PACF",]<-fillin
            }
        }
        assign(paste(model,"alltrends","ssp370",sep="."),alltrends)
        assign(paste(model,"alliasds","ssp370",sep="."),alliasds)
        assign(paste(model,"allacfs","ssp370",sep="."),allacfs)
    }
}


inorout<-array(NA,dim=c(length(allmodels),2,2,2))  #here we check if the central estimates of the stitched trends fall within the 95% C.I.s of the target, and if not by how much they miss them
dimnames(inorout)<-list(allmodels,c("hist","future"),c("ssp245","ssp370"),c("in","total"))



for(model in allmodels){
    print(model)
    print(round(100*inorout[model,"future",,"in"]/inorout[model,"future",,"total"],dig=1))
    print(inorout[model,"future",,"total"])
}

howmuchout<-array(NA,dim=c(length(allmodels),2,2))
dimnames(howmuchout)<-list(allmodels,c("ssp245","ssp370"),c("meandiff","total"))
for(model in allmodels){
    print(model)

    trends<-try(get(paste(model,"alltrends","ssp245",sep=".")))
    if(class(trends)!="try-error"){

        x<-seq(dim(trends)[2])
        real<-grep("R",dimnames(trends)[[2]])
        stitched<-x[-real]



        ce<-trends["future",,1]
        lowlim<-trends["future",,1]-2*trends["future",,2]
        uplim<-trends["future",,1]+2*trends["future",,2]


        cestitched<-ce[-real]
        lowlim<-min(lowlim[real],na.rm=TRUE)
        uplim<-max(uplim[real],na.rm=TRUE)

        meandiff<-cestitched-lowlim
        meandiff[meandiff>0]<-c(uplim-cestitched)[meandiff>0]
        meandiff<--meandiff[meandiff<0]
        if(length(meandiff)>0)howmuchout[model,"ssp245","meandiff"]<-mean(meandiff,na.rm=TRUE)
        else howmuchout[model,"ssp245","meandiff"]<-0
        howmuchout[model,"ssp245","total"]<-length(stitched)
    }

    trends<-try(get(paste(model,"alltrends","ssp370",sep=".")))
    if(class(trends)!="try-error"){

        real<-grep("R",dimnames(trends)[[2]])
        stitched<-seq(dim(trends)[2])[-real]


        ce<-trends["future",,1]
        lowlim<-trends["future",,1]-2*trends["future",,2]
        uplim<-trends["future",,1]+2*trends["future",,2]


        cestitched<-ce[-real]
        lowlim<-min(lowlim[real],na.rm=TRUE)
        uplim<-max(uplim[real],na.rm=TRUE)

        meandiff<-cestitched-lowlim
        meandiff[meandiff>0]<-c(uplim-cestitched)[meandiff>0]
        meandiff<--meandiff[meandiff<0]
        if(length(meandiff)>0)howmuchout[model,"ssp370","meandiff"]<-mean(meandiff,na.rm=TRUE)
        else howmuchout[model,"ssp370","meandiff"]<-0

        howmuchout[model,"ssp370","total"]<-length(stitched)
    }
}


for(model in allmodels){
    print(model)
    print(howmuchout[model,,"total"])
    print(round(100-100*inorout[model,"future",,"in"]/inorout[model,"future",,"total"],dig=1))
    print(howmuchout[model,,"meandiff"])}



####year-to-year differences

alljumps.vsreal<-array(dim=c(length(allmodels),2,2))
dimnames(alljumps.vsreal)<-list(allmodels,c("ssp245","ssp370"),c("fractionjumps","totalseams"))
for(model in allmodels){
        print(model)
        temp<-try(get(paste(model,"altogether","ssp245",sep=".")))
        if(class(temp)!="try-error"){
            years<-temp[,"year"]
            stitched<-grep("stitched",dimnames(temp)[[2]])

            if(length(stitched)>0){
                tempreal<-temp[,-c(1,stitched),drop=F]
                tempstitched<-temp[,stitched,drop=F]

                nr<-ncol(tempreal)
                ns<-ncol(tempstitched)
                totalseams<-ns*27
                diffreal<-c(apply(tempreal,2,diff))
                sdevreal<-sd(diffreal,na.rm=TRUE)
                diffstitched<-c(apply(tempstitched,2,diff))
                seams<-diffstitched[seq(9,length(diffstitched),by=9)]
                sdevstitched<-sd(diffstitched[-seq(9,length(diffstitched),by=9)],na.rm=TRUE)
            bigjumps<-sum(seams>2*sdevreal,na.rm=TRUE)
                           alljumps.vsreal[model,"ssp245","fractionjumps"]<-bigjumps/totalseams
                alljumps.vsreal[model,"ssp245","totalseams"]<-totalseams
            }}
        temp<-try(get(paste(model,"altogether","ssp370",sep=".")))
        if(class(temp)!="try-error"){
            years<-temp[,"year"]
            stitched<-grep("stitched",dimnames(temp)[[2]])

            if(length(stitched)>0){
                tempreal<-temp[,-c(1,stitched),drop=F]
                tempstitched<-temp[,stitched,drop=F]

                nr<-ncol(tempreal)
                ns<-ncol(tempstitched)
                totalseams<-ns*27
                diffreal<-c(apply(tempreal,2,diff))
                sdevreal<-sd(diffreal,na.rm=TRUE)
                diffstitched<-c(apply(tempstitched,2,diff))
                sdevstitched<-sd(diffstitched[-seq(9,length(diffstitched),by=9)],na.rm=TRUE)
                seams<-diffstitched[seq(9,length(diffstitched),by=9)]
                bigjumps<-sum(seams>2*sdevreal,na.rm=TRUE)
                          alljumps.vsreal[model,"ssp370","fractionjumps"]<-bigjumps/totalseams
                alljumps.vsreal[model,"ssp370","totalseams"]<-totalseams
            }}
}



alljumps.vsstitched<-array(dim=c(length(allmodels),2,2))
dimnames(alljumps.vsstitched)<-list(allmodels,c("ssp245","ssp370"),c("fractionjumps","totalseams"))
for(model in allmodels){
    print(model)
    temp<-try(get(paste(model,"altogether","ssp245",sep=".")))
    if(class(temp)!="try-error"){
        years<-temp[,"year"]
        stitched<-grep("stitched",dimnames(temp)[[2]])

        if(length(stitched)>0){
            tempreal<-temp[,-c(1,stitched),drop=F]
            tempstitched<-temp[,stitched,drop=F]

            nr<-ncol(tempreal)
            ns<-ncol(tempstitched)
            totalseams<-ns*27
            diffreal<-c(apply(tempreal,2,diff))
            sdevreal<-sd(diffreal,na.rm=TRUE)
            diffstitched<-c(apply(tempstitched,2,diff))
            seams<-diffstitched[seq(9,length(diffstitched),by=9)]
            sdevstitched<-sd(diffstitched[-seq(9,length(diffstitched),by=9)],na.rm=TRUE)
                          bigjumps<-sum(seams>2*sdevstitched,na.rm=TRUE)
            alljumps.vsstitched[model,"ssp245","fractionjumps"]<-bigjumps/totalseams
            alljumps.vsstitched[model,"ssp245","totalseams"]<-totalseams
        }}
    temp<-try(get(paste(model,"altogether","ssp370",sep=".")))
    if(class(temp)!="try-error"){
        years<-temp[,"year"]
        stitched<-grep("stitched",dimnames(temp)[[2]])

        if(length(stitched)>0){
            tempreal<-temp[,-c(1,stitched),drop=F]
            tempstitched<-temp[,stitched,drop=F]

            nr<-ncol(tempreal)
            ns<-ncol(tempstitched)
            totalseams<-ns*27
            diffreal<-c(apply(tempreal,2,diff))
            sdevreal<-sd(diffreal,na.rm=TRUE)
            diffstitched<-c(apply(tempstitched,2,diff))
            sdevstitched<-sd(diffstitched[-seq(9,length(diffstitched),by=9)],na.rm=TRUE)
            seams<-diffstitched[seq(9,length(diffstitched),by=9)]
                      bigjumps<-sum(seams>2*sdevstitched,na.rm=TRUE)
            alljumps.vsstitched[model,"ssp370","fractionjumps"]<-bigjumps/totalseams
            alljumps.vsstitched[model,"ssp370","totalseams"]<-totalseams
        }}
}

temp<-cbind(alljumps.vsstitched[,"ssp245",-2],alljumps.vsreal[,"ssp245",-2],alljumps.vsstitched[,"ssp370",-2],alljumps.vsreal[,"ssp370",-2])
library(xtable)
xtable(temp)   #Table 3 in the paper

