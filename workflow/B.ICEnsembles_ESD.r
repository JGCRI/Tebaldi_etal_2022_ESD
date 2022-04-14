##using more ensemble members to look at inter-realization spread
remove(list=ls())
library(RColorBrewer)
library(xtable)
"%&%"<-function(x,y)paste0(x,y)
is.odd<-function(x)return(x%%2==1)
compute.variance<-compute.FUN<-function(M,x,w=5,FUN=var){  #function to compute a statistic over a window of years of length 2w
    w<-(w-1)/2
    vec<-c(M[(x-w):(x+w),])
    return(FUN(vec,na.rm=T))
}

inputdir<-"../paper_output2/enrich_ensemble_size_allExpsArchive/"  #path to files collecting stitched and real series for the experiment in enriching the IC ensembles


allfiles.synthetic<-system("ls "%&%inputdir%&%"*synthetic.csv",intern=TRUE)   #filenames are:
# [1] "../paper_output2/enrich_ensemble_size_allExpsArchive/10_ACCESS-ESM1-5_ssp245_synthetic.csv"
# [2] "../paper_output2/enrich_ensemble_size_allExpsArchive/10_ACCESS-ESM1-5_ssp370_synthetic.csv"
# [3] "../paper_output2/enrich_ensemble_size_allExpsArchive/10_CanESM5_ssp245_synthetic.csv"
# [4] "../paper_output2/enrich_ensemble_size_allExpsArchive/10_CanESM5_ssp370_synthetic.csv"
# [5] "../paper_output2/enrich_ensemble_size_allExpsArchive/10_MIROC-ES2L_ssp245_synthetic.csv"
# [6] "../paper_output2/enrich_ensemble_size_allExpsArchive/10_MIROC-ES2L_ssp370_synthetic.csv"
# [7] "../paper_output2/enrich_ensemble_size_allExpsArchive/10_MPI-ESM1-2-LR_ssp245_synthetic.csv"
# [8] "../paper_output2/enrich_ensemble_size_allExpsArchive/10_MPI-ESM1-2-LR_ssp370_synthetic.csv"


#where the first number indicates the size of the ensembles for each of the scenarios available in the archive
#isolating model names
models.synthetic<-gsub(inputdir,"",allfiles.synthetic)
models.synthetic<-gsub("_ssp245_synthetic.csv","",models.synthetic)
models.synthetic<-gsub("_ssp370_synthetic.csv","",models.synthetic)



for(i in 1:length(allfiles.synthetic)){
    filename<-allfiles.synthetic[i]
    print(filename)
    tempsyn<-read.csv(filename)  #columns are:
 #   ,year,value,variable,stitching_id,tol,experiment,ensemble_size
 #   0,1850,-0.4477532909672277,tas,ssp245~r1i1p1f1~1,0.07,ssp245,5
 #   1,1851,-0.4677988830645176,tas,ssp245~r1i1p1f1~1,0.07,ssp245,5
 #   2,1852,-0.5268856360792711,tas,ssp245~r1i1p1f1~1,0.07,ssp245,5
 #   3,1853,-0.3984339665094012,tas,ssp245~r1i1p1f1~1,0.07,ssp245,5
 #   4,1854,-0.4277587502217557,tas,ssp245~r1i1p1f1~1,0.07,ssp245,5
 #   5,1855,-0.619024511901614,tas,ssp245~r1i1p1f1~1,0.07,ssp245,5
    es<-tempsyn$ensemble_size[1]
    exp<-tempsyn$experiment[1]
    tempsyn<-cbind(tempsyn$year,tempsyn$value)[tempsyn$tol==0.075,]
    assign(gsub(es%&%"_","",models.synthetic[i])%&%"."%&%exp%&%".synthetic."%&%es,tempsyn)
    filename<-gsub("synthetic","compdata",filename)  #point at the target series for comparing characteristics
    print(filename)
    tempcomp<-read.csv(filename)
    tempcomp<-cbind(tempcomp$year,tempcomp$value)
    assign(gsub(es%&%"_","",models.synthetic[i])%&%"."%&%exp%&%".compdata."%&%es,tempcomp)

}

ll<-length(models.synthetic)

for(i in seq(ll)){
    model<-models.synthetic[i]
    exp<-ifelse(is.odd(i),"ssp245","ssp370")
    print(model)

    es<-substr(model,1,2)
    es<-gsub("_","",es)
    es<-as.numeric(es)
    print(es)
    synobject<-get(gsub(es%&%"_","",model)%&%"."%&%exp%&%".synthetic."%&%es)
    synes<-nrow(synobject)/251
    print(c("st. realiz:",synes))
    synobject<-cbind(1850:2100,matrix(synobject[,2],nrow=251,ncol=synes))

    compobject<-get(gsub(es%&%"_","",model)%&%"."%&%exp%&%".compdata."%&%es)
    compes<-nrow(compobject)/251
    print(c("tgt. realiz:",compes))
    compobject<-cbind(1850:2100,matrix(compobject[,2],nrow=251,ncol=compes))

    altogether<-cbind(es,compobject,synobject[,-1])
    dimnames(altogether)<-list(NULL, c("es","year",paste0("real",seq(compes)),paste0("stitched",seq(synes))))
    assign(gsub(es%&%"_","",model)%&%"_"%&%exp%&%"_altogether_"%&%es,altogether)  #matrices containing both target (labelled real) and enriched series

}

rm(altogether)
oo<-objects(pattern="altogether")




pdf("Enrich_timeseries_allExpsArchive.pdf")  #plotting real and enriched together, meanwhile computing E1 and E2 statistics
#individual panels are in Appendix D

collect.E1.E2<-array(NA,dim=c(length(oo),2,5,3))
dimnames(collect.E1.E2)<-list(oo,c("stitched","enriched"),
                              c("E1","E2","V","nreal","nstitched"),c("2010","2050","2090"))
for(obj in oo){
    print(obj)
    temp<-get(obj)
    es<-unique(temp[,1])
    print(es)
    if(grepl("ssp245",obj))exp<-"ssp245"
    if(grepl("ssp370",obj))exp<-"ssp370"
    print(exp)
    model<-gsub("_"%&%exp%&%"_altogether_"%&%es,"",obj)
    print(model)

    years<-temp[,"year"]
    tempreal<-temp[,grepl("real",colnames(temp)),drop=F]
    tempstitched<-temp[,grepl("stitched",colnames(temp)),drop=F]
    tempenriched<-cbind(tempreal,tempstitched)

    meanreal<-apply(tempreal,1,mean,na.rm=TRUE)
    meanstitched<-apply(tempstitched,1,mean,na.rm=TRUE)
    meanenriched<-apply(tempenriched,1,mean,na.rm=TRUE)
    collect.E1.E2[obj,,"nreal",]<-ncol(tempreal)
    collect.E1.E2[obj,,"nstitched",]<-ncol(tempstitched)

    for(x in c(2010,2050,2090)){
        mm<-match(x,years)
        print(c("match:",mm))
        collect.E1.E2[obj,"stitched","E1",as.character(x)]<-(meanreal[mm]-meanstitched[mm])^2
        collect.E1.E2[obj,"stitched","E2",as.character(x)]<-compute.variance(tempstitched,mm,9)
        collect.E1.E2[obj,"stitched","V",as.character(x)]<-compute.variance(tempreal,mm,9)

        collect.E1.E2[obj,"enriched","E1",as.character(x)]<-(meanreal[mm]-meanenriched[mm])^2
        collect.E1.E2[obj,"enriched","E2",as.character(x)]<-compute.variance(tempenriched,mm,9)
        collect.E1.E2[obj,"enriched","V",as.character(x)]<-compute.variance(tempreal,mm,9)

    }

    ylim<-range(c(tempenriched),na.rm=TRUE)
par(mar=c(5,5,5,2))
    matplot(temp[,"year"],tempreal,type="l",lty=1,col="grey",las=1,ylim=ylim,
            main=paste(model,exp,"Size of archive:",es,"\nN. of stitched:",ncol(tempstitched),"N. of target:",ncol(tempreal)),
            cex.main=1.5,cex.lab=2,cex.axis=1.7,xlab="",ylab="GSAT")
    matlines(temp[,"year"],tempstitched,lty=1,col="firebrick")
    legend("topleft",lty=1,col=c("grey","firebrick"),legend=c("target","stitched"),bty="n",lwd=2,cex=2)

}

dev.off()


objects(pattern="E1.E2")
dim(collect.E1.E2)
models<-dimnames(collect.E1.E2)[[1]]
collect.E1only<-collect.E2only<-array(NA,dim=c(length(models),2,3))
dimnames(collect.E1only)<-dimnames(collect.E2only)<-list(models,c("stitched","enriched"),c("2010","2050","2090"))
count<-0
nreal<-nstitched<-numeric(0)
for(i in 1:28){
    count<-count+1
    temp<-collect.E1.E2[i,"stitched",,]
    collect.E1only[count,"stitched",]<-round(100*(temp[1,]/temp[3,]))
    collect.E2only[count,"stitched",]<-round(100*(temp[2,]/temp[3,]))
    nreal<-c(nreal,collect.E1.E2[i,"stitched","nreal",1])
    nstitched<-c(nstitched,collect.E1.E2[i,"stitched","nstitched",1])

    temp<-collect.E1.E2[i,"enriched",,]
    collect.E1only[count,"enriched",]<-round(100*(temp[1,]/temp[3,]))
    collect.E2only[count,"enriched",]<-round(100*(temp[2,]/temp[3,]))

}
names(nreal)<-names(nstitched)<-models

table4<-cbind(nreal,nstitched,collect.E1only[,"stitched",]/100,collect.E2only[,"stitched",]/100)
table5<-cbind(nreal,nstitched,collect.E1only[,"enriched",]/100,collect.E2only[,"enriched",]/100)

table4<-table4[c(1,2,4,3,6,5,11,7:10,16,12:15,18,17,20,19,22,21,24,23,25:28),]
table5<-table5[c(1,2,4,3,6,5,11,7:10,16,12:15,18,17,20,19,22,21,24,23,25:28),]


xtable(table4)   #paper table 4
xtable(table5)   #paper table 5


nr<-nrow(table4)


