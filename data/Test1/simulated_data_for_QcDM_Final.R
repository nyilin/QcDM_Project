rm(list=ls())
#install.packages("anytime")
library(anytime)

##Set working directory
setwd("C:/Users/ephtcs/Documents/QcDM_Project folder/data/Test1")

##Number of patients (or hospital stays)
n<-100

##Set seed
set.seed(1234)

##Number of days per patient and the number of readings per day for each patient
ndays<-rpois(n,11.3)
nreadings<-rpois(sum(ndays),3.9)+1
##Location for each patient:
location<-sample(c("A","B","C","D"),n,replace=TRUE)
  
##===Generate the dates of readings
##Initial admission date
sdat.AD<-sample(1:31,n,replace=TRUE)
##Initial discharge date
sdat.DD<-sdat.AD+ndays-1

AD.shift<-ifelse(sdat.DD>31,sdat.DD-31,0)
AD.shift[which(AD.shift>0)]<-AD.shift[which(AD.shift>0)]+sample(0:(30-max(ndays)),sum(AD.shift>0),replace=TRUE)

#cbind(sdat.AD,sdat.DD,sdat.DD-sdat.AD+1,ndays,AD.shift)
sdat.AD<-sdat.AD-AD.shift
sdat.DD<-sdat.AD+ndays-1

#cbind(sdat.AD,sdat.DD,sdat.DD-sdat.AD+1,ndays,AD.shift)
summary(sdat.AD)
summary(sdat.DD)

##Generate the variables for the simulated data
id<-rep(1:n,times=ndays)
loc<-rep(location,times=ndays)
day<-rep(sdat.AD,times=ndays)+unlist(sapply(ndays,function(x) 0:(x-1)))
mon<-rep(7,length(day))
year<-rep(2020,length(day))

##For format1
date1<-paste(mon,day,year,sep="/")
##For format2
date2<-paste(day,mon,year,sep="/")

##===Generate the variables for the dataset
sdat.ADMISSION.ID<-rep(id,  times=nreadings)
sdat.RESULT.DATE1<-rep(date1,times=nreadings)
sdat.RESULT.DATE2<-rep(date2,times=nreadings)
sdat.LOCATION<-    rep(loc, times=nreadings)

sdat.RESULT<-as.numeric(paste(rpois(length(sdat.ADMISSION.ID),13),sample(0:9,length(sdat.ADMISSION.ID),replace=TRUE),sep="."))
summary(sdat.RESULT)
sdat.RESULT<-as.character(sdat.RESULT)

##For format1: generate hours and minutes
hh<-sample(c(paste("0",c(1:9),sep=""),10:23),length(sdat.ADMISSION.ID), replace=TRUE)
mm<-sample(c(paste("0",c(1:9),sep=""),10:59),length(sdat.ADMISSION.ID), replace=TRUE)
sdat.RESULT.DATE1<-paste(sdat.RESULT.DATE1,paste(hh,mm,sep=":"),sep=" ")

##Gather all the variables together
sdat<-data.frame(ADMISSION.ID=sdat.ADMISSION.ID,
                 RESULT=sdat.RESULT,
                 RESULT.DATE1=sdat.RESULT.DATE1,
                 RESULT.DATE2=sdat.RESULT.DATE2,
                 LOCATION=sdat.LOCATION)

##Order by timing then Admission ID
o<-order(anytime(sdat$RESULT.DATE1),sdat$ADMISSION.ID)
sdat<-sdat[o,]

##Rename the Admission ID
unique(sdat$ADMISSION.ID)
sdat$ADMISSION.ID.OLD<-sdat$ADMISSION.ID
j<-1
for(i in unique(sdat$ADMISSION.ID)){
  sdat$ADMISSION.ID[which(sdat$ADMISSION.ID.OLD==i)]<-j
  j<-j+1
}

all(diff(unique(sdat$ADMISSION.ID))==1)

##Include problematic readings:
sdat$RESULT[sample(1:nrow(sdat),3)]<-c("a4.4","c7.7","d9.6")

##Format1:
sdat1<-sdat[,c("ADMISSION.ID","RESULT","RESULT.DATE1","LOCATION")]
colnames(sdat1)[which(colnames(sdat1)=="RESULT.DATE1")]<-"RESULT.DATE"
write.table(sdat1,file="test_format1.csv",row.names = FALSE, sep=",")

##Format2:
sdat2<-sdat[,c("ADMISSION.ID","RESULT","RESULT.DATE2","LOCATION")]
colnames(sdat2)[which(colnames(sdat2)=="RESULT.DATE2")]<-"RESULT.DATE"
for(i in c("A","B","C","D")){
  
  tmp<-sdat2[which(sdat2$LOCATION==i),1:3]
  write.table(tmp,file=paste("test_format2_",i,".csv",sep=""),row.names = FALSE,col.names=FALSE,sep=",")
  
}



##===Checking if the format 1 and format 2 are the same
##Read in format 1
info1<-read.csv("test_format1.csv")
info1$RESULT.DATE<-as.character(as.Date(strptime(info1$RESULT.DATE,"%m/%d/%Y %H:%M")))

##Read in format 2
info2<-NULL
for(i in c("A","B","C","D")){
  
  tt<-cbind(read.csv(paste("test_format2_",i,".csv",sep=""),header=FALSE),i)
  info2<-rbind(info2,tt)
  
}

colnames(info2)<-colnames(info1)
info2$RESULT.DATE<-as.character(strptime(info2$RESULT.DATE,"%d/%m/%Y"))

##Sort and compare the two data
o1<-order(info1$ADMISSION.ID,info1$RESULT)
o2<-order(info2$ADMISSION.ID,info2$RESULT)
info1<-info1[o1,]
info2<-info2[o2,]
row.names(info1)<-NULL
row.names(info2)<-NULL

all.equal(info1,info2)


