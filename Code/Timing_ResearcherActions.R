library(chron)

setwd("E:/ZooLaptop_062817/Bear/PostDoc/HandlingImpacts/Data_R")
#' align handling timing with HR data
hand<-read.csv("HandlingTiming_030918.csv")
head(hand)
hand$X<-NULL
hand$X.1<-NULL

str(hand)

hand$Second1[is.na(hand$Second1)] <- 0
hand$Second2[is.na(hand$Second2)] <- 0

hand$time1 <- as.factor(paste(hand$Hour1, hand$Minute1, hand$Second1, sep = ":"))
hand$time1P <- chron(times=as.character(hand$time1))
hand$time2 <- as.factor(paste(hand$Hour2, hand$Minute2, hand$Second2, sep = ":"))
hand$time2P <- chron(times=as.character(hand$time2))


hand$dateP<- as.POSIXct(hand$Date, format= "%m/%d/%Y")
hand$dt1 <- as.factor(paste(hand$dateP, hand$time1P, sep = " "))
hand$dt1P<-strptime(hand$dt1, "%Y-%m-%d %H:%M:%S")
hand$dt1P<-as.POSIXct(hand$dt1P,tz="America/Chicago")

hand$dt2 <- as.factor(paste(hand$dateP, hand$time2P, sep = " "))
hand$dt2P<-strptime(hand$dt2, "%Y-%m-%d %H:%M:%S")
hand$dt2P<-as.POSIXct(hand$dt2P,tz="America/Chicago")

head(hand)

hand$Year<-format(hand$dt1P,"%Y")
head(hand)

hand$Idyear<-paste(hand$BearID,hand$Year,sep="_")

nobs<-nrow(hand)
hand$Idyear

#loop to create unique handling instances (den visits)
hand$TOTno<-c(1,rep(NA,nrow(hand)-1))
for(i in 2:nrow(hand)){
  hand$TOTno[i]<-ifelse(hand$Idyear[i]!=hand$Idyear[i-1],hand$TOTno[i-1]+1,hand$TOTno[i-1])
}  
head(hand)
unique(hand$TOTno)
unique(hand$Idyear)
unique(hand$BearID)


hand$Month<-format(hand$dt1P,"%m")
hand$Month<-as.numeric(hand$Month)
hand$Seas<-NA
hand$Seas[hand$Month>0&hand$Month<=4]<-"Spring"
hand$Seas[hand$Month>9&hand$Month<=12]<-"Dec"
table(hand$Seas)
hand$Idyearseas<-as.factor(paste(hand$BearID,hand$Year,hand$Seas,sep="_"))

###read in HR data

HR<-readRDS("E:/ZooLaptop_062817/Bear/PostDoc/BearWare_HRData/All_BWdata_Merge/HeartRate_BW_2012Dec2017_012718.R")
head(HR)

HR$Hdiff<-c(0,diff(HR$Hrdata))

names(HR)
heartD<-data.frame(matrix(NA,0,19))
colnames(heartD)<-names(HR)


HR$Year<-format(HR$TimeStampLocalADJ,"%Y")

HR$Month<-format(HR$TimeStampLocalADJ,"%m")
HR$Month<-as.numeric(HR$Month)
HR$Seas<-NA
HR$Seas[HR$Month>0&HR$Month<=4]<-"Spring"
HR$Seas[HR$Month>9&HR$Month<=12]<-"Dec"

HR$Idyear<-paste(HR$Bear,HR$Year,sep="_")
HR$Idyearseas<-as.factor(paste(HR$Bear,HR$Year,HR$Seas,sep="_"))


test<-HR[HR$Idyearseas[unique(hand$Idyearseas)]]

newdata <- HR[ which(HR$Idyearseas == unique(hand$Idyearseas)), ]

#03/21/2018
####MUST create a season as well to do subsetting (ID & year are not enough!!!!)

#Subset heart rate data (i = hand$TOTno), based on timing within the TOTno (+1 hour???)

for(i in 1:length(unique(hand$Idyearseas))){
  #heart<-subset(BearHR,subset=BearHR$EndTimeAllS>=BearMV$dtPmL[i]&BearHR$EndTimeAllS<=BearMV$dtPmL[i+1])
  subhand<-subset(hand,subset=hand$Idyearseas==unique(hand$Idyearseas)[i])
    subheart<-subset(HR,subset=HR$Idyearseas==unique(as.character(subhand$Idyearseas)))

  
  subheart2<-subset(subheart,subset=subheart$TimeStampLocalADJ>=(min(subhand$dt1P)-(60*60))&subheart$TimeStampLocalADJ<=(max(subhand$dt1P)+(60*60)))
  heartD<- rbind(heartD,subheart2)
  print(i)
}

heartD$Idyearseas<-droplevels(heartD$Idyearseas)
table(heartD$Idyearseas)

head(hand)
tail(heartD)




hand$TimeStampLocalADJ<-hand$dt1P

library(data.table) # v1.9.6+

heartD$Idyear<-as.factor(heartD$Idyear)
hand$Idyear<-as.factor(hand$Idyear)

names(heartD)
head(hand)

heartDHand<-data.frame(matrix(NA,0,24))
table(heartD$Idyearseas)
for(i in 1:length(unique(hand$Idyearseas))){
  subhand<-subset(hand,subset=hand$Idyearseas==unique(hand$Idyearseas)[i])
  subheart<-subset(heartD,subset=heartD$Idyearseas==unique(as.character(subhand$Idyearseas)))
subheart$Action<-setDT(subhand)[subheart, Action, roll = "nearest", on = "TimeStampLocalADJ"]
subheart$dt1P<-setDT(subhand)[subheart, dt1P, roll = "nearest", on = "TimeStampLocalADJ"]
subheart$Bear.Response<-setDT(subhand)[subheart, Bear.Response, roll = "nearest", on = "TimeStampLocalADJ"]
subheart$Bear.Location<-setDT(subhand)[subheart, Bear.Location, roll = "nearest", on = "TimeStampLocalADJ"]
subheart$Comment<-setDT(subhand)[subheart, Comment, roll = "nearest", on = "TimeStampLocalADJ"]

print(i)
#subheart[30:60,]
heartDHand<-rbind(heartDHand,subheart)
}


for(i in 1:length(unique(heartD$TOTno))){
  #heart<-subset(BearHR,subset=BearHR$EndTimeAllS>=BearMV$dtPmL[i]&BearHR$EndTimeAllS<=BearMV$dtPmL[i+1])
  subheart<-subset(HR,subset=HR$TimeStampLocalADJ>=(min(hand$dt1P[i]-(60*60)))&HR$TimeStampLocalADJ<=(max(hand$dt1P[i])+(60*60)))
  heartD<- rbind(heartD,subheart)
  
}












heartDH<-merge(heartD,hand,by="Idyear",all.x=TRUE)

head(heartDH)


tail(HR$TimeStampLocalADJ)

