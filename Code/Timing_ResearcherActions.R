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

#loop to create a total flight number regardless of flight date (should be 1 to 80) 
hand$TOTno<-c(1,rep(NA,nrow(hand)-1))
for(i in 2:nrow(hand)){
  hand$TOTno[i]<-ifelse(hand$Idyear[i]!=hand$Idyear[i-1],hand$TOTno[i-1]+1,hand$TOTno[i-1])
}  
head(hand)
unique(hand$TOTno)
unique(hand$Idyear)
unique(hand$BearID)


###read in HR data

HR<-readRDS("E:/ZooLaptop_062817/Bear/PostDoc/BearWare_HRData/All_BWdata_Merge/HeartRate_BW_2012Dec2017_012718.R")
head(HR)

heartA$Hdiff<-c(0,diff(heartA$Hrdata))

names(heartA)
heartD<-data.frame(matrix(NA,0,11))
colnames(heartD)<-names(heartA)


#Subset heart rate data (i = hand$TOTno), based on timing within the TOTno (+1 hour???)

for(i in 1:length(unique(DroneFLC$julFL))){
  #heart<-subset(BearHR,subset=BearHR$EndTimeAllS>=BearMV$dtPmL[i]&BearHR$EndTimeAllS<=BearMV$dtPmL[i+1])
  subheart<-subset(heartA,subset=heartA$SrtTimeAllSadj>=(min(DroneFLC$DTL[i]-(60*15)))&heartA$SrtTimeAllSadj<=(min(DroneFLC$DTL[i])+(60*15)))
  heartD<- rbind(heartD,subheart)
  
}
hhshs new cganges scsckxjklsjkas kalalka klsklsklsklskls


