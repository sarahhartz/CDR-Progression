date.text<-format(Sys.Date(), "%m-%d")
data.dir="C:/Users/hartzs/Box/!@ Projects/AD Risk/Dementia outcomes/CDR progression/"

dat<-read.csv(paste(data.dir,"cdr05ad_1.csv",sep=""))
cdr05.1<-varfile(dat)

dat<-read.csv(paste(data.dir,"cdr1ad_23.csv",sep=""))
cdr1.23<-varfile(dat)

dat<-read.csv(paste(data.dir,"cdr2ad_3.csv",sep=""))
cdr2.3<-varfile(dat)

dat<-read.csv(paste(data.dir,"cdr3_death.csv",sep=""))
cdr3.death<-varfile(dat)
