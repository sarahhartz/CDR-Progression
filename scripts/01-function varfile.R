varfile<-function(dat){
  age<-dat$naccage_t1
  dat$age_from65=age-65
  dat$age2=dat$age_from65^2
  dat$parent01=ifelse(dat$parent_t1==1,1,0)
  
  dat$age.group5=ifelse(age<65,"60-64",
                        ifelse(age<70,"65-69",
                               ifelse(age<75,"70-74",
                                      ifelse(age<80,"75-79",       
                                             ifelse(age<85,"80-84","85+")))))
  
  dat$age.group5b=ifelse(age<70,"60-69",
                         ifelse(age<75,"70-74",
                                ifelse(age<80,"75-79",       
                                       ifelse(age<85,"80-84","85+"))))
  
  
  
  dat$age.group10=ifelse(age<70,"60-69",
                         ifelse(age<80,"70-79","80+"))
  
  dat<-dat[!is.na(dat$yrs),]
  dat$event.names=ifelse(dat$event==0,"censored",
                         ifelse(dat$event==1,"AD progression","deceased"))
  dat$female.names=ifelse(dat$female==0,"male","female")
  dat$race3=ifelse((dat$race2=="asian")|(dat$race2=="hispanic"),"other",dat$race2)
  dat$parent.text=ifelse(dat$parent01==0,"parental AD","no parental AD")
  dat$edu.text=ifelse(dat$edu_gt12==0,"HS degree or less","education beyond HS")
  
  dat
}