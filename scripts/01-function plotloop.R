plot.loop<-function(){
  f.status1=dat1$event
  f.time1=dat1$yrs
  f.age=dat1$age
  
  any1<-ifelse(f.status1==2,1,f.status1)
  AD.status=ifelse(f.status1==2,0,f.status1)
  death1=ifelse(f.status1==2,1,0)
  
  t1=seq(0,t.max,by=0.1)
  
  a<-cuminc(f.time1,any1)
  b<-timepoints(a,t1)
  any1.inc.y=b$est
  
  a<-cuminc(f.time1,AD.status)
  b<-timepoints(a,t1)
  AD1.inc.y=b$est
  
  surv.death=1-any1.inc.y
  surv.AD1=1-AD1.inc.y
  
  x.upper=t.max
  plot(c(0,x.upper),c(y.lower-0.2,y.upper),col="white",
       xlab="",ylab="",
       xlim=c(x.lower,x.upper),ylim=c(y.lower-0.2,y.upper+0.05),
       axes=FALSE);
  axis1<-seq(x.lower,x.upper,by=tic.int);
  axis(1, pos=0,labels=axis1,at=axis1);
  axis(2, pos=x.cross,labels=seq(y.lower,y.upper,0.25),at=seq(y.lower,y.upper,0.25));  
  lines(t1,surv.AD1,col=col.array[2]);
  lines(t1,surv.death);
  polygon(c(t1,x.upper,0),c(surv.AD1,1,1),col= adjustcolor(col.array[1], alpha=9/10),border=NA);
  polygon(c(t1,rev(t1)),c(surv.death, rev(surv.AD1)),col= adjustcolor(col.array[2], alpha=9/10),border=NA);
  
  a1<-survfit(Surv(f.time1,any1)~1)
  b1<-summary(a1,times=seq(x.lower,x.upper,by=tic.int))
  
  axis(1,pos=Ns.pos,labels=b1$n.risk,at=b1$time,lty=0,col.axis=col.array[3]);
  mtext(x.text,side=1,line=x.text.pos);
  mtext("N at risk",side=1,line=Ns.pos+0.5,at=left.table,col=col.array[3]);
  
  mtext(main.text,side=3,line=0,cex=2);
  
  AD.10y=1-min(surv.AD1,na.rm = T)
  AD.convert.05=-9
  AD.convert.95=-9
  AD.convert.5=-9
  for(i in 1:length(surv.AD1)){
    if((AD.convert.05<0)&(surv.AD1[i]<1-p.lim/2*AD.10y)){AD.convert.05=t1[i]}
    if((AD.convert.95<0)&(surv.AD1[i]<1-(1-p.lim/2)*AD.10y)){AD.convert.95=t1[i]}
    if((AD.convert.5<0)&(surv.AD1[i]<1-0.5*AD.10y)){AD.convert.5=t1[i]}}
  
  mtext(paste(round(AD.10y,2)*100,"% with ",t.max,"y AD progression. Of those who progress, median",sep=""),
        side=1,line=Ns.pos+2,adj=0,col=col.array[3])
  mtext(paste("progression is ",round(AD.convert.5,1),
              " years, ",round((1-p.lim)*100),"% CI ", 
              round(AD.convert.05,1), "-",
              round(AD.convert.95,1)," years.",sep=""),
        side=1,line=Ns.pos+3,adj=0,col=col.array[3])
  
}