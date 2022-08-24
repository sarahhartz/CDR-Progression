nacc.table2<-function(dat){
  
  n1=table(dat$event.names)
  n0=sum(n1)
  
  f0.n=table(dat$female.names)
  f1.n=table(dat$female.names,dat$event.names)
  
  f0=round(prop.table(table(dat$female.names)),2)
  f1=round(prop.table(table(dat$female.names,dat$event.names),margin=2),2)
  
  r0.n=table(dat$race3)
  r1.n=table(dat$race3,dat$event.names)
  
  r0=round(prop.table(table(dat$race3)),2)
  r1=round(prop.table(table(dat$race3,dat$event.names),margin=2),2)
  
  p0.n=table(dat$parent.text)
  p1.n=table(dat$parent.text,dat$event.names)
  
  p0=round(prop.table(table(dat$parent.text)),2)
  p1=round(prop.table(table(dat$parent.text,dat$event.names),margin=2),2)
  
  e0.n=table(dat$edu.text)
  e1.n=table(dat$edu.text,dat$event.names)
  
  e0=round(prop.table(table(dat$edu.text)),2)
  e1=round(prop.table(table(dat$edu.text,dat$event.names),margin=2),2)
  
  c1=c(n0,f0.n,r0.n,p0.n,e0.n)
  c2=c(n1[1],f1.n[,1],r1.n[,1],p1.n[,1],e1.n[,1])
  c3=c(NA,f1[,1],r1[,1],p1[,1],e1[,1])
  c4=c(n1[2],f1.n[,2],r1.n[,2],p1.n[,2],e1.n[,2])
  c5=c(NA,f1[,2],r1[,2],p1[,2],e1[,2])
  
  m0=cbind(c1,c2,c3,c4,c5)
  m1=rownames(m0)
  blank.row=rep(NA,dim(m0)[2])
  m2=rbind(m0[1,],blank.row, m0[2:3,], blank.row, m0[4:6,], blank.row, m0[7:8,], blank.row, m0[9:10,])
  blank.col=rep("|",dim(m2)[1])
  mat1=cbind(m2[,1], blank.col, m2[,2:3], blank.col, m2[,4:5])
  rownames(mat1)=c("N","---",m1[2:3],"---",m1[4:6],"---",m1[7:8],"---",m1[9:10])
  colnames(mat1)=c("Full Sample","|","censored N","p","|","deceased N","p")
  mat1}
