rm(list = ls())
#setwd("/cloud/project/data/")
setwd("~/Desktop/jotarepos/etfs/data/")

## Call the data. unique id per session. 
load("etfmarket.Rda")

## We start with 3 assets and -1 correlation
listofsessions<-c("31031","31041")

for(imp_sessions in listofsessions){
  dplot<-df[df$uses==imp_sessions,]
  png(paste("prices",imp_sessions,".png",sep=""))
  plot(dplot$subsession.ronda,dplot$group.p_a,ylim=c(0,80),col="gray",xaxt = "n",xlab="period",ylab = "prices",pch=2)
  axis(1, at=c(1:15), labels=c(1:15))
  lines(dplot$subsession.ronda,dplot$fva,col="gray")
  points(dplot$subsession.ronda,dplot$group.p_b,col="red",pch=3)
  lines(dplot$subsession.ronda,dplot$fvb,col="red")
  points(dplot$subsession.ronda,dplot$group.p_c,col="blue",pch=1)
  lines(dplot$subsession.ronda,dplot$fvc,col="blue")
  lines(dplot$subsession.ronda,dplot$group.nav,col="black",lty=2)
  legend(2, 75, c("A","FV A","B","FV B","C","FV C", "NAV"), col = c("gray","gray","red","red","blue","blue","black"), pch = c(2,NA,3,NA,1,NA,NA),lty = c(NA,1,NA,1,NA,1,2),bty = "n",ncol=4)
  dev.off()
}

for(imp_sessions in listofsessions){
  dplot<-df[df$uses==imp_sessions,]
  png(paste("qs",imp_sessions,".png",sep=""))
  plot(dplot$subsession.ronda,cumsum(dplot$group.q_a)/90,ylim=c(0,1),col="gray",type="l",xaxt = "n",xlab="period",ylab = "cumulative quantity (as a fraction of total assets 90)")
  axis(1, at=c(1:15), labels=c(1:15))
  lines(dplot$subsession.ronda,cumsum(dplot$group.q_b)/90,col="red")
  lines(dplot$subsession.ronda,cumsum(dplot$group.q_c)/90,col="blue")
  legend(2, .9, c("A","B","C"), col = c("gray","red","blue"),lty = c(1,1,1),bty = "n")
  dev.off()
}


## Now, 2 assets and -1 correlation
listofsessions<-c("21010","21011")

for(imp_sessions in listofsessions){
  dplot<-df[df$uses==imp_sessions,]
  png(paste("prices",imp_sessions,".png",sep=""))
  plot(dplot$subsession.ronda,dplot$group.p_a,ylim=c(0,50),col="gray",xaxt = "n",xlab="period",ylab = "prices",pch=2)
  axis(1, at=c(1:15), labels=c(1:15))
  lines(dplot$subsession.ronda,dplot$fva,col="gray")
  points(dplot$subsession.ronda,dplot$group.p_b,col="red",pch=1)
  lines(dplot$subsession.ronda,dplot$fvb,col="red")
  legend(10.5, 48, c("A","FV A","B","FV B"), col = c("gray","gray","red","red"), pch = c(2,NA,1,NA),lty=c(NA,1,NA,1),bty="n",ncol=2)
  dev.off()
}

for(imp_sessions in listofsessions){
  dplot<-df[df$uses==imp_sessions,]
  png(paste("qs",imp_sessions,".png",sep=""))
  plot(dplot$subsession.ronda,cumsum(dplot$group.q_a)/120,ylim=c(0,1),col="gray",type="l",xaxt = "n",xlab="period",ylab = "cumulative quantity (as a fraction of total assets 120)")
  axis(1, at=c(1:15), labels=c(1:15))
  lines(dplot$subsession.ronda,cumsum(dplot$group.q_b)/120,col="red")
  legend(2, .9, c("A","B"), col = c("gray","red"),lty = c(1,1),bty = "n")
  dev.off()
}


