rm(list = ls())
#setwd("/cloud/project/data/")
setwd("~/Desktop/jotarepos/etfs/data/")

## Call the data. unique id per session. 
load("etfmarket.Rda")

fundab<-c(rep(18,8),18-seq(1:7))
fundab/10
df$idsu<-df$subsession.round_number*100+df$tre

df$pmean_a<-ave(df$group.p_a,df$idsu,FUN=function(x) median(x, na.rm=T))
df$pmean_b<-ave(df$group.p_b,df$idsu,FUN=function(x) median(x, na.rm=T))
df$pmean_c<-ave(df$group.p_c,df$idsu,FUN=function(x) median(x, na.rm=T))
df$navmeans<-df$pmean_a+df$pmean_b
df$rel.price<-ave(df$group.p_b/df$group.p_a,df$idsu,FUN=function(x) median(x, na.rm=T))

png(paste("price_rel_1",".png",sep=""))
plot(df$subsession.round_number[df$tre==0],df$rel.price[df$tre==0],ylim=c(.5,2.5),col="black",xaxt = "n",xlab="period",ylab = "price B / price A",pch=20)
axis(1, at=c(1:30), labels=c((1:15),(1:15)))
points(df$subsession.round_number[df$tre==1],df$rel.price[df$tre==1],col="black",lwd=2,pch=25)
#points(df$subsession.round_number[df$tre==2],df$rel.price[df$tre==2],col="blue",lwd=1,pch=23)
#points(df$subsession.round_number[df$tre==3],df$rel.price[df$tre==3],col="blue",lwd=1,pch=25)
lines(seq(1:15),fundab/10,col="gray",lwd=3)
lines(seq(1:15)+15,fundab/10,col="gray",lwd=3)
legend(13, .8, c("2N","3N", "FV B / FV A"), col = c("black","black","gray"), lwd=c(1,2,1), lty = c(NA,NA,1), pch = c(20,25,NA),bty = "n",ncol=2)
dev.off()

png(paste("price_rel_3",".png",sep=""))
plot(df$subsession.round_number[df$tre==2],df$rel.price[df$tre==2],ylim=c(.5,2.5),col="black",xaxt = "n",xlab="period",ylab = "price B / price A",pch=20)
axis(1, at=c(1:30), labels=c((1:15),(1:15)))
points(df$subsession.round_number[df$tre==3],df$rel.price[df$tre==3],col="black",lwd=2,pch=25)
#points(df$subsession.round_number[df$tre==2],df$rel.price[df$tre==2],col="blue",lwd=1,pch=23)
#points(df$subsession.round_number[df$tre==3],df$rel.price[df$tre==3],col="blue",lwd=1,pch=25)
lines(seq(1:15),fundab/10,col="gray",lwd=3)
lines(seq(1:15)+15,fundab/10,col="gray",lwd=3)
legend(13, .8, c("2Z","3Z", "FV B / FV A"), col = c("black","black","gray"), lwd=c(1,2,1), lty = c(NA,NA,1), pch = c(20,25,NA),bty = "n",ncol=2)
dev.off()


for(trata in c(1,3)){
  png(paste("prices_sum",trata,".png",sep=""))
  plot(df$subsession.round_number[df$tre==trata],df$pmean_a[df$tre==trata],ylim=c(0,80),col="gray",xaxt = "n",xlab="period",ylab = "prices",pch=2)
  axis(1, at=c(1:30), labels=c((1:15),(1:15)))
  lines(df$subsession.round_number[df$tre==trata],df$fva[df$tre==trata],col="gray",lwd=2)
  points(df$subsession.round_number[df$tre==trata],df$pmean_b[df$tre==trata],col="red",pch=3)
  lines(seq(1:15),fundab,col="red")
  lines(seq(1:15)+15,fundab,col="red")
  points(df$subsession.round_number[df$tre==trata],df$pmean_c[df$tre==trata],col="blue",pch=1)
  lines(seq(1:15),fundab+10,col="blue",lty=2)
  lines(seq(1:15)+15,fundab+10,col="blue",lty=2)
  abline(v=15,col='gray',lty=3,lwd=2)
  points(df$subsession.round_number[df$tre==trata & df$subsession.round_number<16],df$pmean_b[df$tre==trata & df$subsession.round_number<16]+df$pmean_a[df$tre==trata & df$subsession.round_number<16],col="black",lty=2,pch=22)
  points(df$subsession.round_number[df$tre==trata & df$subsession.round_number>15],df$pmean_b[df$tre==trata & df$subsession.round_number>15]+df$pmean_a[df$tre==trata & df$subsession.round_number>15],col="black",lty=2,pch=22)
  legend(2, 75, c("A","FV A","B","FV B","C","FV C", "NAV"), col = c("gray","gray","red","red","blue","blue","black"), pch = c(2,NA,3,NA,1,NA,22),lty = c(NA,1,NA,1,NA,2,NA),bty = "n",ncol=4)
  dev.off()
}


for(trata in c(0,2)){
  png(paste("prices_sum",trata,".png",sep=""))
  plot(df$subsession.round_number[df$tre==trata],df$pmean_a[df$tre==trata],ylim=c(0,80),col="gray",xaxt = "n",xlab="period",ylab = "prices",pch=2)
  axis(1, at=c(1:30), labels=c((1:15),(1:15)))
  lines(df$subsession.round_number[df$tre==trata],df$fva[df$tre==trata],col="gray",lwd=2)
  points(df$subsession.round_number[df$tre==trata],df$pmean_b[df$tre==trata],col="red",pch=3)
  lines(seq(1:15),fundab,col="red")
  abline(v=15,col='gray',lty=3,lwd=2)
  lines(seq(1:15)+15,fundab,col="red")
  legend(2, 75, c("A","FV A","B","FV B"), col = c("gray","gray","red","red"), pch = c(2,NA,3,NA),lty = c(NA,1,NA,1),bty = "n",ncol=2)
  dev.off()
}

## We start with 3 assets and -1 correlation
listofsessions<-c("30041","30051")

for(imp_sessions in listofsessions){
  dplot<-df[df$uses==imp_sessions,]
  png(paste("prices",imp_sessions,".png",sep=""))
  plot(dplot$subsession.ronda,dplot$group.p_a,ylim=c(0,140),col="gray",xaxt = "n",xlab="period",ylab = "prices",pch=2)
  axis(1, at=c(1:15), labels=c(1:15))
  lines(dplot$subsession.ronda,dplot$fva,col="gray")
  points(dplot$subsession.ronda,dplot$group.p_b,col="red",pch=3)
  lines(dplot$subsession.ronda,dplot$fvb,col="red")
  points(dplot$subsession.ronda,dplot$group.p_c,col="blue",pch=1)
  lines(dplot$subsession.ronda,dplot$fvc,col="blue")
  lines(dplot$subsession.ronda,dplot$group.nav,col="black",lty=2)
  legend(2, 120, c("A","FV A","B","FV B","C","FV C", "NAV"), col = c("gray","gray","red","red","blue","blue","black"), pch = c(2,NA,3,NA,1,NA,NA),lty = c(NA,1,NA,1,NA,1,2),bty = "n",ncol=4)
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
listofsessions<-c("20011","20021","20031","20041","20051")
for(imp_sessions in listofsessions){
  dplot<-df[df$uses==imp_sessions,]
  png(paste("prices",imp_sessions,".png",sep=""))
  plot(dplot$subsession.ronda,dplot$group.p_a,ylim=c(0,80),col="gray",xaxt = "n",xlab="period",ylab = "prices",pch=2)
  axis(1, at=c(1:15), labels=c(1:15))
  lines(dplot$subsession.ronda,dplot$fva,col="gray")
  points(dplot$subsession.ronda,dplot$group.p_b,col="red",pch=1)
  lines(dplot$subsession.ronda,dplot$fvb,col="red")
  legend(10.5, 68, c("A","FV A","B","FV B"), col = c("gray","gray","red","red"), pch = c(2,NA,1,NA),lty=c(NA,1,NA,1),bty="n",ncol=2)
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

##Individual's risk responses
rm(list = ls())
setwd("~/Desktop/jotarepos/etfs/data/")
## Call the data. 
load("etfalldata.Rda")
library("plm")
dbox<-df[df$subsession.round_number==30,c("tre","uses","player.boxes_collected")]
#set.seed(100)
p2N <- hist(dbox$player.boxes_collected[dbox$tre==0],breaks=20,title="")
p2Z <- hist(dbox$player.boxes_collected[dbox$tre==2],breaks=20,title="")
p3N <- hist(dbox$player.boxes_collected[dbox$tre==1],breaks=20,title="")
p3Z <- hist(dbox$player.boxes_collected[dbox$tre==3],breaks=20,title="")
#wilcox.test(dbox$player.boxes_collected[dbox$tre==2],dbox$player.boxes_collected[dbox$tre==3])
#p2 <- hist(dbox$boxes[dbox$tre==0],breaks=20,ylim=c(0,12))                # centered at 6
png("histb3Z.png")
plot(p3Z,ylim=c(0,10),xlim=c(0,100), col="black",border=F,main=NULL,xlab="boxes collected")  # first histogram
legend(70,8,c("3Z"),fill=c("black"),bty="n",border=F)
dev.off()

