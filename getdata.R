rm(list = ls())
setwd("/cloud/project/project/data/")
files = list.files(pattern="etfs.*")
filename<-files[2]
df<-read.csv(filename,sep=",",header=T, stringsAsFactors = FALSE)

important_names<-c(1,14:57)
df<-df[,important_names]
df$subsession.round_number[df$subsession.round_number>15]<-df$subsession.round_number[df$subsession.round_number>15]+2

df$type<-0
df$type[df$participant.id_in_session>3]<-1
df$type[df$participant.id_in_session>6]<-2
df$typeperiod<-df$subsession.round_number*10+df$type
df$portab<-(df$player.n_a-df$player.n_b)^2/3
df$portdifab<-(df$player.n_a-df$player.n_b)/3
df$portgroup<-ave(df$portab,df$subsession.round_number,FUN=function(x) sum(x, na.rm=T))
df$agroup<-ave(df$player.n_a,df$typeperiod,FUN=function(x) sum(x, na.rm=T))
df$bgroup<-ave(df$player.n_b,df$typeperiod,FUN=function(x) sum(x, na.rm=T))
df$cgroup<-ave(df$player.n_c,df$typeperiod,FUN=function(x) sum(x, na.rm=T))
df$portype<-ave(df$portab,df$typeperiod,FUN=function(x) sum(x, na.rm=T))
df$portdiftype<-ave(df$portdifab,df$typeperiod,FUN=function(x) sum(x, na.rm=T))


#plot(df$subsession.round_number,df$portgroup,ylim=c(0,500),col="blue")

#plot(df$subsession.round_number[df$type==0],df$portype[df$type==0],ylim=c(0,500),col="blue")
#plot(df$subsession.round_number,df$group.p_b-df$group.p_a,ylim=c(-40,40),col="blue",xaxt = "n",xlab="period",ylab = "prices")

pdf("prices.pdf")
plot(df$subsession.round_number,df$group.p_c,ylim=c(0,100),col="blue",xaxt = "n",xlab="period",ylab = "prices")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number,df$group.nav,col="blue")
points(df$subsession.round_number,df$group.p_a,col="gray",pch=2)
#lines(df$subsession.round_number,df$group.nav_a,col="black")
points(df$subsession.round_number,df$group.p_b,col="red",pch=3)
#lines(df$subsession.round_number,df$group.nav_b,col="red")
dev.off()

pdf("marketq.pdf")
plot(df$subsession.round_number,df$group.q_c,ylim=c(0,10),col="blue",type="l",xaxt = "n",xlab="period",ylab = "q")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number,df$group.q_a,col="gray")
lines(df$subsession.round_number,df$group.q_b,col="red")
rect(15, 0,   18, 10, col = "white", border = "white", lwd = 2)
dev.off()

pdf("dport.pdf")
plot(df$subsession.round_number[df$type==2],df$portype[df$type==2],ylim=c(0,80),type="l",col="black",lty=1,xaxt = "n",xlab="period",ylab = "disersion a-b")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number[df$type==1],df$portype[df$type==1],ylim=c(0,80),type="l",col="black",lty=2)
lines(df$subsession.round_number[df$type==0],df$portype[df$type==0],ylim=c(0,80),type="l",col="black",lty=3)
dev.off()

pdf("dport.pdf")
plot(df$subsession.round_number[df$type==2],df$portype[df$type==2],ylim=c(0,80),type="l",col="black",lty=1,xaxt = "n",xlab="period",ylab = "disersion a-b")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number[df$type==1],df$portype[df$type==1],ylim=c(0,80),type="l",col="black",lty=2)
lines(df$subsession.round_number[df$type==0],df$portype[df$type==0],ylim=c(0,80),type="l",col="black",lty=3)
dev.off()

pdf("difport.pdf")
plot(df$subsession.round_number[df$type==2],df$portdiftype[df$type==2],ylim=c(-20,20),type="l",col="black",lty=1,xaxt = "n",xlab="period",ylab = "disersion a-b")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number[df$type==1],df$portdiftype[df$type==1],col="black",lty=2)
lines(df$subsession.round_number[df$type==0],df$portdiftype[df$type==0],col="black",lty=3)
dev.off()

pdf("holdings_gc.pdf")
plot(df$subsession.round_number[df$type==2],df$agroup[df$type==2],ylim=c(0,40),type="l",col="gray",lty=1,xaxt = "n",xlab="period",ylab = "holdings type c")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number[df$type==2],df$bgroup[df$type==2],col="red")
lines(df$subsession.round_number[df$type==2],df$cgroup[df$type==2],col="blue")
dev.off()

pdf("holdings_gb.pdf")
plot(df$subsession.round_number[df$type==2],df$agroup[df$type==1],ylim=c(0,40),type="l",col="gray",lty=1,xaxt = "n",xlab="period",ylab = "holdings type b")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number[df$type==2],df$bgroup[df$type==1],col="red")
lines(df$subsession.round_number[df$type==2],df$cgroup[df$type==1],col="blue")
dev.off()

pdf("holdings_ga.pdf")
plot(df$subsession.round_number[df$type==2],df$agroup[df$type==0],ylim=c(0,40),type="l",col="gray",lty=1,xaxt = "n",xlab="period",ylab = "holdings type a")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number[df$type==2],df$bgroup[df$type==0],col="red")
lines(df$subsession.round_number[df$type==2],df$cgroup[df$type==0],col="blue")
dev.off()


#plot(df$subsession.round_number[df$type==1],df$cgroup[df$type==1],ylim=c(0,40),type="l",col="black",lty=2)
