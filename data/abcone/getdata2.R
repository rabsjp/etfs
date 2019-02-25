rm(list = ls())
setwd("/cloud/project/data/abone")
files = list.files(pattern="*.csv")
numa<-2
filename<-files[numa]
df<-read.csv(filename,sep=",",header=T, stringsAsFactors = FALSE)

important_names<-c(1,14:57)
df<-df[,important_names]
df$market<-2003
df$market[df$subsession.round_number>15]<-df$market[df$subsession.round_number>15]+1

df$subsession.round_number[df$subsession.round_number>15]<-df$subsession.round_number[df$subsession.round_number>15]+2

df$type<-0
df$type[df$participant.id_in_session>3]<-1
df$type[df$participant.id_in_session>6]<-2
df$typeperiod<-df$subsession.round_number*10+df$type
df$portab<-(df$player.n_a+df$player.t_a-df$player.n_b-df$player.t_b)^2/3
df$portdifab<-(df$player.n_a+df$player.t_a-df$player.n_b-df$player.t_b)/3
df$portgroup<-ave(df$portab,df$subsession.round_number,FUN=function(x) sum(x, na.rm=T))
df$agroup<-ave(df$player.n_a+df$player.t_a,df$typeperiod,FUN=function(x) sum(x, na.rm=T))
df$bgroup<-ave(df$player.n_b+df$player.t_b,df$typeperiod,FUN=function(x) sum(x, na.rm=T))
df$portype<-ave(df$portab,df$typeperiod,FUN=function(x) sum(x, na.rm=T))
df$portdiftype<-ave(df$portdifab,df$typeperiod,FUN=function(x) sum(x, na.rm=T))

#compute FVs
df$fva<-10
df$fvb<-df$subsession.ronda-8
df$fvb[df$fvb<0]<-0
df$fvb<-18-df$fvb

df$group.p_a[df$group.p_a==0]<-NA
df$group.p_b[df$group.p_b==0]<-NA
df$rae_a<-abs(df$group.p_a/df$fva-1)
df$rae_b<-abs(df$group.p_b/df$fvb-1)

df$dp_ba<-abs(df$group.p_b/(df$group.p_a+df$fvb-df$fva)-1)
df$dpj_ba<-abs((df$group.p_b/(df$group.p_a))/(df$fvb/df$fva)-1)

df$mrae_a<-ave(df$rae_a,df$market,FUN=function(x) mean(x, na.rm=T))
df$mrae_b<-ave(df$rae_b,df$market,FUN=function(x) mean(x, na.rm=T))

df$mdp_ba<-ave(df$dp_ba,df$market,FUN=function(x) mean(x, na.rm=T))
df$mdpj_ba<-ave(df$dpj_ba,df$market,FUN=function(x) mean(x, na.rm=T))

market_ind<-unique(df[,c("market","mrae_a","mrae_b","mdp_ba","mdpj_ba")])

unique(ave(df$group.q_a,df$market,FUN=function(x) sum(x, na.rm=T)))/9


png(paste("prices",numa,".png",sep=""))
plot(df$subsession.round_number,df$group.p_b,ylim=c(0,50),col="red",xaxt = "n",xlab="period",ylab = "prices")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
points(df$subsession.round_number,df$group.p_a,col="gray",pch=2)
legend(1, 40, c("B","A"), col = c("red","gray"), pch = c(1,2))
dev.off()


png(paste("marketq",numa,".png",sep=""))
plot(df$subsession.round_number,df$group.q_b,ylim=c(0,20),col="red",type="l",xaxt = "n",xlab="period",ylab = "q")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number,df$group.q_a,col="gray")
rect(15, 0,   18, 20, col = "white", border = "white", lwd = 2)
legend(1, 15, c("B","A"), col = c("red","gray"), lty = c(1,1))
dev.off()

png(paste("dport",numa,".png",sep=""))
plot(df$subsession.round_number[df$type==2],df$portype[df$type==2],ylim=c(0,560),type="l",col="black",lty=1,xaxt = "n",xlab="period",ylab = "(a-b)^2")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number[df$type==1],df$portype[df$type==1],ylim=c(0,560),type="l",col="black",lty=2)
lines(df$subsession.round_number[df$type==0],df$portype[df$type==0],ylim=c(0,560),type="l",col="black",lty=3)
dev.off()

png("difport.png")
plot(df$subsession.round_number[df$type==2],df$portdiftype[df$type==2],ylim=c(-20,20),type="l",col="black",lty=1,xaxt = "n",xlab="period",ylab = "|a-b|")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number[df$type==1],df$portdiftype[df$type==1],col="black",lty=2)
lines(df$subsession.round_number[df$type==0],df$portdiftype[df$type==0],col="black",lty=3)
dev.off()

png("holdings_gc.png")
plot(df$subsession.round_number[df$type==2],df$agroup[df$type==2],ylim=c(0,60),type="l",col="gray",lty=1,xaxt = "n",xlab="period",ylab = "holdings type c")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number[df$type==2],df$bgroup[df$type==2],col="red")
dev.off()

png("holdings_gb.png")
plot(df$subsession.round_number[df$type==2],df$agroup[df$type==1],ylim=c(0,40),type="l",col="gray",lty=1,xaxt = "n",xlab="period",ylab = "holdings type b")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number[df$type==2],df$bgroup[df$type==1],col="red")
dev.off()

png("holdings_ga.png")
plot(df$subsession.round_number[df$type==2],df$agroup[df$type==0],ylim=c(0,40),type="l",col="gray",lty=1,xaxt = "n",xlab="period",ylab = "holdings type a")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number[df$type==2],df$bgroup[df$type==0],col="red")
dev.off()


#plot(df$subsession.round_number[df$type==1],df$cgroup[df$type==1],ylim=c(0,40),type="l",col="black",lty=2)
