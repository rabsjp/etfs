rm(list = ls())
setwd("/cloud/project/data/abcone")
files = list.files(pattern="*.csv")
numa<-2
filename<-files[numa]
df<-read.csv(filename,sep=",",header=T, stringsAsFactors = FALSE)

important_names<-c(1,14:57)
df<-df[,important_names]
df$market<-3002
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
df$cgroup<-ave(df$player.n_c+df$player.t_c,df$typeperiod,FUN=function(x) sum(x, na.rm=T))
df$portype<-ave(df$portab,df$typeperiod,FUN=function(x) sum(x, na.rm=T))
df$portdiftype<-ave(df$portdifab,df$typeperiod,FUN=function(x) sum(x, na.rm=T))

#compute FVs
df$fva<-10
df$fvb<-df$subsession.ronda-8
df$fvb[df$fvb<0]<-0
df$fvb<-18-df$fvb
df$fvc<-df$fvb+df$fva

df$group.p_a[df$group.p_a==0]<-NA
df$group.p_b[df$group.p_b==0]<-NA
df$group.p_c[df$group.p_c==0]<-NA
df$group.nav[df$group.nav==0]<-NA

df$rae_a<-abs(df$group.p_a/df$fva-1)
df$rae_b<-abs(df$group.p_b/df$fvb-1)
df$rae_c<-abs(df$group.p_c/df$fvc-1)
df$rae_cnav<-abs(df$group.p_c/df$group.nav-1)

df$dp_ba<-abs(df$group.p_b/(df$group.p_a+df$fvb-df$fva)-1)
df$dp_ca<-abs(df$group.p_c/(df$group.p_c+df$fvc-df$fva)-1)
df$dp_cb<-abs(df$group.p_c/(df$group.p_b+df$fvc-df$fvb)-1)


df$dpj_ba<-abs((df$group.p_b/(df$group.p_a))/(df$fvb/df$fva)-1)
df$dpj_ca<-abs((df$group.p_c/(df$group.p_a))/(df$fvc/df$fva)-1)
df$dpj_cb<-abs((df$group.p_c/(df$group.p_b))/(df$fvc/df$fvb)-1)

df$mrae_a<-ave(df$rae_a,df$market,FUN=function(x) mean(x, na.rm=T))
df$mrae_b<-ave(df$rae_b,df$market,FUN=function(x) mean(x, na.rm=T))
df$mrae_c<-ave(df$rae_c,df$market,FUN=function(x) mean(x, na.rm=T))
df$mrae_cnav<-ave(df$rae_cnav,df$market,FUN=function(x) mean(x, na.rm=T))

df$mdp_ba<-ave(df$dp_ba,df$market,FUN=function(x) mean(x, na.rm=T))
df$mdp_ca<-ave(df$dp_ca,df$market,FUN=function(x) mean(x, na.rm=T))
df$mdp_cb<-ave(df$dp_cb,df$market,FUN=function(x) mean(x, na.rm=T))

df$mdp_baj<-ave(df$dpj_ba,df$market,FUN=function(x) mean(x, na.rm=T))
df$mdp_caj<-ave(df$dpj_ca,df$market,FUN=function(x) mean(x, na.rm=T))
df$mdp_cbj<-ave(df$dpj_cb,df$market,FUN=function(x) mean(x, na.rm=T))

market_ind<-unique(df[,c("market","mrae_a","mrae_b","mrae_c","mdp_ba", "mdp_ca", "mdp_cb","mdp_baj","mdp_caj","mdp_cbj","mrae_cnav")])


unique(ave(df$group.q_a,df$market,FUN=function(x) sum(x, na.rm=T)))/9

png("prices32.png")
plot(df$subsession.round_number,df$group.p_c,ylim=c(0,80),col="blue",xaxt = "n",xlab="period",ylab = "prices")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number,df$group.nav,col="black")
points(df$subsession.round_number,df$group.p_a,col="gray",pch=2)
points(df$subsession.round_number,df$group.p_b,col="red",pch=3)
lines(df$subsession.round_number,df$fva,col="gray")
lines(df$subsession.round_number,df$fvb,col="red")
lines(df$subsession.round_number,df$fvc,col="blue")
legend(25, 70, c("NAV","C","B","A","FV A","FV B", "FV C"), col = c("black","blue","red","gray","gray","red","blue"), pch = c(NA,1,3,2,NA,NA,NA),lty = c(1,NA,NA,NA,1,1,1),bty = "n")
dev.off()

png("marketq1.png")
plot(df$subsession.round_number,df$group.q_c,ylim=c(0,10),col="blue",type="l",xaxt = "n",xlab="period",ylab = "q")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number,df$group.q_a,col="gray")
lines(df$subsession.round_number,df$group.q_b,col="red")
rect(15, 0,   18, 10, col = "white", border = "white", lwd = 2)
dev.off()

png("dport1.png")
plot(df$subsession.round_number[df$type==2],df$portype[df$type==2],ylim=c(0,80),type="l",col="black",lty=1,xaxt = "n",xlab="period",ylab = "(a-b)^2")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number[df$type==1],df$portype[df$type==1],ylim=c(0,80),type="l",col="black",lty=2)
lines(df$subsession.round_number[df$type==0],df$portype[df$type==0],ylim=c(0,80),type="l",col="black",lty=3)
dev.off()



png("difport1.png")
plot(df$subsession.round_number[df$type==2],df$portdiftype[df$type==2],ylim=c(-20,20),type="l",col="black",lty=1,xaxt = "n",xlab="period",ylab = "|a-b|")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number[df$type==1],df$portdiftype[df$type==1],col="black",lty=2)
lines(df$subsession.round_number[df$type==0],df$portdiftype[df$type==0],col="black",lty=3)
dev.off()

png("holdings_gc1.png")
plot(df$subsession.round_number[df$type==2],df$agroup[df$type==2],ylim=c(0,40),type="l",col="gray",lty=1,xaxt = "n",xlab="period",ylab = "holdings type c")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number[df$type==2],df$bgroup[df$type==2],col="red")
lines(df$subsession.round_number[df$type==2],df$cgroup[df$type==2],col="blue")
dev.off()

png("holdings_gb1.png")
plot(df$subsession.round_number[df$type==2],df$agroup[df$type==1],ylim=c(0,40),type="l",col="gray",lty=1,xaxt = "n",xlab="period",ylab = "holdings type b")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number[df$type==2],df$bgroup[df$type==1],col="red")
lines(df$subsession.round_number[df$type==2],df$cgroup[df$type==1],col="blue")
dev.off()

png("holdings_ga1.png")
plot(df$subsession.round_number[df$type==2],df$agroup[df$type==0],ylim=c(0,40),type="l",col="gray",lty=1,xaxt = "n",xlab="period",ylab = "holdings type a")
axis(1, at=c(1:15,18:32), labels=c(1:15,1:15))
lines(df$subsession.round_number[df$type==2],df$bgroup[df$type==0],col="red")
lines(df$subsession.round_number[df$type==2],df$cgroup[df$type==0],col="blue")
dev.off()


#plot(df$subsession.round_number[df$type==1],df$cgroup[df$type==1],ylim=c(0,40),type="l",col="black",lty=2)
