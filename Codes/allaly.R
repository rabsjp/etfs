rm(list = ls())
#setwd("/cloud/project/data/")
setwd("~/Desktop/jotarepos/etfs/data/")
## Call the data. unique id per session. 
load("etfalldata.Rda")

df$portdifab<-(df$player.n_a+df$player.t_a-df$player.n_b-df$player.t_b)
df$finala<-NA
df$finala[df$subsession.ronda==15]<-df$player.n_a[df$subsession.ronda==15]+df$player.t_a[df$subsession.ronda==15]
df$finalb<-NA
df$finalb[df$subsession.ronda==15]<-df$player.n_b[df$subsession.ronda==15]+df$player.t_b[df$subsession.ronda==15]

listofsessions<-c(31011,31021)
imp_sessions <- listofsessions[1]



##Perhaps, one should should for active according to the bid/asks submitted
attach(df)
tapply(player.nbida+player.nbidb+player.naska+player.naskb,uses,function(x) sum(x>0, na.rm=T))
detach(df)



dplot<-df[df$uses==imp_sessions,]



plot(df$finala[df$tre==1 & df$subsession.round_number==30],df$finalb[df$tre==1& df$subsession.round_number==30],ylim=c(0,15),xlim=c(0,15))
abline(0,1)

plot(df$finala[df$tre==0& df$subsession.round_number==30],df$finalb[df$tre==0& df$subsession.round_number==30],ylim=c(0,30),xlim=c(0,30))
abline(0,1)
