rm(list = ls())
setwd("~/Desktop/jotarepos/etfs/data/")
## Call the data. 
load("etfalldata.Rda")
library("plm")
library("xtable")

#write.csv(df,file="etfalldata.csv")
#work only with second market
df<-df[df$subsession.round_number>15,]

dp<-pdata.frame(df, index=c("uses","subsession.ronda"), drop.index=F, row.names=TRUE)

#dp$all.bids<-1
#dp$all.bids[dp$player.bida==0]<-0
#dp$all.bids[dp$player.bidb==0]<-0
#dp$all.bids[dp$player.bidc==0]<-0

dp$all.bids<-1
dp$all.bids[dp$player.aska==0]<-0
dp$all.bids[dp$player.askb==0]<-0
dp$all.bids[dp$player.askc==0]<-0

dp$all.bids.player<-ave(dp$all.bids,dp$uses,FUN = max)

arb.bids.q<-function(ba,bb,bc,id){
  ba[ba>0]<-1
  bb[bb>0]<-1
  bc[bc>0]<-1
  trae<-ba+bb+bc
  trae[trae<3]<-0
  trae[trae==3]<-1
  nrae<-tapply(trae,id,sum,na.rm=T)
  return(nrae)
}

arb.bids<-function(ba,bb,bc,id){
  ba[ba==0]<-NA
  bb[bb==0]<-NA
  bc[bc==0]<-NA
  ayb<-ba+bb
  #trae<-abs(ayb/bc-1)
  trae<-abs(ayb-bc)
  nrae<-tapply(trae,id,mean,na.rm=T)
  return(nrae)
}

dhold<-function(qa,qb,qc,id){
  den<-(qa+qb+2*qc)
  den[den==0]<-1
  trae<-abs(qa-qb)/den
  #trae<-apply(cbind(qa,qb),1,min)
  #trae<-trae+qc
  nrae<-tapply(trae,id,mean,na.rm=T)
  return(nrae)
}

dfinal<-df[df$subsession.round_number==30,]
dfinal$hold.a<-dfinal$player.n_a+dfinal$player.t_a
dfinal$hold.b<-dfinal$player.n_b+dfinal$player.t_b
dfinal$hold.c<-dfinal$player.n_c+dfinal$player.t_c
balance.port<-dhold(dfinal$hold.a,dfinal$hold.b,dfinal$hold.c,dfinal$session)

arbitrage_abc<-arb.bids(dp$player.bida,dp$player.bidb,dp$player.bidc,dp$session)
arbitrage_abc.asks<-arb.bids(dp$player.aska,dp$player.askb,dp$player.askc,dp$session)
arbitrage_abc.q.asks<-arb.bids.q(dp$player.aska,dp$player.askb,dp$player.askc,dp$session)
arbitrage_abc.q<-arb.bids.q(dp$player.bida,dp$player.bidb,dp$player.bidc,dp$session)
arbitrage_abc.asks[is.na(arbitrage_abc.asks)]<-0

total.arbitrage<-(arbitrage_abc*arbitrage_abc.q+arbitrage_abc.q.asks*arbitrage_abc.asks)/(sum(arbitrage_abc.q.asks,na.rm=T)+sum(arbitrage_abc.q,na.rm=T))
jota<-(arbitrage_abc*arbitrage_abc.q+arbitrage_abc.q.asks*arbitrage_abc.asks)/c(rep(NA,10),50,26,28,21,9,13,46,16,80,64)

library("coin")
dtest<-data.frame(NA)
dtest<-cbind(dtest,arbitrage_abc[c(11:20)])
dtest[,1]<-factor(c(rep("dos",5),rep("tres",5)))
names(dtest)<-c("tre","rap")

oneway_test(rap~tre,data=dtest,distribution="exact")
wilcox_test(rap~tre,data=dtest,distribution="exact")
ks.test(dtest$rap[dtest$tre=="dos"],dtest$rap[dtest$tre=="tres"])

#2Z vs 2N 
dtest<-data.frame(NA)
dtest<-cbind(dtest,balance.port[c(1:5,6:10)])
dtest[,1]<-factor(c(rep("dos",5),rep("tres",5)))
names(dtest)<-c("tre","rap")

oneway_test(rap~tre,data=dtest,distribution="exact")
wilcox_test(rap~tre,data=dtest,distribution="exact")
ks.test(dtest$rap[dtest$tre=="dos"],dtest$rap[dtest$tre=="tres"])


balance.final<-cbind(balance.port[1:5],balance.port[11:15],balance.port[6:10],balance.port[16:20])
balance.final<-rbind(balance.final,apply(balance.final,2,mean))
xtable(balance.final)



dp$type.funda.a<-0
dp$type.funda.a.count<-0

dp$type.mom.a<-0
dp$type.mom.a.count<-0

dp$type.rsp.a<-0
dp$type.rsp.a.count<-0


types.charles<-function(p,fv,nbid,nask){
  dp$type.funda.a[(p-fv)*(nbid-nask)<0]<-1
  dp$type.funda.a[is.na(p)]<-NA
  dp$type.funda.ap<-ave(dp$type.funda.a,dp$uses,FUN=function(x) mean(x, na.rm=T))

  deltap.a.lag<-lag(p)-lag(p,2)
  dp$type.mom.a[(deltap.a.lag)*(nbid-nask)>0]<-1
  dp$type.mom.a[is.na(deltap.a.lag)]<-NA
  dp$type.mom.ap<-ave(dp$type.mom.a,dp$uses,FUN=function(x) mean(x, na.rm=T))

  deltap.a.lead<-lead(p)-p
  dp$type.rsp.a[(deltap.a.lead)*(nbid-nask)>0]<-1
  dp$type.rsp.a[is.na(deltap.a.lead)]<-NA
  dp$type.rsp.ap<-ave(dp$type.rsp.a,dp$uses,FUN=function(x) mean(x, na.rm=T))

  # 1 fv
  # 2 mom
  # 3 speculators
  # 4 1&2
  # 5 1&3
  # 6 2&3
  #7 1&2&3
  maxtype<-apply(cbind(dp$type.rsp.ap,dp$type.mom.ap,dp$type.funda.ap),1,max)
  
  final.type.a<-rep(7,dim(dp)[1])
  final.type.a[dp$type.funda.ap==maxtype]<-1
  final.type.a[dp$type.mom.ap==maxtype]<-2
  final.type.a[dp$type.rsp.ap==maxtype]<-3
  
  final.type.a[dp$type.funda.ap==maxtype & dp$type.mom.ap==maxtype]<-4
  final.type.a[dp$type.funda.ap==maxtype & dp$type.rsp.ap==maxtype]<-5
  final.type.a[dp$type.mom.ap==maxtype & dp$type.rsp.ap==maxtype]<-6
  
  #table.type.a<-table(final.type.a,dp$tre)
  #table.type.a[1,]<-table.type.a[1,]+table.type.a[4,]+table.type.a[5,]
  #table.type.a[2,]<-table.type.a[2,]+table.type.a[4,]+table.type.a[6,]
  #table.type.a[3,]<-table.type.a[3,]+table.type.a[5,]+table.type.a[6,]
  
  #table.type<-table.type.a[c(1:3),]
  return(final.type.a)  
  #return(table.type.a)  
}

dp$type_a<-types.charles(dp$group.p_a,dp$fva,dp$player.nbida,dp$player.naska)
dp$type_b<-types.charles(dp$group.p_b,dp$fvb,dp$player.nbidb,dp$player.naskb)
dp$type_c<-types.charles(dp$group.p_c,dp$fvc,dp$player.nbidc,dp$player.naskc)

dp$type_final<-dp$type_a*100+dp$type_b*10+dp$type_c
table(dp$type_final,dp$tre)

table.charles.a<-types.charles(dp$group.p_a,dp$fva,dp$player.nbida,dp$player.naska)
table.charles.a<-rbind(table.charles.a,rep(0,4))
row.names(table.charles.a)[6]<-5
table.charles.a<-table.charles.a[c(1:4,6,5),]

table.charles.b<-types.charles(dp$group.p_b,dp$fvb,dp$player.nbidb,dp$player.naskb)
table.charles.b<-rbind(table.charles.b,rep(0,4))
row.names(table.charles.b)[6]<-4
table.charles.b<-table.charles.b[c(1:3,6,4:5),]

table.charles.c<-types.charles(dp$group.p_c,dp$fvc,dp$player.nbidc,dp$player.naskc)
table.charles.c[6,]<-0
row.names(table.charles.c)[6]<-4
table.charles.c<-table.charles.c[c(1:3,6,4:5),c(4,2)]

table.charles.report<-cbind(table.charles.a[,c(3,4,1,2)],table.charles.b[,c(3,4,1,2)],table.charles.c)/(5*15*9)
table.charles.report<-round(table.charles.report,2)
xtable(table.charles.report)

# you need to check whether all types appear
table.charles.a[1,]<-table.charles.a[1,]+table.charles.a[4,]*.5
table.charles.a[2,]<-table.charles.a[2,]+table.charles.a[4,]*.5+table.charles.a[5,]*.5
table.charles.a[3,]<-table.charles.a[3,]+table.charles.a[5,]*.5
table.charles.a<-table.charles.a[c(1:3),]
totals<-apply(table.charles.a,2,sum)
table.charles.a[1,]<-table.charles.a[1,]/totals
table.charles.a[2,]<-table.charles.a[2,]/totals
table.charles.a[3,]<-table.charles.a[3,]/totals

# you need to check whether all types appear
table.charles.b[1,]<-table.charles.b[1,]+table.charles.b[4,]
table.charles.b[2,]<-table.charles.b[2,]+table.charles.b[5,]
table.charles.b[3,]<-table.charles.b[3,]+table.charles.b[4,]+table.charles.b[5,]
table.charles.b<-table.charles.b[c(1:3),]
totals<-apply(table.charles.b,2,sum)
table.charles.b[1,]<-table.charles.b[1,]/totals
table.charles.b[2,]<-table.charles.b[2,]/totals
table.charles.b[3,]<-table.charles.b[3,]/totals

# you need to check whether all types appear
table.charles.c[1,]<-table.charles.c[1,]
table.charles.c[2,]<-table.charles.c[2,]+table.charles.c[4,]+table.charles.c[5,]
table.charles.c[3,]<-table.charles.c[3,]+table.charles.c[4,]+table.charles.c[5,]
table.charles.c<-table.charles.c[c(1:3),]
totals<-apply(table.charles.c,2,sum)
table.charles.c[1,]<-table.charles.c[1,]/totals
table.charles.c[2,]<-table.charles.c[2,]/totals
table.charles.c[3,]<-table.charles.c[3,]/totals

## Indivial profit per treatment
ganancias_0<-dp$player.cum_payoff[dp$subsession.round_number==30 & dp$tre==0]
mean(ganancias_0)


####CLASSIFY PLAYERS

types.charles<-function(p,fv,nbid,nask){
  dp$type.funda.a[(p-fv)*(nbid-nask)<0]<-1
  dp$type.funda.a[is.na(p)]<-NA
  dp$type.funda.ap<-ave(dp$type.funda.a,dp$uses,FUN=function(x) mean(x, na.rm=T))
  
  deltap.a.lag<-lag(p)-lag(p,2)
  dp$type.mom.a[(deltap.a.lag)*(nbid-nask)>0]<-1
  dp$type.mom.a[is.na(deltap.a.lag)]<-NA
  dp$type.mom.ap<-ave(dp$type.mom.a,dp$uses,FUN=function(x) mean(x, na.rm=T))
  
  deltap.a.lead<-lead(p)-p
  dp$type.rsp.a[(deltap.a.lead)*(nbid-nask)>0]<-1
  dp$type.rsp.a[is.na(deltap.a.lead)]<-NA
  dp$type.rsp.ap<-ave(dp$type.rsp.a,dp$uses,FUN=function(x) mean(x, na.rm=T))
  
  # 1 fv
  # 2 mom
  # 3 speculators
  # 4 1&2
  # 5 1&3
  # 6 2&3
  #7 1&2&3
  maxtype<-apply(cbind(dp$type.rsp.ap,dp$type.mom.ap,dp$type.funda.ap),1,max)
  
  final.type.a<-rep(7,dim(dp)[1])
  final.type.a[dp$type.funda.ap==maxtype]<-1
  final.type.a[dp$type.mom.ap==maxtype]<-2
  final.type.a[dp$type.rsp.ap==maxtype]<-3
  
  final.type.a[dp$type.funda.ap==maxtype & dp$type.mom.ap==maxtype]<-4
  final.type.a[dp$type.funda.ap==maxtype & dp$type.rsp.ap==maxtype]<-5
  final.type.a[dp$type.mom.ap==maxtype & dp$type.rsp.ap==maxtype]<-6
  
  #table.type.a<-table(final.type.a,dp$tre)
  #table.type.a[1,]<-table.type.a[1,]+table.type.a[4,]+table.type.a[5,]
  #table.type.a[2,]<-table.type.a[2,]+table.type.a[4,]+table.type.a[6,]
  #table.type.a[3,]<-table.type.a[3,]+table.type.a[5,]+table.type.a[6,]
  
  #table.type<-table.type.a[c(1:3),]
  return(final.type.a)  
  #return(table.type.a)  
}

dp$pnav<-0
dp$deltap.p<- dp$group.p_c-lag(dp$group.p_c,1)
dp$delta.nav<- dp$group.nav-lag(dp$group.nav)
dp$pnav[(dp$deltap.p)*(dp$delta.nav)<0]<-1
dp$pnav[(dp$deltap.p)*(dp$delta.nav)>0]<-2
dp$pnav[is.na(dp$deltap.p)]<-NA

pnav.measures<-function(p,nav){
  deltap.p<- p-lag(p,1)
  delta.nav<- nav-lag(nav,1)
  dp$pnav[(deltap.p)*(delta.nav)<0]<-1
  dp$pnav[is.na(deltap.p)]<-NA
}

pnav.measures(dp$group.p_c,dp$group.nav)

#dp$arb.bidnav<-dp$player.askc-(dp$player.bida+dp$player.bidb)
dp$arb.bidnav<-(dp$player.aska+dp$player.askb)-dp$player.askc
dp$arb.bidnav[dp$player.aska==0]<-NA
dp$arb.bidnav[dp$player.askb==0]<-NA
dp$arb.bidnav[dp$player.askc==0]<-NA

tapply(dp$arb.bidnav!=0,dp$,sum,na.rm=T)
tapply(dp$arb.bidnav!=0,dp$session*10+dp$participant.id_in_session,sum,na.rm=T)








