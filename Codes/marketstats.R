rm(list = ls())
#setwd("/cloud/project/data/")
setwd("~/Desktop/jotarepos/etfs/data/")
## Call the data. unique id per session. 
load("etfmarket.Rda")
#write.csv(df,file="etfmarketdata.csv")
df<-df[df$subsession.round_number>15,]
#dsum<- aggregate(df,by = list(df$idsu),FUN=mean)
df$missing_a<-is.na(df$group.p_a)
## Price amplitud (relative to FV)
tapply(df$missing_a,df$session,sum)#/f[1]
pa<-function(p,f,id){
  maxpa<-ave(p-f,id,FUN=function(x) max(x, na.rm=T))
  minpa<-ave(p-f,id,FUN=function(x) min(x, na.rm=T))
  npa<-(maxpa-minpa)
  npa<-tapply(npa,id,mean)#/f[1]
  return(npa)
}

## RAE (relative absolute deviation) function
rae<-function(p,f,id){
  trae<-abs(p/f-1)
  #nrae<-unique(ave(trae,id,FUN=function(x) mean(x, na.rm=T)))
  nrae<-tapply(trae,id,mean,na.rm=T)
  return(nrae)
}

rae.nav<-function(p,f,nav,id){
  #trae<-abs(p-nav)/f
  trae<-(p-nav)/f
  #nrae<-unique(ave(trae,id,FUN=function(x) mean(x, na.rm=T)))
  nrae<-tapply(trae,id,mean,na.rm=T)
  return(nrae)
}

gad<-function(p,f,id){
  trae<-abs(log(p/f))
  #nrae<-unique(ave(trae,id,FUN=function(x) mean(x, na.rm=T)))
  nrae<-tapply(trae,id,mean,na.rm=T)
  nrae<-exp(nrae)-1
  return(nrae)
}

rd<-function(p,f,id){
  trd<-(p/f-1)
  nrd<-tapply(trd,id,mean,na.rm=T)
  return(nrd)
}

pd<-function(p,id){
  trd<-(p)
  nrd<-tapply(trd,id,mean,na.rm=T)
  return(nrd)
}


## RAE weighted fuction (according to market transactions)
wrae<-function(p,f,q,id){
  trae<-abs(p/f-1)*q
  qse<- ave(q,id,FUN=function(x) sum(x, na.rm=T))
  #wnrae<-ave(trae/qse,id,FUN=function(x) sum(x, na.rm=T))
  wnrae<-tapply(trae/qse,id,sum,na.rm=T)
  return(wnrae)
}

##LEVEL OF RAD
trae<-function(p,f,q,id){
  trae<-abs(p-f)*q
  ndpj<-tapply(q,id,sum,na.rm=T)
  qse<- ave(q,id,FUN=function(x) sum(x, na.rm=T))
  #wnrae<-ave(trae/qse,id,FUN=function(x) sum(x, na.rm=T))
  wnrae<-tapply(trae,id,sum,na.rm=T)/ndpj
  return(wnrae)
}



## DP-> deviation of parity
dpj<-function(p1,f1,p2,f2,id){
  trae<-abs((p1/f1)/(p2/f2)-1)
  #ndpj<-ave(trae,id,FUN=function(x) mean(x, na.rm=T))
  ndpj<-tapply(trae,id,mean,na.rm=T)
  return(ndpj)
}


dpjs<-function(p1,f1,p2,f2,id){
  trae<-(p1/f1)/(p2/f2)-1
  #ndpj<-ave(trae,id,FUN=function(x) mean(x, na.rm=T))
  ndpj<-tapply(trae,id,mean,na.rm=T)
  return(ndpj)
}


dpt<-function(p1,f1,p2,f2,id){
  trae<-abs((p1+p2)/(f1+f2)-1)
  #ndpj<-ave(trae,id,FUN=function(x) mean(x, na.rm=T))
  ndpj<-tapply(trae,id,mean,na.rm=T)
  return(ndpj)
}

#Total RAD weighted per asset
dptw<-function(p1,f1,q1,p2,f2,q2,id){
  trae1<-abs(p1/f1-1)*q1
  trae2<-abs(p2/f2-1)*q2
  ndpj<-tapply(q1+q2,id,sum,na.rm=T)
  temp1<-apply(cbind(trae1,trae2),1,sum,na.rm=T)
  final<-tapply(temp1,id,sum,na.rm=T)/ndpj
  return(final)
}

dptw3<-function(p1,f1,q1,p2,f2,q2,p3,f3,q3,id){
  trae1<-abs(p1/f1-1)*q1
  trae2<-abs(p2/f2-1)*q2
  trae3<-abs(p3/f3-1)*q3
  ndpj<-tapply(q1+q2+q3,id,sum,na.rm=T)
  temp1<-apply(cbind(trae1,trae2,trae3),1,sum,na.rm=T)
  final<-tapply(temp1,id,sum,na.rm=T)/ndpj
  return(final)
}


## Standard deviation
disperse<-function(p1,id){
  disp<-tapply(p1,id,sd,na.rm=TRUE)
  return(disp)
}

dfcor<-df[,c("session","tre","group.p_c","group.nav","fvc","subsession.round_number")]
dfcor<-dfcor[!is.na(dfcor$group.p_c),]
mueva<-by(dfcor,dfcor$session,FUN= function(X) cor.test(X$group.p_c,X$fvc,method="spearman",use="pairwise.complete.obs"))

mueve<-by(df,df$session,FUN= function(X) cor.test(X$group.p_c,X$group.nav,method="spearman",use="pairwise.complete.obs"))



## asset turnover
turnover<-function(q1,id){
  tov<-tapply(q1,id,sum,na.rm=TRUE)
  return(tov)
}

turnoverba<-function(q1,id){
  tov<-tapply(q1,id,mean,na.rm=TRUE)
  return(tov)
}

rae(df$group.p_c,df$group.nav,df$uses)
rae(df$group.p_c,df$group.nav,df$uses)
## MEANS RD
#rae_a<-rae(df$group.p_a,df$fva,df$uses)/mean(df$fva)
#rae_b<-rae(df$group.p_b,df$fvb,df$uses)/mean(df$fvb)
#rae_c<-rae(df$group.p_c,df$fvc,df$uses)/mean(df$fvc)
#rae_cnav<-rae(df$group.p_c,df$group.nav,df$uses)
#rae_cnav<-rae_cnav/tapply(df$group.nav,df$uses,mean,na.rm=TRUE)

rae_a<-rae(df$group.p_a,df$fva,df$session)
rae_b<-rae(df$group.p_b,df$fvb,df$session)
rae_c<-rae(df$group.p_c,df$fvc,df$session)
rae_cnav<-rae(df$group.p_c,df$group.nav,df$session)
rae.nav_cnav<-rae.nav(df$group.p_c,df$fvc,df$group.nav,df$session)
rd_cnav<-rd(df$group.p_c,df$group.nav,df$session)

gad_a<-gad(df$group.p_a,df$fva,df$session)
gad_b<-gad(df$group.p_b,df$fvb,df$session)
gad_c<-gad(df$group.p_c,df$fvc,df$session)
gad_cnav<-gad(df$group.p_c,df$group.nav,df$session)

pa_a<-pd(df$group.p_a,df$session)
pa_b<-pd(df$group.p_b,df$session)
pa_c<-pd(df$group.p_c,df$session)
pa_cnav<-pd(df$group.p_c,df$group.nav,df$session)/df$fvc[1]

pd_c<-pd(df$group.p_c,df$session)

pas<-rbind(pa_a,pa_b,pa_c,pa_cnav)


rd_a<-rd(df$group.p_a,df$fva,df$session)
rd_b<-rd(df$group.p_b,df$fvb,df$session)
rd_c<-rd(df$group.p_c,df$fvc,df$session)
rd_cnav<-rd(df$group.p_c,df$group.nav,df$session)

rdp<-dpjs(df$group.p_b,df$fvb,df$group.p_a,df$fva,df$tre)

rds<-rbind(rd_a,rd_b,rd_c,rd_cnav)

wrae_a<-wrae(df$group.p_a,df$fva,df$group.q_a,df$uses)
wrae_b<-wrae(df$group.p_b,df$fvb,df$group.q_b,df$session)
wrae_c<-wrae(df$group.p_c,df$fvc,df$group.q_c,df$session)
wrae_cnav<-wrae(df$group.p_c,df$group.nav,df$group.q_c,df$session)

wtotal<-dptw(df$group.p_a,df$fva,df$group.q_a,df$group.p_b,df$fvb,df$group.q_b,df$session)
wtotal3<-dptw3(df$group.p_a,df$fva,df$group.q_a,df$group.p_b,df$fvb,df$group.q_b,df$group.p_c,df$fvc,df$group.q_c,df$session)

#rap<-dpj(df$group.p_b,mean(df$fvb),df$group.p_a,mean(df$fva),df$uses)
rap<-dpj(df$group.p_b,df$fvb,df$group.p_a,df$fva,df$tre)

boxes<-tapply(df$player.boxes_collected,df$uses,mean,na.rm=T)

rapt<-dpt(df$group.p_b,df$fvb,df$group.p_a,df$fva,df$session)


raes<-rbind(rae_a,rae_b,rae_c,rae_cnav,rap)
wraes<-rbind(wrae_a,wrae_b,rae_c,wrae_cnav)

balance.port<-tapply(df$portab,df$uses,mean)
balance.port.a<-tapply(df$portype.a,df$uses,mean)
balance.port.b<-tapply(df$portype.b,df$uses,mean)
balance.port.c<-tapply(df$portype.c,df$uses,mean)

nse2<-10
nses<-10
totala<- c(rep(20*3,nse2),rep(10*3,nses))
totalb<- c(rep(20*3,nse2),rep(10*3,nses))
totalc<- c(rep(20*3,nse2),rep(10*3,nses))

turnover_a<-turnover(df$group.q_a,df$session)/totala*100
turnover_b<-turnover(df$group.q_b,df$session)/totalb*100
turnover_c<-turnover(df$group.q_c,df$session)/totalc*100

mean(turnover_b[1:5])
mean(turnover_b[6:10])
mean(turnover_c[11:15])
mean(turnover_c[16:20])


turnover_a<-turnover(df$group.q_a,df$session)
turnover_b<-turnover(df$group.q_b,df$session)
turnover_c<-turnover(df$group.q_c,df$session)

trae_a<-trae(df$group.p_a,df$fva,df$group.q_a,df$session)
trae_b<-trae(df$group.p_b,df$fvb,df$group.q_b,df$session)
trae_c<-trae(df$group.p_c,df$fvc,df$group.q_c,df$session)
trae_cnav<-trae(df$group.p_c,df$group.nav,df$group.q_c,df$session)

trae_all<-(trae_a+trae_b+trae_c)#/(turnover_a+turnover_b+turnover_c)

turnoverba_a<-turnoverba(df$active_a,df$uses)/totala*100
turnoverba_b<-turnoverba(df$active_b,df$uses)/totalb*100
turnoverba_c<-turnoverba(df$active_c,df$uses)/totalc*100

vueltas<-rbind(turnover_a,turnover_b,turnover_c)
vueltasba<-rbind(turnoverba_a,turnoverba_b,turnoverba_c)

vol_a<-disperse(df$group.p_a,df$session)
vol_b<-disperse(df$group.p_b,df$session)
vol_c<-disperse(df$group.p_c,df$session)

## Test CNAV and C
library("coin")
dtest<-data.frame(NA)
#dtest<-cbind(dtest,c(rae_c[16:20],rae_cnav[16:20]))
dtest<-cbind(dtest,c(pa_c[11:15],pa_c[16:20]))
dtest[,1]<-factor(c(rep("dos",5),rep("tres",5)))
names(dtest)<-c("tre","rap")
#atest<-rae_c[16:20]-rae_cnav[16:20]
#atest<-rae_c[11:15]-rae_cnav[11:15]
#atest<-rae_cnav[11:15]-rae_cnav[16:20]
wilcox.test(rd_cnav[11:15], mu = 0, alternative = "two.sided")
#atest<-trae_cnav[11:15]-trae_c[11:15]
#wilcox.test(atest, mu = 0, alternative = "two.sided")
#wilcox.test(rae.nav_cnav[16:20],rae.nav_cnav[16:20], paired = TRUE, alternative = "two.sided")

oneway_test(rap~tre,data=dtest,distribution="exact")
wilcox_test(rap~tre,data=dtest,distribution="exact")
ks.test(dtest$rap[dtest$tre=="dos"],dtest$rap[dtest$tre=="tres"])

volatilities<-rbind(vol_a,vol_b,vol_c)

#rap 3N vs 2N
library("coin")
dtest<-data.frame(NA)
#dtest<-cbind(dtest,turnover_b[c(12,14,16,18,20,32,34,36,38,40)])
#dtest[,1]<-factor(c(rep("dos",5),rep("tres",5)))
dtest<-cbind(dtest,rae_b[c(6:10,16:20)])
#dtest<-cbind(dtest,c(wtotal[c(1:10)]))
dtest[,1]<-factor(c(rep("dos",5),rep("tres",5)))
names(dtest)<-c("tre","rap")

oneway_test(rap~tre,data=dtest,distribution="exact")
wilcox_test(rap~tre,data=dtest,distribution="exact")
ks.test(dtest$rap[dtest$tre=="dos"],dtest$rap[dtest$tre=="tres"])

#oneway_test(rap~tre,data=dtest,distribution="exact",alternative = "greater")
#wilcox_test(rap~tre,data=dtest,distribution="exact",alternative = "greater")
#ks.test(dtest$rap[dtest$tre=="dos"],dtest$rap[dtest$tre=="tres"],alternative = "less")

#rap 2Z vs 2N
library("coin")
dtest<-data.frame(NA)
dtest<-cbind(dtest,rapt[c(12,14,16,18,20,2,4,6,8,10)])
dtest[,1]<-factor(c(rep("dos",5),rep("tres",5)))
names(dtest)<-c("tre","rap")
  
oneway_test(rap~tre,data=dtest,distribution="exact")
wilcox_test(rap~tre,data=dtest,distribution="exact")
ks.test(dtest$rap[dtest$tre=="dos"],dtest$rap[dtest$tre=="tres"])


oneway_test(rap~tre,data=dtest,distribution="exact",alternative = "greater")
wilcox_test(rap~tre,data=dtest,distribution="exact",alternative = "greater")
ks.test(dtest$rap[dtest$tre=="dos"],dtest$rap[dtest$tre=="tres"],alternative = "less")

#rap 3N vs 3Z
library("coin")
dtest<-data.frame(NA)
dtest<-cbind(dtest,rd_c[c(32,34,36,38,40,22,24,26,28,30)])
dtest[,1]<-factor(c(rep("dos",5),rep("tres",5)))
names(dtest)<-c("tre","rap")

#works<-c(22,24,26,28,30)
#10,12,14,16,18,
#cor(rae_c[works],boxes[works])
#summary(lm(rae_b[c(seq(2,38,2))] ~ boxes[c(seq(2,38,2))]))

oneway_test(rap~tre,data=dtest,distribution="exact")
wilcox_test(rap~tre,data=dtest,distribution="exact")
ks.test(dtest$rap[dtest$tre=="dos"],dtest$rap[dtest$tre=="tres"])

oneway_test(rap~tre,data=dtest,distribution="exact",alternative = "greater")
wilcox_test(rap~tre,data=dtest,distribution="exact",alternative = "greater")
ks.test(dtest$rap[dtest$tre=="dos"],dtest$rap[dtest$tre=="tres"],alternative = "less")


#rap 2Z vs 3Z
dtest<-data.frame(NA)
dtest<-cbind(dtest,gad_b[c(1:5,11:15)])
dtest[,1]<-factor(c(rep("dos",5),rep("tres",5)))
names(dtest)<-c("tre","rap")

oneway_test(rap~tre,data=dtest,distribution="exact")
wilcox_test(rap~tre,data=dtest,distribution="exact")
ks.test(dtest$rap[dtest$tre=="dos"],dtest$rap[dtest$tre=="tres"])

oneway_test(rap~tre,data=dtest,distribution="exact",alternative = "greater")
wilcox_test(rap~tre,data=dtest,distribution="exact",alternative = "greater")
ks.test(dtest$rap[dtest$tre=="dos"],dtest$rap[dtest$tre=="tres"],alternative = "less")

#rap 2Z vs 3N
dtest<-data.frame(NA)
dtest<-cbind(dtest,rap[c(2,4,6,8,10,32,34,36,38,40)])
dtest[,1]<-factor(c(rep("dos",5),rep("tres",5)))
names(dtest)<-c("tre","rap")

oneway_test(rap~tre,data=dtest,distribution="exact")
wilcox_test(rap~tre,data=dtest,distribution="exact")
ks.test(dtest$rap[dtest$tre=="dos"],dtest$rap[dtest$tre=="tres"])

#rap 3Z vs 2N
dtest<-data.frame(NA)
dtest<-cbind(dtest,pa_b[c(10,12,14,16,18,20,22,24,26,28)])
dtest[,1]<-factor(c(rep("dos",5),rep("tres",5)))
names(dtest)<-c("tre","rap")

oneway_test(rap~tre,data=dtest,distribution="exact")
wilcox_test(rap~tre,data=dtest,distribution="exact")
ks.test(dtest$rap[dtest$tre=="dos"],dtest$rap[dtest$tre=="tres"])

#dtest<-cbind(dtest,turnoverba_b[c(2,4,6,8,18,20,22,24)])
#
#dtest<-cbind(dtest,rd_cnav[c(18,20,22,24,26,28,30,32,34)])




oneway_test(rap~tre,data=dtest,distribution=approximate(nresample=9999),alternative = "greater")


cor(df$group.p_a[df$uses==2010],df$group.p_b[df$uses==2010],use="complete.obs")
cor(df$group.p_a[df$uses==2011],df$group.p_b[df$uses==2011],use="complete.obs")
cor(df$group.p_a[df$uses==2020],df$group.p_b[df$uses==2020],use="complete.obs")
cor(df$group.p_a[df$uses==2021],df$group.p_b[df$uses==2021],use="complete.obs")

cor(df$group.p_a[df$uses==3010],df$group.p_b[df$uses==3010],use="complete.obs")
cor(df$group.p_a[df$uses==3011],df$group.p_b[df$uses==3011],use="complete.obs")
cor(df$group.p_a[df$uses==3020],df$group.p_b[df$uses==3020],use="complete.obs")
cor(df$group.p_a[df$uses==3021],df$group.p_b[df$uses==3021],use="complete.obs")

div_a<-NULL
div_b<-NULL

i<-1
while(i<16){
  div_a<-c(div_a,sample(c(-1,1),1))
  div_b<-c(div_b,sample(c(-1,1),1))
  i<-i+1
}

pa<-rep(10,15)+div_a
pb<-c(rep(28,8),c(27:21))+div_b
cor(pa,pb)


