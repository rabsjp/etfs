rm(list = ls())
#setwd("/cloud/project/data/")
setwd("~/Desktop/jotarepos/etfs/data/")
## Call the data. unique id per session. 
load("etfmarket.Rda")

## Price amplitud (relative to FV)

pa<-function(p,f,id){
  maxpa<-ave(p/f-1,id,FUN=function(x) max(x, na.rm=T))
  minpa<-ave(p/f-1,id,FUN=function(x) min(x, na.rm=T))
  npa<-maxpa-minpa
  npa<-tapply(npa,df$uses,mean)
  return(npa)
}

## RAE (relative absolute deviation) function
rae<-function(p,f,id){
  trae<-abs(p/f-1)
  #nrae<-unique(ave(trae,id,FUN=function(x) mean(x, na.rm=T)))
  nrae<-tapply(trae,id,mean,na.rm=T)
  return(nrae)
}

## RAE weighted fuction (according to market transactions)
wrae<-function(p,f,q,id){
  trae<-abs(p/f-1)*q
  qse<- ave(q,id,FUN=function(x) sum(x, na.rm=T))
  #wnrae<-ave(trae/qse,id,FUN=function(x) sum(x, na.rm=T))
  wnrae<-tapply(trae/qse,id,sum,na.rm=T)
  return(wnrae)
}


## DP-> deviation of parity
dpj<-function(p1,f1,p2,f2,id){
  trae<-abs((p1/f1)/(p2/f2)-1)
  #ndpj<-ave(trae,id,FUN=function(x) mean(x, na.rm=T))
  ndpj<-tapply(trae,id,mean,na.rm=T)
  return(ndpj)
}

## Standard deviation
disperse<-function(p1,id){
  disp<-tapply(p1,id,sd,na.rm=TRUE)
  return(disp)
  }

## asset turnover
turnover<-function(q1,id){
  tov<-tapply(q1,id,sum,na.rm=TRUE)
  return(tov)
}

rae(df$group.p_c,df$group.nav,df$uses)
rae(df$group.p_c,df$group.nav,df$uses)


abs((df$group.p_b/(df$group.p_a))/(df$fvb/df$fva)-1)
pa(df$group.p_a,df$fva,df$uses)

rae_a<-rae(df$group.p_a,df$fva,df$uses)
rae_b<-rae(df$group.p_b,df$fvb,df$uses)
rae_c<-rae(df$group.p_c,df$fvc,df$uses)
rae_cnav<-rae(df$group.p_c,df$group.nav,df$uses)

wrae_a<-wrae(df$group.p_a,df$fva,df$group.q_a,df$uses)
wrae_b<-wrae(df$group.p_b,df$fvb,df$group.q_b,df$uses)
wrae_c<-wrae(df$group.p_c,df$fvc,df$group.q_c,df$uses)
wrae_cnav<-wrae(df$group.p_c,df$group.nav,df$group.q_c,df$uses)


rap<-rapdpj(df$group.p_b,df$fvb,df$group.p_a,df$fva,df$uses)
raes<-rbind(rae_a,rae_b,rae_c,rae_cnav,rap)
wraes<-rbind(wrae_a,wrae_b,rae_c,wrae_cnav)


nses<-8
totala<- c(rep(20*3,nses),rep(10*3,nses))
totalb<- c(rep(20*3,nses),rep(10*3,nses))
totalc<- c(rep(20*3,nses),rep(10*3,nses))

turnover_a<-turnover(df$group.q_a,df$uses)/totala*100
turnover_b<-turnover(df$group.q_b,df$uses)/totalb*100
turnover_c<-turnover(df$group.q_c,df$uses)/totalc*100

vueltas<-rbind(turnover_a,turnover_b,turnover_c)

vol_a<-disperse(df$group.p_a,df$uses)
vol_b<-disperse(df$group.p_b,df$uses)
vol_c<-disperse(df$group.p_c,df$uses)

volatilities<-rbind(vol_a,vol_b,vol_c)


cor(df$group.p_a[df$uses==2010],df$group.p_b[df$uses==2010],use="complete.obs")
cor(df$group.p_a[df$uses==2011],df$group.p_b[df$uses==2011],use="complete.obs")
cor(df$group.p_a[df$uses==2020],df$group.p_b[df$uses==2020],use="complete.obs")
cor(df$group.p_a[df$uses==2021],df$group.p_b[df$uses==2021],use="complete.obs")

cor(df$group.p_a[df$uses==3010],df$group.p_b[df$uses==3010],use="complete.obs")
cor(df$group.p_a[df$uses==3011],df$group.p_b[df$uses==3011],use="complete.obs")
cor(df$group.p_a[df$uses==3020],df$group.p_b[df$uses==3020],use="complete.obs")
cor(df$group.p_a[df$uses==3021],df$group.p_b[df$uses==3021],use="complete.obs")
