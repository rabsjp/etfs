rm(list = ls())
setwd("/cloud/project/data/")

load("etfmarket.Rda")

## RAE

rae<-function(p,f,id){
  trae<-abs(p/f-1)
  nrae<-unique(ave(trae,id,FUN=function(x) mean(x, na.rm=T)))
  return(nrae)
}

## DP
dpj<-function(p1,f1,p2,f2,id){
  trae<-abs((p1/f1)/(p2/f2)-1)
  ndpj<-unique(ave(trae,id,FUN=function(x) mean(x, na.rm=T)))
  return(ndpj)
}


## Standard deviation
disperse<-function(p1,id){
  return(unique(ave(p1,id,FUN=function(x) sd(x, na.rm=T))))
}



abs((df$group.p_b/(df$group.p_a))/(df$fvb/df$fva)-1)

rae(df$group.p_a,df$fva,df$uses)





unique(ave(df$group.p_a,df$uses,FUN=function(x) sd(x, na.rm=T)))
unique(ave(df$group.p_b,df$uses,FUN=function(x) sd(x, na.rm=T)))
unique(ave(df$group.p_b,df$uses,FUN=function(x) sd(x, na.rm=T)))

cor(df$group.p_a[df$uses==2010],df$group.p_b[df$uses==2010],use="complete.obs")
cor(df$group.p_a[df$uses==2011],df$group.p_b[df$uses==2011],use="complete.obs")
cor(df$group.p_a[df$uses==2020],df$group.p_b[df$uses==2020],use="complete.obs")
cor(df$group.p_a[df$uses==2021],df$group.p_b[df$uses==2021],use="complete.obs")


cor(df$group.p_a[df$uses==3010],df$group.p_b[df$uses==3010],use="complete.obs")
cor(df$group.p_a[df$uses==3011],df$group.p_b[df$uses==3011],use="complete.obs")
cor(df$group.p_a[df$uses==3020],df$group.p_b[df$uses==3020],use="complete.obs")
cor(df$group.p_a[df$uses==3021],df$group.p_b[df$uses==3021],use="complete.obs")
