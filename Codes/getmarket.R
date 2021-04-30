rm(list = ls())
#setwd("/cloud/project/data/abcone")
setwd("~/Desktop/jotarepos/etfs/data/abcone")
files = list.files(pattern="*.csv")

##We create a new identifier that reads d$uses
### 1 digit: 2 or 3 assets
### 2 digit: 1 or 0 correlation
##  3-4 digits: Number of sesion
##  5 digit: Early 0 or late round 1 
important_names<-c( "group.nav_a","group.nav_b","group.nav", "group.p_a" ,"group.q_a","group.d_a","group.p_b","group.q_b","group.d_b" ,"group.p_c", "group.q_c","group.d_c","subsession.round_number","subsession.ronda","player.nbida",
                    "player.naska", "player.nbidb", "player.naskb", "player.nbidc","player.naskc","player.n_a","player.n_b","player.t_a","player.t_b","participant.id_in_session","session.code")

df<-NULL

for(i in seq(along=files)){
  d<-read.csv(files[i],sep=",",header=T, stringsAsFactors = FALSE)
  d<-d[,important_names]
  d$tre <- 1
  d$session<-3100+i
  #d<-d[!duplicated(d), ]
  df<-rbind(df,d)
}


setwd("~/Desktop/jotarepos/etfs/data/abczero")
files = list.files(pattern="*.csv")

##We create a new identifier that reads d$uses
### 1 digit: 2 or 3 assets
### 2 digit: 1 or 0 correlation
##  3-4 digits: Number of sesion
##  5 digit: Early 0 or late round 1 

for(i in seq(along=files)){
  d<-read.csv(files[i],sep=",",header=T, stringsAsFactors = FALSE)
  d<-d[,important_names]
  d$tre <- 3
  d$session<-3000+i
  #d<-d[!duplicated(d), ]
  df<-rbind(df,d)
}


#setwd("/cloud/project/data/abone")
setwd("~/Desktop/jotarepos/etfs/data/abone")

files = list.files(pattern="*.csv")

for(i in seq(along=files)){
  d<-read.csv(files[i],sep=",",header=T, stringsAsFactors = FALSE)
  d<-d[,important_names]
  d$tre <- 0
  d$session<-2100+i
  #d<-d[!duplicated(d), ]
  df<-rbind(df,d)
}

setwd("~/Desktop/jotarepos/etfs/data/abzero/")

files = list.files(pattern="*.csv")

for(i in seq(along=files)){
  d<-read.csv(files[i],sep=",",header=T, stringsAsFactors = FALSE)
  d<-d[,important_names]
  d$tre <- 2
  d$session<-2000+i
  #d<-d[!duplicated(d), ]
  df<-rbind(df,d)
}
df$type<-0
df$type[df$participant.id_in_session>3]<-1
df$type[df$participant.id_in_session>6]<-2

df$active_a<-(df$player.nbida+df$player.naska)*9
df$active_b<-(df$player.nbidb+df$player.naskb)*9
df$active_c<-(df$player.nbidc+df$player.naskc)*9
df$ids<-df$session*100+df$subsession.round_number
df$portab<-abs(df$player.n_a+df$player.t_a-df$player.n_b-df$player.t_b)
df$portype.a<-0
df$portype.b<-0
df$portype.c<-0
df$portype.a[df$type==0]<-df$portab[df$type==0]
df$portype.b[df$type==1]<-df$portab[df$type==1]
df$portype.c[df$type==2]<-df$portab[df$type==2]

drisk<-read.csv("~/Desktop/jotarepos/etfs/data/risk_bomb/allsessionrisk.csv",sep=",",header=T, stringsAsFactors = FALSE)
drisk<-drisk[,c("player.boxes_collected","session.code")]

drisk <- aggregate(drisk,by = list(drisk$session.code),FUN = mean)
drisk$session.code<-drisk$Group.1

df<-merge(df,drisk,by=c("session.code"))

df <- aggregate(df,by = list(df$ids),FUN = mean)

df<-df[,-1]

df$group.p_a[df$group.p_a==0]<-NA
df$group.p_b[df$group.p_b==0]<-NA
df$group.p_c[df$group.p_c==0]<-NA
df$group.nav[df$group.nav==0]<-NA
df$group.nav_a[df$group.nav_a==0]<-NA
df$group.nav_b[df$group.nav_b==0]<-NA

df$uses<-df$session*10
df$uses[df$subsession.round_number>15]<-df$uses[df$subsession.round_number>15]+1
df$fva<-10
df$fvb<-df$subsession.ronda-8
df$fvb[df$fvb<0]<-0
df$fvb<-18-df$fvb
df$fvc<-df$fvb+df$fva

save(df,file="etfmarket.Rda")
# Please move this dataset to /data!

