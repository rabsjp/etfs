rm(list = ls())
#setwd("/cloud/project/data/abcone")
setwd("~/Desktop/jotarepos/etfs/data/abcone")
files = list.files(pattern="*.csv")

##We create a new identifier that reads d$uses
### 1 digit: 2 or 3 assets
### 2 digit: 1 or 0 correlation
##  3-4 digits: Number of sesion
##  5 digit: Early 0 or late round 1 

#important_names<-c( "group.nav_a","group.nav_b","group.nav", "group.p_a" ,"group.q_a","group.d_a","group.p_b","group.q_b","group.d_b" ,"group.p_c", "group.q_c","group.d_c","subsession.round_number","subsession.ronda")
important_names<-c(1,14:57)
df<-NULL

for(i in seq(along=files)){
  d<-read.csv(files[i],sep=",",header=T, stringsAsFactors = FALSE)
  d<-d[,important_names]
  d$tre <- 1
  d$session<-3100+i
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
  df<-rbind(df,d)
}

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

save(df,file="etfalldata.Rda")
# Please move this dataset to /data!

