rm(list = ls())
#setwd("/cloud/project/data/abcone")
setwd("~/Desktop/jotarepos/etfs/data/abcone")
files = list.files(pattern="*.csv")

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
