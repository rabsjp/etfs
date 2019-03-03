rm(list = ls())
setwd("/cloud/project/data/")

## Call the data. unique id per session. 
load("etfmarket.Rda")

imp_sessions<-c("3011")
dplot<-df[df$uses==imp_sessions,]
png("prices3.png")
plot(dplot$subsession.round_number,dplot$group.p_c,ylim=c(0,80),col="blue",xaxt = "n",xlab="period",ylab = "prices")
axis(1, at=c(1:15), labels=c(1:15))
lines(dplot$subsession.round_number,dplot$group.nav,col="black")
points(dplot$subsession.round_number,dplot$group.p_a,col="gray",pch=2)
points(dplot$subsession.round_number,dplot$group.p_b,col="red",pch=3)
lines(dplot$subsession.round_number,dplot$fva,col="gray")
lines(dplot$subsession.round_number,dplot$fvb,col="red")
lines(dplot$subsession.round_number,dplot$fvc,col="blue")
legend(10, 70, c("NAV","C","B","A","FV A","FV B", "FV C"), col = c("black","blue","red","gray","gray","red","blue"), pch = c(NA,1,3,2,NA,NA,NA),lty = c(1,NA,NA,NA,1,1,1),bty = "n")
dev.off()