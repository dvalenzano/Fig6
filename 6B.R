# Goal: to run survival statistics for cross 1(g) and cross 2 (aa)

###### CROSS G #########

g <- read.csv("/Volumes/group_dv/personal/DValenzano/month-by-month/Sep2015/g-cross_2.csv", header=T, sep=',')
g
library(survival)
fitg <- survfit(formula=Surv(weeks,status)~group, data=g)
plot(fitg, xlim=c(0,100), ylab="Fraction survived", xlab="Time (days)")

gll.f1.f2 <- subset(g, g$group!="P0SL") 
gf1.f2 <- subset(gll.f1.f2, gll.f1.f2$group!="P0LL") 
gll.f1 <-  subset(gll.f1.f2, gll.f1.f2$group!="F2") 
gll.f2 <-  subset(gll.f1.f2, gll.f1.f2$group!="F1") 

gsl.f1.f2 <- subset(g, g$group!="P0LL") 
gsl.f1 <-  subset(gsl.f1.f2, gsl.f1.f2$group!="F2") 
gsl.f2 <-  subset(gsl.f1.f2, gsl.f1.f2$group!="F1")

gsl.ll.f1 <- subset(g, g$group!="F2") 
gsl.ll <-  subset(gsl.ll.f1, gsl.ll.f1$group!="F1") 

survdiff(Surv(weeks, status)~group, data = gsl.f2)
survdiff(Surv(weeks, status)~group, data = gsl.ll)
survdiff(Surv(weeks, status)~group, data = gsl.f1)
survdiff(Surv(weeks, status)~group, data = gll.f1)
survdiff(Surv(weeks, status)~group, data = gll.f2)
survdiff(Surv(weeks, status)~group, data = gf1.f2)

#### CROSS AA #####

aa <- read.csv("/Volumes/group_dv/personal/DValenzano/month-by-month/Sep2015/aa-cross_2.csv", header=T, sep=',')
aa

fitaa <- survfit(formula=Surv(weeks,status)~group, data=aa)
plot(fitaa, xlim=c(0,100), ylab="Fraction survived", xlab="Time (days)")

aall.f1.f2 <- subset(aa, aa$group!="P0SL") 
aaf1.f2 <- subset(aall.f1.f2, aall.f1.f2$group!="P0LL") 
aall.f1 <-  subset(aall.f1.f2, aall.f1.f2$group!="F2") 
aall.f2 <-  subset(aall.f1.f2, aall.f1.f2$group!="F1") 

aasl.f1.f2 <- subset(aa, aa$group!="P0LL") 
aasl.f1 <-  subset(aasl.f1.f2, aasl.f1.f2$group!="F2") 
aasl.f2 <-  subset(aasl.f1.f2, aasl.f1.f2$group!="F1")

aasl.ll.f1 <- subset(aa, aa$group!="F2") 
aasl.ll <-  subset(aasl.ll.f1, aasl.ll.f1$group!="F1") 

survdiff(Surv(weeks, status)~group, data = aasl.f2)
survdiff(Surv(weeks, status)~group, data = aasl.f1)
