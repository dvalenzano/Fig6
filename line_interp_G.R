#########################################################################################################
############################### Multiple plot function ##################################################
#########################################################################################################
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##########################################################################################
############################## END OF MULTIPLOT FUNCTION #################################
##########################################################################################

library(lattice)
library(ggplot2)

gdays <- read.csv('/Volumes/group_dv/personal/DValenzano/month-by-month/Sep2014/qtl-direction-analysis/ReAutoReqtlresults/gdays2_tab.csv', sep=',', header=TRUE)
gdaysM <- read.csv('/Volumes/group_dv/personal/DValenzano/month-by-month/Sep2014/qtl-direction-analysis/ReAutoReqtlresults/gdaysM2_tab.csv', sep=',', header=TRUE)
gdaysF <- read.csv('/Volumes/group_dv/personal/DValenzano/month-by-month/Sep2014/qtl-direction-analysis/ReAutoReqtlresults/gdaysF2_tab.csv', sep=',', header=TRUE)
gdays_Res <- read.csv('/Volumes/group_dv/personal/DValenzano/month-by-month/Oct2014/gdaysRes2_tab.csv', sep=',', header=TRUE)

# First, I start with gdays (all individuals together)

##########################################################################################
############################## FOR FIRST FIGURE RUN FROM HERE#############################
##########################################################################################

gdays_LG1 <- subset(gdays, gdays$LG == '1')
gdays_LG2 <- subset(gdays, gdays$LG == '2')
gdays_LG3 <- subset(gdays, gdays$LG == '3')
gdays_LG4 <- subset(gdays, gdays$LG == '4')
gdays_LG5 <- subset(gdays, gdays$LG == '5')
gdays_LG6 <- subset(gdays, gdays$LG == '6')
gdays_LG7 <- subset(gdays, gdays$LG == '7')
gdays_LG8 <- subset(gdays, gdays$LG == '8')
gdays_LG9 <- subset(gdays, gdays$LG == '9')
gdays_LG10 <- subset(gdays, gdays$LG == '10')
gdays_LG11 <- subset(gdays, gdays$LG == '11')
gdays_LG12 <- subset(gdays, gdays$LG == '12')
gdays_LG13 <- subset(gdays, gdays$LG == '13')
gdays_LG14 <- subset(gdays, gdays$LG == '14')
gdays_LG15 <- subset(gdays, gdays$LG == '15')
gdays_LG16 <- subset(gdays, gdays$LG == '16')
gdays_LG17 <- subset(gdays, gdays$LG == '17')
gdays_LG18 <- subset(gdays, gdays$LG == '18')
gdays_LG19 <- subset(gdays, gdays$LG == '19')

###LG1 for gdays

gdays_LG1_2 <- gdays_LG1[,1:3]
gdays_LG1_2
gdays_LG1_3 <- rbind(gdays_LG1_2, gdays_LG1_2, gdays_LG1_2)
med1 <- c(gdays_LG1$med0,gdays_LG1$med1, gdays_LG1$med2) 
mgr1 <- c(rep('SL/SL', length(gdays_LG1$med0)), rep('SL/LL', length(gdays_LG1$med1)), rep('LL/LL', length(gdays_LG1$med2)))
gdays_LG1_3$med <- med1
gdays_LG1_3$genotype <- mgr1
gdays_LG1_3

pg1 <- ggplot(gdays_LG1_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG1") + ylim(180,380) + xlim(0,max(gdays_LG1$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q1<- qplot(cM,neg_log.qval., data=gdays_LG1, xlab="cM", ylab="-log(q)", ylim=c(0,5), xlim=c(0, max(gdays_LG1$cM)))
#multiplot(pg1, q1, cols=1)

###LG2 for gdays
gdays_LG2_2 <- gdays_LG2[,1:3]
gdays_LG2_2
gdays_LG2_3 <- rbind(gdays_LG2_2, gdays_LG2_2, gdays_LG2_2)
med2 <- c(gdays_LG2$med0,gdays_LG2$med1, gdays_LG2$med2) 
mgr2 <- c(rep('SL/SL', length(gdays_LG2$med0)), rep('SL/LL', length(gdays_LG2$med1)), rep('LL/LL', length(gdays_LG2$med2)))
gdays_LG2_3$med <- med2
gdays_LG2_3$genotype <- mgr2
gdays_LG2_3

pg2<- ggplot(gdays_LG2_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG2") + ylim(180,380) + xlim(0,max(gdays_LG2$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q2<- qplot(cM,neg_log.qval., data=gdays_LG2, xlab="cM", ylab="-log(q)", ylim=c(0,5), xlim=c(0, max(gdays_LG2$cM)))
#multiplot(pg2, q2, cols=1)

###LG3 for gdays
gdays_LG3_2 <- gdays_LG3[,1:3]
gdays_LG3_2
gdays_LG3_3 <- rbind(gdays_LG3_2, gdays_LG3_2, gdays_LG3_2)
med3 <- c(gdays_LG3$med0,gdays_LG3$med1, gdays_LG3$med2) 
mgr3 <- c(rep('SL/SL', length(gdays_LG3$med0)), rep('SL/LL', length(gdays_LG3$med1)), rep('LL/LL', length(gdays_LG3$med2)))
gdays_LG3_3$med <- med3
gdays_LG3_3$genotype <- mgr3
gdays_LG3_3

pg3<- ggplot(gdays_LG3_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG3") + ylim(180,380) + xlim(0,max(gdays_LG3$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q3<- qplot(cM,neg_log.qval., data=gdays_LG3, xlab="cM", ylab="-log(q)", ylim=c(0,5), xlim=c(0, max(gdays_LG3$cM)))
#multiplot(pg3, q3, cols=1)

###LG4 for gdays
gdays_LG4_2 <- gdays_LG4[,1:3]
gdays_LG4_2
gdays_LG4_3 <- rbind(gdays_LG4_2, gdays_LG4_2, gdays_LG4_2)
med4 <- c(gdays_LG4$med0,gdays_LG4$med1, gdays_LG4$med2) 
mgr4 <- c(rep('SL/SL', length(gdays_LG4$med0)), rep('SL/LL', length(gdays_LG4$med1)), rep('LL/LL', length(gdays_LG4$med2)))
gdays_LG4_3$med <- med4
gdays_LG4_3$genotype <- mgr4
gdays_LG4_3

pg4<- ggplot(gdays_LG4_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays, LG4") + ylim(180,380) + xlim(0,max(gdays_LG4$cM)) + xlab("cM")+ylab("Days") +theme(legend.position="bottom")

q4<- qplot(cM,neg_log.qval., data=gdays_LG4, xlab="cM", ylab="-log(q)", ylim=c(0,5), xlim=c(0, max(gdays_LG4$cM)))
#multiplot(pg4, q4, cols=1)

#multiplot(pg1, pg2, pg3, pg4, cols=2)

#TO ARRANGE MULTIPLE PANELS SEE HERE:
# http://stackoverflow.com/questions/9490482/combined-plot-of-ggplot2-not-in-a-single-plot-using-par-or-layout-functio

layout <- matrix(c(1,2,2,3,4,4,5,6,6,7,8,8), nrow = 3)
multiplot(q1, pg1, q2, pg2, q3, pg3, q4, pg4, layout=layout)

##########################################################################################
############################## FOR FIRST FIGURE RUN TO HERE ##############################
##########################################################################################


########################################################################################
####################### FOR SEX-REGRESSED RUN FROM HERE ################################
########################################################################################

gdays_Res_LG1 <- subset(gdays_Res, gdays_Res$LG == '1')
gdays_Res_LG2 <- subset(gdays_Res, gdays_Res$LG == '2')
gdays_Res_LG3 <- subset(gdays_Res, gdays_Res$LG == '3')
gdays_Res_LG4 <- subset(gdays_Res, gdays_Res$LG == '4')
gdays_Res_LG5 <- subset(gdays_Res, gdays_Res$LG == '5')
gdays_Res_LG6 <- subset(gdays_Res, gdays_Res$LG == '6')
gdays_Res_LG7 <- subset(gdays_Res, gdays_Res$LG == '7')
gdays_Res_LG8 <- subset(gdays_Res, gdays_Res$LG == '8')
gdays_Res_LG9 <- subset(gdays_Res, gdays_Res$LG == '9')
gdays_Res_LG10 <- subset(gdays_Res, gdays_Res$LG == '10')
gdays_Res_LG11 <- subset(gdays_Res, gdays_Res$LG == '11')
gdays_Res_LG12 <- subset(gdays_Res, gdays_Res$LG == '12')
gdays_Res_LG13 <- subset(gdays_Res, gdays_Res$LG == '13')
gdays_Res_LG14 <- subset(gdays_Res, gdays_Res$LG == '14')
gdays_Res_LG15 <- subset(gdays_Res, gdays_Res$LG == '15')
gdays_Res_LG16 <- subset(gdays_Res, gdays_Res$LG == '16')
gdays_Res_LG17 <- subset(gdays_Res, gdays_Res$LG == '17')
gdays_Res_LG18 <- subset(gdays_Res, gdays_Res$LG == '18')
gdays_Res_LG19 <- subset(gdays_Res, gdays_Res$LG == '19')


gdays_Res_LG1_2 <- gdays_Res_LG1[,1:3]
gdays_Res_LG1_2
gdays_Res_LG1_3 <- rbind(gdays_Res_LG1_2, gdays_Res_LG1_2, gdays_Res_LG1_2)
med1 <- c(gdays_Res_LG1$med0,gdays_Res_LG1$med1, gdays_Res_LG1$med2) 
mgr1 <- c(rep('SL/SL', length(gdays_Res_LG1$med0)), rep('SL/LL', length(gdays_Res_LG1$med1)), rep('LL/LL', length(gdays_Res_LG1$med2)))
gdays_Res_LG1_3$med <- med1
gdays_Res_LG1_3$genotype <- mgr1
gdays_Res_LG1_3

pg1 <- ggplot(gdays_Res_LG1_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG1") + ylim(180,380) + xlim(0,max(gdays_Res_LG1$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q1<- qplot(cM,neg_log.qval., data=gdays_Res_LG1, xlab="cM", ylab="-log(q)", ylim=c(0,5), xlim=c(0, max(gdays_Res_LG1$cM)))
#multiplot(pg1, q1, cols=1)

###LG2 for gdays_Res
gdays_Res_LG2_2 <- gdays_Res_LG2[,1:3]
gdays_Res_LG2_2
gdays_Res_LG2_3 <- rbind(gdays_Res_LG2_2, gdays_Res_LG2_2, gdays_Res_LG2_2)
med2 <- c(gdays_Res_LG2$med0,gdays_Res_LG2$med1, gdays_Res_LG2$med2) 
mgr2 <- c(rep('SL/SL', length(gdays_Res_LG2$med0)), rep('SL/LL', length(gdays_Res_LG2$med1)), rep('LL/LL', length(gdays_Res_LG2$med2)))
gdays_Res_LG2_3$med <- med2
gdays_Res_LG2_3$genotype <- mgr2
gdays_Res_LG2_3

pg2<- ggplot(gdays_Res_LG2_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG2") + ylim(180,380) + xlim(0,max(gdays_Res_LG2$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q2<- qplot(cM,neg_log.qval., data=gdays_Res_LG2, xlab="cM", ylab="-log(q)", ylim=c(0,5), xlim=c(0, max(gdays_Res_LG2$cM)))
#multiplot(pg2, q2, cols=1)

###LG3 for gdays_Res
gdays_Res_LG3_2 <- gdays_Res_LG3[,1:3]
gdays_Res_LG3_2
gdays_Res_LG3_3 <- rbind(gdays_Res_LG3_2, gdays_Res_LG3_2, gdays_Res_LG3_2)
med3 <- c(gdays_Res_LG3$med0,gdays_Res_LG3$med1, gdays_Res_LG3$med2) 
mgr3 <- c(rep('SL/SL', length(gdays_Res_LG3$med0)), rep('SL/LL', length(gdays_Res_LG3$med1)), rep('LL/LL', length(gdays_Res_LG3$med2)))
gdays_Res_LG3_3$med <- med3
gdays_Res_LG3_3$genotype <- mgr3
gdays_Res_LG3_3

pg3<- ggplot(gdays_Res_LG3_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG3") + ylim(180,380) + xlim(0,max(gdays_Res_LG3$cM)) + xlab("cM")+ylab("Days")+theme(legend.position="bottom")

q3<- qplot(cM,neg_log.qval., data=gdays_Res_LG3, xlab="cM", ylab="-log(q)", ylim=c(0,5), xlim=c(0, max(gdays_Res_LG3$cM)))
#multiplot(pg3, q3, cols=1)

###LG4 for gdays_Res
gdays_Res_LG4_2 <- gdays_Res_LG4[,1:3]
gdays_Res_LG4_2
gdays_Res_LG4_3 <- rbind(gdays_Res_LG4_2, gdays_Res_LG4_2, gdays_Res_LG4_2)
med4 <- c(gdays_Res_LG4$med0,gdays_Res_LG4$med1, gdays_Res_LG4$med2) 
mgr4 <- c(rep('SL/SL', length(gdays_Res_LG4$med0)), rep('SL/LL', length(gdays_Res_LG4$med1)), rep('LL/LL', length(gdays_Res_LG4$med2)))
gdays_Res_LG4_3$med <- med4
gdays_Res_LG4_3$genotype <- mgr4
gdays_Res_LG4_3

pg4<- ggplot(gdays_Res_LG4_3, aes(x=cM, y=med, group=genotype, color=genotype)) +
  geom_smooth(alpha=.4, size=1) + geom_point(alpha=1) +
  ggtitle("gdays_Res, LG4") + ylim(180,380) + xlim(0,max(gdays_Res_LG4$cM)) + xlab("cM")+ylab("Days") +theme(legend.position="bottom")

q4<- qplot(cM,neg_log.qval., data=gdays_Res_LG4, xlab="cM", ylab="-log(q)", ylim=c(0,5), xlim=c(0, max(gdays_Res_LG4$cM)))
#multiplot(pg4, q4, cols=1)

#multiplot(pg1, pg2, pg3, pg4, cols=2)

#TO ARRANGE MULTIPLE PANELS SEE HERE:
# http://stackoverflow.com/questions/9490482/combined-plot-of-ggplot2-not-in-a-single-plot-using-par-or-layout-functio

layout <- matrix(c(1,2,2,3,4,4,5,6,6,7,8,8), nrow = 3)
multiplot(q1, pg1, q2, pg2, q3, pg3, q4, pg4, layout=layout)

########################################################################################
####################### FOR SEX-REGRESSED RUN TO HERE ##################################
########################################################################################

save.image(file="/Volumes/group_dv/personal/DValenzano/Dec2014/line_interp_G.RData")
