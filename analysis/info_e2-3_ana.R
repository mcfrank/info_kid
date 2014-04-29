rm(list=ls())
source("analysis/useful.R")

d1 <- read.csv("data/info_e2_data.csv")
d1$expt <- "Experiment 2"
d2 <- read.csv("data/info_e3_data.csv")
d2$expt <- "Experiment 3"

## bind all together
d <- rbind.fill(d1,d2)
d$expt <- factor(d$expt)
d$age.group <- factor(d$age.group)
d <- d[d$age.group != 5,] # remove the one 5-year-old

#### PLOT ####
mss <- aggregate(correct ~ type + age.group + expt + subid,d,mean)
ms <- aggregate(correct ~ type + age.group + expt,mss,mean)
ms$ci.h <- aggregate(correct ~ type + age.group + expt,mss,ci.high)$correct
ms$ci.l <- aggregate(correct ~ type + age.group + expt,mss,ci.high)$correct
ms$n <- aggregate(subid ~ type + age.group + expt,mss,n.unique)$subid

# bar graph
# pdf("~/Projects/Pragmatics/writeup/ICOM journal/figures/kids_barplot.pdf",
#     width=6.5,height=3)
qplot(age.group,y=correct,fill=type,facets=.~expt,
      group=type,pch=type,stat="identity",
      ymin=correct - ci.l,ymax = correct + ci.h,
      position=position_dodge(),
      xlab="Age Group (Years)",ylab="Proportion Correct",
      data=subset(ms,expt!="Experiment 4"),
      geom="bar",ylim=c(0,1)) + 
  geom_pointrange(position=position_dodge(width=.9),pch="") +
  geom_abline(intercept=.5,slope=0,lty=2) +
  theme_bw() 
# dev.off()

#### descriptives ####
d1$male <- d1$gender=="M"
g <- aggregate(male ~ subid + age.group,d1,unique)
aggregate(male ~ age.group,g,sum)
aggregate(age_months ~ age.group,d1,mean)
aggregate(subid ~ age.group,d1,n.unique)
aggregate(correct ~ age.group + type,d1,mean)
aggregate(correct ~ age.group + type,d1,sd)


t.test(mss$correct[mss$expt=="Experiment 2" & 
                     mss$type=="inference"]-.5)
wilcox.test(mss$correct[mss$expt=="Experiment 2" & 
                     mss$type=="inference"]-.5)
t.test(mss$correct[mss$expt=="Experiment 2" & 
                     mss$type=="inference" & 
                     mss$age.group==3]-.5)
t.test(mss$correct[mss$expt=="Experiment 2" & 
                     mss$type=="inference" & 
                     mss$age.group==4]-.5)
wilcox.test(mss$correct[mss$expt=="Experiment 2" & 
                          mss$type=="inference"]-.5)

t.test(mss$correct[mss$expt=="Experiment 2" & 
                     mss$type=="inference"]-.5)

aggregate(subid ~ age.group,d2,n.unique)
d2$male <- d2$gender=="M"
g <- aggregate(male ~ subid + age.group,d2,unique)
aggregate(male ~ age.group,g,sum)
aggregate(age_months ~ age.group,d2,mean)
aggregate(correct ~ age.group + type,d2,mean)
aggregate(correct ~ age.group + type,d2,sd)

#### STATISTICS ####
d1$age.group <- factor(d1$age.group)
d1$subid <- factor(d1$subid)
d1$type <- factor(d1$type,levels=c("inference","filler"))
l1 <- lmer(correct ~ age.group * type  + (type|subid) + (1 |item),
     data=d1,family="binomial")
l2 <- lmer(correct ~ age.group + type  + (type|subid) + (1 |item),
           data=d1,family="binomial")
anova(l1,l2)

d2 <- subset(d2,age.group < 5)
d2$subid <- factor(d2$subid)
d2$age.group <- factor(d2$age.group,levels=c("3","4"))
d2$type <- factor(d2$type,levels=c("inference","filler"))
l1a <- lmer(correct ~ age.group * type + (type|subid) + (1|item),
     data=d2,family="binomial")
l2a <- lmer(correct ~ age.group + type + (type|subid) + (1|item),
            data=d2,family="binomial")
anova(l1a,l2a)

## now do experiment 3
d3$age.group <- factor(d3$age.group,levels=c("3","4"))
d3$type <- factor(d3$type,levels=c("inference","filler"))
lmer(correct ~ age.group * type + (type|subid) + (age.group * type|item),
     data=d3,family="binomial")


d3$age.group <- factor(d3$age.group,levels=c("4","3"))
d3$type <- factor(d3$type,levels=c("inference","filler"))
lmer(correct ~ age.group * type + (type|subid) + (age.group * type|item),
     data=d3,family="binomial")

