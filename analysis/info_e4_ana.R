rm(list=ls())
source("~/Projects/R/Ranalysis/useful.R")

d1 <- read.csv("~/Projects/R/info_nouns/data/INFO_REP.csv")
d1$expt <- "replication"
d2 <- read.csv("~/Projects/R/info_nouns/data/INFO_DISAMBIG.csv")
d2$expt <- "disambiguation"
d3 <- read.csv("~/Projects/R/info_nouns/data/INFO_NOWORD.csv")
d3$expt <- "salience"

d <- rbind(d1,d2,d3)
d$expt <- factor(d$expt, levels=c("replication","disambiguation","salience"))
d$age <- d$age_group
md <- melt(d, id.vars=c("SID","age","expt"),
           measure.vars=names(d)[grepl("ambig",names(d)) | 
                                   grepl("inference",names(d))],
           value.name = "correct")
             
md$trial <- as.numeric(str_sub(md$variable,2,2))
md$trial.type <- c("filler","inference")[as.numeric(grepl("inference",md$variable))+1]

# reverse 

mss <- aggregate(correct ~ trial.type + expt + age + SID,
                 md, mean)
mss$age_group <- floor(mss$age)

ms <- ddply(mss, .(trial.type,expt,age_group), 
            summarise,          
            cil = ci.low(correct),
            cih = ci.high(correct),
            correct = mean(correct))

qplot(age_group, correct, fill = trial.type,
      stat="identity",
      position=position_dodge(.9),
      ymin=correct-cil, ymax=correct+cih,
      data=ms, facets = .~expt, geom=c("bar","linerange")) + 
  geom_hline(yintercept=.5,lty=2)