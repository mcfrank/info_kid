rm(list=ls())
source("analysis/useful.R")

d1 <- read.csv("data/info_e4_rep_data.csv")
d1$expt <- "replication"
d2 <- read.csv("data/info_e4_disambig_data.csv")
d2$expt <- "disambiguation"
d3 <- read.csv("data/info_e4_salience_data.csv")
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

# reverse disambiguation responses 
# the disambiguation "inference" trials are coded as correct when 
md$correct[md$expt=="disambiguation" & md$trial.type=="inference"] <- 1 - md$correct[md$expt=="disambiguation" & md$trial.type=="inference"]

mss <- aggregate(correct ~ trial.type + expt + age + SID,
                 md, mean)
mss$age_group <- factor(floor(mss$age))

ms <- ddply(mss, .(trial.type,expt,age_group), 
            summarise,          
            cil = ci.low(correct),
            cih = ci.high(correct),
            correct = mean(correct))

quartz()
qplot(age_group, correct, fill = trial.type,
      stat="identity",
      position=position_dodge(.9),
      ymin=correct-cil, ymax=correct+cih,
      data=ms, facets = .~expt, geom=c("bar","linerange"),
      xlab="Age Group (Years)",ylab="Proportion Inference Consistent") + 
  geom_hline(yintercept=.5,lty=2) 