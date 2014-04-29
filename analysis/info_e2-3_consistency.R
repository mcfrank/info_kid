rm(list=ls())
source("analysis/useful.R")

d1 <- read.csv("data/info_e2_data.csv")
d1$expt <- "Experiment 2"
d2 <- read.csv("data/info_e3_data.csv")
d2$expt <- "Experiment 3"

###### WITHIN EXPERIMENT RESPONSES CONSISTENCY (REVIEWER RESPONSE) ####

md1 <- aggregate(correct ~ type + age.group + subid,d1,mean)
md2 <- aggregate(correct ~ type + age.group + subid,d2,mean)

get.freqs <- function(y) {
  return(c(sum(y==0),sum(y==.25),sum(y==.5),sum(y==.75),sum(y==1)) / length(y))
}

quartz()
par(mfrow=c(2,2))
x <- seq(0,1,.25)
n.sims <- 10000
for (i in 1:4) {
  if (i == 1) {
    y <- subset(md1,age.group==3 & type=="inference")$correct  
    t <- "Experiment 2, 3- to 4-year-olds"
  } else if (i == 2) {  
    y <- subset(md1,age.group==4 & type=="inference")$correct  
    t <- "Experiment 2, 4- to 5-year-olds"
  } else if (i == 3) {  
    y <- subset(md2,age.group==3 & type=="inference")$correct  
    t <- "Experiment 3, 3- to 4-year-olds"
  } else if (i == 4) {  
    y <- subset(md2,age.group==4 & type=="inference")$correct  
    t <- "Experiment 3, 4- to 5-year-olds"
  }
  
  freqs <- get.freqs(y)
  plot(x,freqs,ylim=c(0,.8),
       xlab="Proportion Correct",ylab="Response Frequency",
       yaxp=c(0,.8,2),main=t,
       bty="n",pch=20)
  y.hat <- dbinom(x*4,4,mean(y))
  
  freqs.sims <- matrix(nrow=n.sims,ncol=5)
  for (i in 1:n.sims) {
    freqs.sims[i,] <- get.freqs(rbinom(length(y),4,mean(y))/4)
  }
  qs <- matrix(nrow=2,ncol=5)
  for (i in 1:5) {
    qs[,i] <- quantile(freqs.sims[,i],c(.025,.975))
  }
  
  lines(x,y.hat,col="red")
  lines(x,qs[1,],col="red",lty=2)
  lines(x,qs[2,],col="red",lty=2)
}
