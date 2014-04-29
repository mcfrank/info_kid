rm(list=ls())
library(directlabels)
source("useful.R")

clean <- function(x) {
  x <- as.numeric(gsub("[^0-9\\.]","",as.character(x)))
  x[is.na(x)] <- 0
  x[x > 100] <- 0
  return(x)
}

raw.data <- read.csv("data/info_e1_data.csv")
length(raw.data$WorkerId)

raw.data <- raw.data[!duplicated(raw.data$WorkerId),]
length(raw.data$WorkerId)

vars <- names(raw.data)
raw.data$Input.robot_trial_type <- as.character(raw.data$Input.robot_trial_type)
raw.data$Input.dino_trial_type <- as.character(raw.data$Input.dino_trial_type)
raw.data$Input.rocket_trial_type <- as.character(raw.data$Input.rocket_trial_type)
raw.data$Input.bear_trial_type <- as.character(raw.data$Input.bear_trial_type)

mvs <- grep("(Input)|(Answer)",vars)
mdata <- melt(raw.data,id.vars="WorkerId",measure.vars=vars[mvs])

# extract the actual answers
targets <- mdata[grep("val|type|bet|_answer",mdata$variable),]

targets$trial <- "dino"
targets$trial[grep("robot",targets$variable)] <- "robot"
targets$trial[grep("rocket",targets$variable)] <- "rocket"
targets$trial[grep("bear",targets$variable)] <- "bear"
targets$trial <- factor(targets$trial)
targets$variable <- sub("(robot)|(rocket)|(dino)|(bear)|(robot)_|(rocket)_|(dino)_|(bear)_","",targets$variable)   

targets$variable <- sub("Answer.|Input.","",targets$variable)

# recast as what we want
data <- dcast(targets,WorkerId + trial ~ variable)
data$val1 <- clean(data$"1val")
data$val2 <- clean(data$"2val")

# now choose target
data$target.bet <- data$val1
data$target.bet[data$bet_position=="right"] <- data$val2[data$bet_position=="right"]

# exclusions by catch trials etc.
data$ans1 <- data$answer1
data$ans1[data$bet_position=="right"] <- data$answer2[data$bet_position=="right"]
data$ans2 <- data$answer2
data$ans2[data$bet_position=="right"] <- data$answer1[data$bet_position=="right"]
data$c.ans1 <- sapply(data$trial_type,FUN=function(x) {as.numeric(substr(x,1,1))})
data$c.ans2 <- sapply(data$trial_type,FUN=function(x) {as.numeric(substr(x,2,2))})

sumto100 <- (data$val1 + data$val2)==100
checktrials <- data$c.ans1 == data$ans1 & data$c.ans2 == data$ans2
checktrials[is.na(checktrials)] <- FALSE

mean(!sumto100)
mean(!checktrials)

data <- subset(data,sumto100 & checktrials)
t.test(data$target.bet[data$trial_type=="11"]-50)
t.test(data$target.bet[data$trial_type=="12"]-50)
t.test(data$target.bet[data$trial_type=="13"]-50)
t.test(data$target.bet[data$trial_type=="23"]-50)

t.test(data$target.bet[data$trial_type=="11"],data$target.bet[data$trial_type=="12"])
t.test(data$target.bet[data$trial_type=="11"],data$target.bet[data$trial_type=="13"])
t.test(data$target.bet[data$trial_type=="11"],data$target.bet[data$trial_type=="23"])

t.test(data$target.bet[data$trial_type=="12"],data$target.bet[data$trial_type=="13"])
t.test(data$target.bet[data$trial_type=="11"],data$target.bet[data$trial_type=="23"])

ms <- aggregate(target.bet ~ trial_type, data, mean)
ms$sd <- aggregate(target.bet ~ trial_type, data, sd)$target.bet
ms$cil <- aggregate(target.bet ~ trial_type , data, ci.low)$target.bet
ms$cih <- aggregate(target.bet ~ trial_type , data, ci.high)$target.bet
ms$preds <- c(50,66.66,75,60)

# quartz()
levels(ms$trial_type) <- c("1/1","1/2","1/3","2/3")
qplot(preds,target.bet, data=ms, colour=trial_type,
      xlim=c(40,80), ylim=c(40,80),
      xlab="Model prediction",ylab="Average bet",
      ymin=target.bet - cil, ymax = target.bet + cih,
      geom="pointrange") + 
#       geom_abline(slope=1,intercept=0,lty=2,colour="red") + 
      geom_smooth(method="lm",se=FALSE,aes(group=1),
                  colour="black",lty=2)+
  geom_dl(aes(label=trial_type),method="last.points") + 
  theme_bw() +
  theme(legend.position="none")
        
summary(lm(target.bet ~ trial_type - 1 + trial + bet_position,data=data))
cor.test(ms$target.bet,ms$preds)
summary(lm(ms$target.bet~ms$preds))