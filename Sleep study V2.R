## importing data
library(ggplot2)
sleep<-read.csv(file.choose(), header= TRUE)


## Exploring the data
summary(sleep)

attach(sleep)

summary(DASScore)

ggplot(sleep, aes(x=DASScore)) + geom_histogram(binwidth = 1,color="white", fill="skyblue")+ggtitle("Frequency of DASscore")


str(AllNighter)
AN<-as.factor(AllNighter)
AN

dat<-data.frame(table(AN))
dat
prop.table(table(AN))

names(dat)<-c("AllNighter", "Count")

ggplot(data=dat, aes(x=AllNighter,y=Count))+geom_col(color="white",fill="skyblue")+ggtitle("The count of All Nighter")+scale_x_discrete(breaks=c("0","1"),
                                                                                                                          labels=c("No","Yes"))

## 1.	What is an estimate for the average combined depressed, anxious, stressed score (DASscore) for this population of college students? 

sum(is.na(DASScore))

n<-length(DASScore)
n
x.bar<-mean(DASScore)
x.bar
sd<-sd(DASScore)
sd

## bootstrap
boot.xbars<-c()
for(b in 1:100000){
  boot.sample<-sample(DASScore, n, replace= TRUE)
  boot.xbar<-mean(boot.sample)
  boot.xbars<-c(boot.xbar, boot.xbars)
}

## 95% confidence interval
CI.lower<-sort(boot.xbars)[2500]
CI.upper<-sort(boot.xbars)[97500]

CI<-c(CI.lower, CI.upper)
CI

## Central Limit Theorem

standard_error<-function(s,n){
  return(s/sqrt(n))
}

ci_function<-function(xbar,s,n,tstar){
  lower <- xbar-tstar*standard_error(s,n)
  upper <- xbar+tstar*standard_error(s,n)
  return(c(lower,upper))
}

tstar.das<-qt(0.025,df=n-1, lower.tail=FALSE)
tstar.das

ci_function(x.bar,sd,n,tstar.das)


## 2.	What is an estimate for the proportion of college students who pulled 
##    all nighters (AllNighter) for this population? Do the majority of college 
##    students in this population pull all nighters?

## Proportion
k.nighter<-sum(AllNighter==1)
n.nighter<-length(AllNighter)
phat<-k.nighter/n.nighter

n.nighter*phat
n.nighter*(1-phat)

se.nighter<-sqrt(phat*(1-phat)/n)

z<-qnorm(0.025,mean=0,sd=1,lower.tail=FALSE)

CI<-phat+c(-1,1)*z*se.nighter
CI


## H0: p=50%,Ha: p<50% test left tail. I suppose the samples are random samples.
p0 <- 0.5

## Check conditions
p0*n.nighter
(1-p0)*n.nighter

SE.p0<-sqrt(p0*(1-p0)/n.nighter)

Zscore<-(phat-p0)/SE.p0

pnorm(Zscore,mean=0, sd=1)



## extra
library(tidyverse)
sleep_by_larkowl<-group_by(sleep,LarkOwl)
summarise(sleep_by_larkowl, AvgDAS=mean(DASScore))

dat.larkowl<-data.frame(summarise(sleep_by_larkowl, AvgDAS=mean(DASScore)))
dat.larkowl

ggplot(data=dat.larkowl, aes(x=LarkOwl,y=AvgDAS))+geom_col(color="white",fill=
                                                             "skyblue")+ggtitle("Average DASscore of LarkOwl")+scale_x_discrete(breaks=c("Lark","Neither","Owl"),labels=c("Early riser","Neither","night Owl"))




