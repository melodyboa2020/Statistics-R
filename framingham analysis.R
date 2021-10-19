library(tidyverse)
library(ggplot2)

## upload file
fm<-read.csv(file=file.choose(), header=TRUE)
head(fm)
summary(fm)

## Question: how does the shock index differ for those who aren't diabetic versus those who are?

#check if there are NAs in diabetes, heartRate& sysBP variables
sum(is.na(fm$diabetes)) 
sum(is.na(fm$heartRate))
sum(is.na(fm$sysBP)) 

## remove the row where NA is in heartRate, and save it as a new dataset
fm2<-subset(fm, !is.na(fm$heartRate))
summary(fm2)

##Create the variable shockIndex which is the person's heart rate divided by the person's systolic blood pressure for all people

fm2.shock<-mutate(fm2, shockIndex=heartRate/sysBP)
head(fm2.shock)
summary(fm2.shock$shockIndex)

## change the diabetes variable to categorical variable
str(fm2.shock$diabetes)
fm2.shock$diabetes<-as.factor(fm2.shock$diabetes)


## create a side-by-side boxplot 

ggplot(data=fm2.shock, aes(diabetes,shockIndex,fill=diabetes
                           ))+geom_boxplot(alpha=0.3)+ggtitle("ShockIndex in Diabetes categories")+scale_x_discrete(breaks=c("0","1"),
                                                                                                                    labels=c("Not diabetic","Diabetic"))+theme(legend.position="none")
 
                                                                                                                    
                                                                                                                                                                                                                               
## The difference in means

fm2.shock %>%
  group_by(diabetes) %>%
  summarise(avg_shockIndex=mean(shockIndex), sd_shockIndex=sd(shockIndex), n=length(shockIndex))

fm2.nodiabete<-subset(fm2.shock,fm2.shock$diabetes=="0")
fm2.diabete<-subset(fm2.shock,fm2.shock$diabetes=="1")

ggplot(data=fm.nodiabete, aes(x=shockIndex))+geom_histogram(bins=30, color="white", fill="skyblue", alpha=0.5)+ggtitle("The spread of shock index for those who aren't diabetic")
ggplot(data=fm.diabete, aes(x=shockIndex))+geom_histogram(bins=30, color="white", fill="skyblue", alpha=0.5)+ggtitle("The spread of shock index for those who are diabetic")


## Create confidence interval
xbar.nodiabete<-mean(fm2.nodiabete$shockIndex)
xbar.diabete<-mean(fm2.diabete$shockIndex)
diffmean<- xbar.nodiabete - xbar.diabete

#Bootstrap simulation 
b=10000 
boot.diffmeans<-c()

for (i in 1:b){
  boot.samp1<-sample(fm2.nodiabete$shockIndex,replace=TRUE)
  mean.samp1<-mean(boot.samp1)
  boot.samp2<-sample(fm2.diabete$shockIndex,replace = TRUE)
  mean.samp2<-mean(boot.samp2)
  boot.diffmean<-mean.samp1-mean.samp2
  boot.diffmeans<-c(boot.diffmean,boot.diffmeans)
}

hist(boot.diffmeans)

#95% CI
CI.lb = sort(boot.diffmeans)[250]
CI.ub = sort(boot.diffmeans)[9750]
c(CI.lb, CI.ub)

# t test for difference in Means

t.test(fm2.nodiabete$shockIndex, fm2.diabete$shockIndex, conf.level=0.95,alternative="two.sided")


# How the shock index differs for different cholesterol levels in women 

summary(fm2.shock$totChol)
fm.chol<-subset(fm2.shock, !is.na(fm2.shock$totChol))
fm.chol$gender<-as.factor(fm.chol$gender)
summary(fm.chol$gender)

## create the scatterplot for the totChol and shockindex in women

fm.fe<-subset(fm.chol, gender=="Female")
summary(fm.fe$totChol)
summary(fm.fe$shockIndex)

ggplot(data=fm.fe,mapping=aes(totChol,shockIndex))+geom_point(color="skyblue",fill="skyblue", alpha=0.7)+labs(x="Total cholesterol(mg/dL)",y="Shock Index", title="Total cholesterol and shock index in women")
plot1=ggplot(data=fm.fe, aes(x=totChol))+geom_histogram(bins=30, color="white", fill="skyblue", alpha=0.5)+labs(title="The spread of Total cholesterol in women",x="Total cholesterol(mg/dL)")
plot2=ggplot(data=fm.fe, aes(x=shockIndex))+geom_histogram(bins=30, color="white", fill="skyblue", alpha=0.5)+labs(title="The spread of shock index in women")
plot1
plot2

## Explore the shock index between nonsmokers and smokers
sum(is.na(fm.chol$currentSmoker))
fm.chol$currentSmoker<-as.factor(fm.chol$currentSmoker)

ggplot(data=fm.chol, aes(currentSmoker,shockIndex,fill=currentSmoker))+geom_boxplot(alpha=0.3)+ggtitle("ShockIndex in Smoker categories")+scale_x_discrete(breaks=c("0","1"),labels=c("Nonsmoker","Smoker"))+theme(legend.position="none")

fm.chol %>%
  group_by(currentSmoker)%>%
  summarise(n=length(shockIndex),avg_shockindex=mean(shockIndex),median_shockindex=median(shockIndex), sd_shockindex=sd(shockIndex),min(shockIndex), max(shockIndex))
  

