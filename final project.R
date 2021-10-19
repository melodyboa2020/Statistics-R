# loading the file
load("E:/_2. University of St. Thomas/_02 Spring 2021 semester/SEIS 631 Foundations of Data Analysis/_Module 15 Semester review and final project/Final project Part I - Data Analysis/loans_subset.rda")

library(tidyverse)
library(ggplot2)

# go through the file
View(loans_subset)
str(loans_subset)

# Research question: Is there a difference in the average interest rate on a loan based on a borrower's home-ownership status?
# focusing on 2 variables: "homeownership" (categorical) and "interest_rate"(numerical)


str(loans_subset$homeownership)
loans<-filter(loans_subset, homeownership %in% c("MORTGAGE", "OWN", "RENT"))

loans$homeownership<-factor(loans$homeownership)
str(loans$homeownership)

summary(loans$interest_rate)
sum(is.na(loans$interest_rate))

# look at the summary for interest rate in each category of homeownership
loans %>%
  group_by(homeownership) %>%
  summarize(avg_InterestRate=mean(interest_rate), Median_InterestRate=median(interest_rate),sd_InterestRate=sd(interest_rate), n=length(interest_rate))

# subset each category of homeownership
loans.M<- filter(loans, homeownership=="MORTGAGE")
loans.O<- filter(loans, homeownership=="OWN")
loans.R<- filter(loans, homeownership=="RENT")

# plot the graphical display of interest rate for each subset

ggplot(loans.M, aes(interest_rate))+geom_histogram(fill="steelblue3",
                                                   color="white",bins=20,alpha=0.7)+labs(x="Interest Rate", 
                                                                                         y="Frequency",title="The spread of interest rate for whose home is with mortgage")

ggplot(loans.O, aes(interest_rate))+geom_histogram(fill="steelblue3",
                                                   color="white",bins=20,alpha=0.7)+labs(x="Interest Rate", 
                                                                                         y="Frequency",title="The spread of interest rate for who is a home-owner")


ggplot(loans.R, aes(interest_rate))+geom_histogram(fill="steelblue3",
                                                   color="white",bins=20,alpha=0.7)+labs(x="Interest Rate", 
                                                                                         y="Frequency",title="The spread of interest rate for who rent a house")

# plot the side-by-side boxplot 
ggplot(loans, aes(homeownership, interest_rate, fill=homeownership))+geom_boxplot(alpha=0.7)+scale_fill_brewer(palette="BuPu")+theme(legend.position="none")+labs(title="The status of home-ownership VS the spread of interest rate",x="Status of home's ownership", y="Interest Rate")

#ANOVA test
results<-aov(interest_rate ~ homeownership, data=loans)
summary(results)

# pairwise t-test
pairwise.t.test(loans$interest_rate, loans$homeownership, p.adj="bonferroni")






