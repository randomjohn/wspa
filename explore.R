library(ggplot2)
library(e1071)
library(dplyr)
library(tidyr)
library(Hmisc)
library(broom)

load("bond.RData")

# exploration of main_group -----------------------------------------------

main_group$ND_bin <- cut(main_group$Number.of.Days,c(-Inf,3,6,9,12,15,18,21,Inf))
qplot(ND_bin,data=main_group,geom="bar")
qplot(ND_bin,data=main_group,geom="bar",fill=Disposition.Result.Simple) + xlab("Number of days to post bail") + 
  scale_x_discrete(labels=c(paste((0:4)*3+1,"-",(1:5)*3),"16+","Missing")) +
  ylab("Number of prisoners")
unique(main_group$Race)
qplot(Race,data=main_group,geom="bar")
qplot(ND_bin,data=main_group,fill=Race)
qplot(Race,Number.of.Days,data=main_group %>% filter(Number.of.Days<100),geom="boxplot")
qplot(Race,log(Number.of.Days),data=main_group,geom="boxplot")

# define function for gg mosaic plot
ggMMplot <- function(var1, var2){
  require(ggplot2)
  levVar1 <- length(levels(var1))
  levVar2 <- length(levels(var2))
  
  jointTable <- prop.table(table(var1, var2))
  plotData <- as.data.frame(jointTable)
  plotData$marginVar1 <- prop.table(table(var1))
  plotData$var2Height <- plotData$Freq / plotData$marginVar1
  plotData$var1Center <- c(0, cumsum(plotData$marginVar1)[1:levVar1 -1]) +
    plotData$marginVar1 / 2
  
  ggplot(plotData, aes(var1Center, var2Height)) +
    geom_bar(stat = "identity", aes(width = marginVar1, fill = var2), col = "Black") +
    geom_text(aes(label = as.character(var1), x = var1Center, y = 1.05)) 
}
with(main_group,ggMMplot(Arrest.Officer,Race)) + xlab("Arresting Officer") + ylab("Race")

# histogram and density of days in jail before bond
qplot(Number.of.Days,data=main_group,geom="histogram")
qplot(log(Number.of.Days+1),data=main_group,geom="density")

skewness(main_group$Number.of.Days,na.rm=TRUE)
skewness(log(main_group$Number.of.Days),na.rm = TRUE)

with(main_group,t.test(log(Number.of.Days)~`Case Status`))
with(main_group,t.test(Number.of.Days~`Case Status`))
with(main_group[main_group$Number.of.Days<25,],t.test(Number.of.Days~Disposition.Result.Simple))
qplot(Disposition.Result.Simple,log(Number.of.Days),data=main_group,geom="boxplot")
qplot(Number.of.Days,data=main_group,colour=Disposition.Result.Simple,geom="density")

qplot(Number.of.Days,BOND.AMOUNT,data=main_group,geom="point",colour=Disposition.Result.Simple)
qplot(Number.of.Days,Disposition.Result.Simple,data=main_group,geom="point")


# exploration of bond_combined2 ------------------------------------------------

nd <- with(bond_combined2,Release.Date.1-Booked.Date.1)+1
bond_combined2$Number.of.Days-nd

qplot(log(Number.of.Days),data=bond_combined2,colour=Disposition.Simple,geom="density",facets = Offense~.)
qplot(Disposition.Simple,Number.of.Days,data=bond_combined2,geom="boxplot",facets = Offense~.) + ylim(0,500)

bond_summary <- bond_combined2 %>%
  filter(Bond.Amount>0 & !is.na(Bond.Amount)) %>% 
  group_by(Offense,Disposition.Simple,Bond.Paid) %>% 
  summarise(dMedian=median(Number.of.Days),` N`=n(),aMean=mean(Number.of.Days),
            bSD=sd(Number.of.Days),cQ1=quantile(Number.of.Days,.25),eQ3=quantile(Number.of.Days,.75)) %>% 
  gather(Statistic,Value,4:9) %>% 
  spread(Disposition.Simple,Value) %>% 
  mutate(ord=dense_rank(Statistic),Statistic=substring(Statistic,2))
View(bond_summary)

qplot(Age,Number.of.Days,data=bond_combined2,colour=Disposition.Simple,geom=c("point","smooth"),facets=Offense~Bond.Paid) + ylim(0,600)

qplot(Number.of.Days,Bond.Amount,data=bond_combined2,colour=Disposition.Simple,geom=c("point"),facets=Offense~.)+xlim(0,500)

qplot(Public.Defender,log(Number.of.Days),data=bond_combined2,geom="boxplot",facets=Offense~.)
qplot(Public.Defender,Bond.Amount,data=bond_combined2,geom="boxplot",facets=Offense~.) # no relationship here

# public defender vs. disposition
pd_tbl <- by(bond_combined2,bond_combined2$Offense,function(x) with(x,table(Public.Defender,`Case Status`)))
print(pd_tbl)
lapply(pd_tbl,prop.table,margin=1)
lapply(pd_tbl,chisq.test)

# public defender vs. number o fdays
bond_combined2 %>% 
  group_by(Offense,Public_Defender) %>% 
  summarise(N=n(),Mean=mean(Number.of.Days),SD=sd(Number.of.Days))
  

# %age guilty by race
bond_combined2 %>% 
  group_by(Offense,Race) %>% 
  summarise(charges=n(),guilty=sum(Disposition.Simple=="Guilty")) %>% 
  mutate(percent_guilty=guilty/charges*100) %T>% print %>%
  ggplot(aes(x=Race,y=percent_guilty)) + geom_bar(stat="identity") + facet_wrap(~Offense)

# %age guilty by number of days
bond_combined2 %>% 
  filter(Year==2013) %>% 
  mutate(nd_group=cut2(Number.of.Days,g=8)) %>% 
  group_by(Offense,nd_group) %>% 
  summarise(charges=n(),guilty=sum(Disposition.Simple=="Guilty")) %>% 
  mutate(percent_guilty=guilty/charges*100) %T>% print(n=40) %>%
  ggplot(aes(x=nd_group,y=percent_guilty)) + geom_bar(stat="identity") + facet_wrap(~Offense)

# testing by group --------------------------------------------------------

tests <- by(bond_combined2,bond_combined2$Offense,function(x) with(x,t.test(log(Number.of.Days)~Disposition.Simple))) %>% 
sapply(tidy) %>% as.data.frame

tests_orig <- tests
tests_orig[c(1:3,7:8),] <- sapply(tests[c(1:3,7:8),],function(x) sapply(x,exp))
tests
