data(ToothGrowth)
ToothGrowth
library(ggplot2)
## if the base len equal = 0 
ToothGrowth$baseLen<-0
ToothGrowth$id<-1:60
Too<-melt(ToothGrowth,id=c("supp","dose","id"),measure.vars = c("baseLen","len"))


ggplot(data=Too,aes(y=value,x=variable,group=value))+facet_grid(cols = vars(dose),rows=vars(supp))+
  geom_point()+stat_summary(aes(group=1),geom="line",fun.y=mean,size=1,col="red")+
  labs(y="len",x="time")
#mean(subset(ToothGrowth,supp %in% "OJ" & dose %in% 0.5)$len)
#subset(ToothGrowth,supp %in% "OJ" & dose %in% 0.5)

## ahora que estoy haciendo , voy a hacer prubas mùltiples
# voy a hacer pruebas separando por supp
TooSepForSupp<- split(ToothGrowth,ToothGrowth$supp)
# hipòtesis con VC
names(TooSepForSupp$VC)
v<-vector("numeric",3)
names(v)<-c("0.5","1","2")

# I begin hypothesis testing for the VC supplement
# u1 = mean the 0.5 and u2 = mean the 1
# ho: u1=u2 ha: u2!=u1
# alpha = 0.05 
alpha<- 0.05
g1<- subset(TooSepForSupp$VC, dose %in% 0.5 )$len
g2<- subset(TooSepForSupp$VC, dose %in% 1)$len
u1 <-mean(g1)
n1<- length(g1)
u2<- mean(g2)
n2<- length(g2)
s1<- sd(g1)
s2<- sd(g2)
df<- (((s1^2)/n1 + (s2^2)/n2 )^2)/((((s1^2)/n1)^2)/(n1-1)  + (((s2^2)/n2)^2/(n2-1)))
mn<- u1-u2

#testGreater<-t.test(g2,g1,var.equal = FALSE,alternative = "greater")
#testLess<-t.test(g2,g1,var.equal = FALSE,alternative = "less")
testBina<-t.test(g1,g2,paires=FALSE,var.equal = FALSE)
est<-mn/sqrt(((s1^2)/n1)+((s2^2)/n2))
testBina$p.value/2<alpha/2
pt(est,df)<alpha/2
#I reject the null hypothesis so I use the t statistic to know which of the 2 groups I am going to compare with now.

testBina$statistic
est
#Al ser el estadístico negativo,  por el orden de los grupos Y-X. Entonces, el grupo mayor o la media mayor en este caso es X=g2 
g1<- g2
g2<- subset(TooSepForSupp$VC, dose %in% 2 )$len

testBina<- t.test(g1,g2,paired= FALSE,var.equal = FALSE)

testBina$p.value/2 < alpha/2

#Again my p.values are below my alpha value so again the null hypothesis is rejected.

testBina$statistic

# the statistic is negative so my choice of the largest group is X=g2.
# In conclusion, the teeth for the VC supplement grew more with 2 doses


##
# I begin hypothesis testing for the OJ supplement
# u1 = mean the 0.5 and u2 = mean the 1
# ho: u1=u2 ha: u2!=u1
# alpha = 0.05 
alpha<- 0.05
g1<- subset(TooSepForSupp$OJ, dose %in% 0.5 )$len
g2<- subset(TooSepForSupp$OJ, dose %in% 1)$len

u1 <-mean(g1)
n1<- length(g1)
u2<- mean(g2)
n2<- length(g2)
s1<- sd(g1)
s2<- sd(g2)
df<- (((s1^2)/n1 + (s2^2)/n2 )^2)/((((s1^2)/n1)^2)/(n1-1)  + (((s2^2)/n2)^2/(n2-1)))
mn<- u1-u2

est<-mn/sqrt(((s1^2)/n1)+((s2^2)/n2))
prob<-pt(est,df)
prob<alpha/2

# The value for my test is less than the established alpha, so the difference between the 2 means is large enough to consider that the null hypothesis should be rejected.
# I use the t statistic to know which group to choose to continue comparing.
est
# Since the statistic is negative that means that g2 is larger than g1 so I choose g2


# u1 = mean the 1 and u2 = mean the 2
# ho: u1=u2 ha: u2!=u1
# alpha = 0.05 

g1<- g2
g2<- subset(TooSepForSupp$OJ, dose %in% 2 )$len

testBina<- t.test(g1,g2,paired= FALSE,var.equal = FALSE)

testBina$p.value/2 < alpha/2

#Again my p.values are below my alpha value so again the null hypothesis is rejected.

testBina$statistic

#I choose group 2, so in conclusion, for the OJ supplement, the dose that made the teeth grow the most is 2 doses.


### Identify the dose that makes teeth grow the most in both supplements, the next thing is to obtain the supplement with the greatest growth

# u1 = average of the 2nd dose with VC supplement  and u2 = average of the 2nd dose with OJ supplement
# ho: u1=u2 ha: u2!=u1
# alpha = 0.05 

g1<-subset(TooSepForSupp$VC, dose %in% 2)$len
g2<-subset(TooSepForSupp$OJ, dose %in% 2)$len

# I'm going to use resampling
g1
n1<- length(g1)
n2<- length(g2)
nsim<- 10000


g1M<- matrix(sample(g1,nsim*n1,replace = TRUE),nsim,n1)
g2M<- matrix(sample(g2,nsim*n2,replace= TRUE),nsim,n2)
meanG1V<- apply(g1M,1,mean)
meanG2V<- apply(g2M,1,mean)
sdG1V<- apply(g1M,1,sd)
sdG2V<- apply(g2M,1,sd)
u1<- mean(meanG1V)
u2<- mean(meanG2V)
s1<- mean(sdG1V)
s2<- mean(sdG2V)
df<- (((s1^2)/n1 + (s2^2)/n2 )^2)/((((s1^2)/n1)^2)/(n1-1)  + (((s2^2)/n2)^2/(n2-1)))

#
u1 <-mean(g1)
n1<- length(g1)
u2<- mean(g2)
n2<- length(g2)
s1<- sd(g1)
s2<- sd(g2)
#

est<- (u1-u2)/sqrt((s1^2)/n1 + (s2^2)/n2)
prob<-pt(est,df,lower.tail = FALSE)
prob<alpha
prob<alpha/2


#Due to the result obtained with the hypothesis test, the p-value is not less than alpha/2 and not even less than alpha. It is concluded that between the VC and OJ supplement groups with 2 doses there is not a difference notable enough to obtain a supplement that more favors tooth growth. Then the null hypothesis is not rejected.


# 
# v
# m<-mean(TooSepForSupp$VC[TooSepForSupp$VC$dose==0.5,1])
# s<-sd(TooSepForSupp$VC[TooSepForSupp$VC$dose==0.5,1])
# est<-m/(s/sqrt(length(TooSepForSupp$VC[TooSepForSupp$VC$dose==0.5,1])))
# TooSepForSupp$VC[,3] %in% 0.5
# est
# t.test(TooSepForSupp$VC[TooSepForSupp$VC$dose==0.5,1])
# 2*pt(est,length(TooSepForSupp$VC[TooSepForSupp$VC$dose==0.5,1])-1,lower.tail = FALSE)
