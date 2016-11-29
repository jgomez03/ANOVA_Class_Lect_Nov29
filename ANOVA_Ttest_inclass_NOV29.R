#### t-tests and ANOVA

library(tidyverse)
my.data <- read_csv("drugData.csv")
my.data$Group <- as.factor(my.data$Group)
## turn grouping data into a factor, officially telling R something is categorical variable
##CRITICAL TO MAKE DATA A FACTOR WHEN DOING ANOVAS/TTEST


glimpse(my.data)


psych::describeBy(x=my.data$Arousal,group=my.data$Group)
#comparing group 1 average to group 2 average 

#can also take out 'By' and will get mean/sd for whole column, but we want it by group

my.data_grouped <- group_by(my.data, Group)
# going to ue summarise command next, and group by Group column - Group column has been flagged - saving into new data set
#in brackets - data is first, then what you want to group by

#my.data_grouped %>% summarise(M=mean(Arousal, SD=sd(Arousal))
#can name mean anything - M=mean, can change M to something else)                              
#the above doesnt handle missing data, so do:
my.data_grouped %>% summarise(M=mean(Arousal, na.rm=TRUE), 
                              SD=sd(Arousal, na.rm=TRUE))
#now summarizing every group in my.data and getting M and SD for every group


#run LEVENES TEST
car::leveneTest(my.data$Arousal, group=my.data$Group,center="median")
#levenes is non sig so variance is the same 
#car is a   PACKAGE, dont do library(car b/c conflicts w tidyverse)

#take orig data and run filter command and break those groups up - filter gives a subset of the rows (with ALL the columns)
exp.group.rows <- my.data %>% filter(Group==0)
control.group.rows <- my.data %>% filter(Group==1)
##create new sets of data (one for filtering only group 0 and one filtering only group 1)

#RUN TTEST 
t.test(x=exp.group.rows$Arousal, y=control.group.rows$Arousal,var.equal=TRUE)
#$ pulls out the column - dont use select 

###Notes on the above
## Two versions of the T-TEST - one is for homo variance and other is for not homo variance
# in R just put TRUE or FALSE, TRUE gives you first one and FALSE gives you the second - we use the first

 
#?ci.smd                            
smd(Group.1=exp.group.rows$Arousal, Group.2=control.group.rows$Arousal)
#Dvalue =  0.623664
#Get D value from article, can sub in numbers:
smd(Mean.1=3.2,s.1=.8, Mean.2=2.45, s.2=.91,n.1=10,n.2=10)
#=0.8753837

#get CI on D value
ci.smd(smd=0.8753837, n.1=10,n.2=10)
#$Lower.Conf.Limit.smd
# -0.05691424

#$smd
#0.8753837

#$Upper.Conf.Limit.smd
#1.785794


#### ONE WAY ANOVA
mdata <- read_csv("Viagra.csv")
mdata$dose <- as.factor(mdata$dose)
levels(mdata$dose) <- list("Placebo" =1,
                           "Low Dose" =2,
                           "High Dose" = 3)

##Do Levenes
car::leveneTest(mdata$libido,
                group=mdata$dose,center="median")
#test is not signifcant 
#Levene's Test for Homogeneity of Variance (center = "median")
#Df F value Pr(>F)
#group  2  0.1176   0.89
#12

#If using R and Levene's test is sig, run another command (in Field book)
options(contrasts = c("contr.sum", "contr.poly"))


oneway.results <- lm(libido~dose,data=mdata)

car::Anova(oneway.results,type=3)
library(apaTables)
#will give us CIs
apa.aov.table(oneway.results)

#Results
#ANOVA results using libido as the dependent variable
#Predictor     SS df     MS     F    p partial_eta2 CI_90_partial_eta2
#(Intercept) 180.27  1 180.27 91.66 .000                                
#dose  20.13  2  10.06  5.12 .025          .46         [.04, .62]
#Error  23.60 12   1.97   

#why use 90 CI instead of 95? - better congruence, can still be wrong 

apa.1way.table(iv=dose, dv=libido, data=mdata)
#descriptive statistics
#Descriptive statistics for libido as a function of dose.  
#dose    M   SD
#Placebo 2.20 1.30
#Low Dose 3.20 1.30
#High Dose 5.00 1.58
