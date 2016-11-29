####CRF ANOVA (aka multiway ANOVA - completely randomized factorial)

library(tidyverse) 
crf.data<-read_csv("crfData.csv")


# set up using factors!!!
crf.data$anxiety <- as.factor(crf.data$anxiety) 
crf.data$preparation <- as.factor(crf.data$preparation)

levels(crf.data$anxiety) <- list("Low Anxiety"=1, 
                                 "High Anxiety"=2) 

levels(crf.data$preparation) <- list("Low Preparation"=1,
                                     "Medium Preparation"=2,
                                     "High Preparation"=3)

# Setting contrasts
options(contrasts = c("contr.sum", "contr.poly"))

##run the analysis
crf.lm <- lm(mark ~ anxiety * preparation, data=crf.data)

### seeing if there is an INTERACTION

##two options, car package or apa tables
#car package:
car::Anova(crf.lm, type=3)
#apatables
library(apaTables)
apa.aov.table(crf.lm) #will figure for you whether it one way of two way

#ANOVA results using mark as the dependent variable
#Predictor        SS        df        MS       F     p       partial_eta2  CI_90_partial_eta2
#(Intercept)      132933.63  1 132933.63 2215.56    .000                                
#anxiety         3477.63     1   3477.63   57.96    .000      .71         [.50, .79]
#preparation      434.47     2    217.24    3.62    .042      .23         [.00, .40]
#anxiety x 
#preparation      539.27     2    269.63    4.49    .022      .27         [.02, .44]
#Error            1440.00    24    60.00      

apa.aov.table(crf.lm,filename="Table1.doc")
#gives you effect size plus CIs
#there is main effect of prep, main effect of anxiety, and interaction

##NEXT STEP IS SIMPLE MAIN EFFECTS (B/C THERE IS AN INTERACTION)
#make means table
#when making the means, need to specify oneway or twoway
apa.2way.table(iv1=preparation, iv2=anxiety, dv=mark, data=crf.data,
               show.marginal.means = TRUE, filename="Table2.doc")

#Means and standard deviations for mark as a function of a 3(preparation) X 2(anxiety) design 
#anxiety                                        
#Low Anxiety       High Anxiety       Marginal      
#preparation         M    SD            M    SD        M    SD
#Low Preparation    71.40  5.50        59.40  3.13    65.40  7.60
#Medium Preparation 72.60  8.88        52.60  7.64    62.60 13.12
#High Preparation   88.00  8.15        55.40 10.78    71.70 19.40
#Marginal           77.33 10.55        55.80  7.81

#one way vs two way referes to # of independant variables (e.g., anxiety/prep or just drugs)

#phia package runs simple main effects
##bonferroni = post hoc test
library(phia)
#simple main effects
testInteractions(crf.lm, fixed="anxiety",
                 across="preparation",
                 adjustment="none")

#F Test: 
#P-value adjustment method: none
#             preparation1 preparation2  Df   Sum of Sq    F        Pr(>F)   
#Low Anxiety        -16.6        -15.4   2    856.93       7.1411  0.003686 **
#High Anxiety         4.0         -2.8   2    116.80       0.9733  0.392244   
#Residuals                               24   1440.00                   

#effect size is not on output (not in apa tables yet), so calculate by hand
#formula = 856.93/856.93 + 1440)

#get confidence intervals 
get.ci.partial.eta.squared(F.value=7.1411, df1=2, df2=24, conf.level = .90)

get.ci.partial.eta.squared(F.value=0.9733, df1=2, df2=24, conf.level = .90)

##report p values to .3 decimals (unless less than .001, then just put less than)

###run Bonferroni correction - need to add in to script when doing activity: see below 
##correction for paired comparisions 
testInteractions(crf.lm, fixed="anxiety", 
                 pairwise="preparation", 
                 adjustment="bonferroni")  

