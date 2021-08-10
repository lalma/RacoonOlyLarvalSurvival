#KM survival curves

library(survival)
library(survminer)
library(ggplot2)
library(rms)
library(readxl)
setwd("C:/Users/Lindsay/Dropbox/Raccoon/larvae/oyster/larvae survival stats/GitHub/RacoonOlyLarvalSurvival/KM curves -single treatment-locatation")
OlyLarvaeKMforR <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/larvae/oyster/larvae survival stats/GitHub/RacoonOlyLarvalSurvival/OlyLarvaeKMforR.xlsx")

##CI20############
CI20<-subset(OlyLarvaeKMforR, Location=="CI20")

#make surv object
survCI20 <-Surv(time = CI20$day, CI20$dead, type = "right")

# fit model and plot without random effect
sfCI20 <- survfit(survCI20 ~ Treatment, data = CI20)
summary(coxph(survCI20 ~ Treatment, data = CI20))
ggsurvplot(sf, conf.int = TRUE)

#plot Km curves
#darker color=14c, lighter color=20c
ggsurvplot(sfCI20, data=CI20, conf.int=T,  risk.table=F, pval=F,legend=c("right"),
           legend.labs=c("14C","20C"),legend.title="Treatment", 
           palette =  c('darkgreen', '#33CC66'),   
           risk.table.height=.25,xlab="Time (days)", size=0.7, break.time.by = 3, break.y.by=.2, ggtheme = theme_bw() +  theme(
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),  
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank()
           ))


#COX
#cox model sample code with interaction, this is the model for the main analysis
coxCI20<-coxph(survCI20~Treatment, data=CI20)
coxCI20##<here results!!!, p<0.0001
summary(coxCI20)
#Concordance= 0.575  (se = 0.003 )
#Likelihood ratio test= 696.8  on 1 df,   p=<2e-16
#Wald test            = 687.9  on 1 df,   p=<2e-16
#Score (logrank) test = 708  on 1 df,   p=<2e-16
#Test no parametric (logrank test) p<0.0001 chisq=4053
survdiff(formula=Surv(day,dead)~Treatment, data=CI20)
#Chisq= 675  on 1 degrees of freedom, p= <2e-16 
surv_pvalue(sfCI20) 
anova(coxCI20)
#p<0.0001
cox.zph(coxCI20)
plot(cox.zph(coxCI20)) 
ggforest(coxCI20, data=CI20)
#sig dfference between temps at CI20 p<0.001 HR=0.54



#CI5########################

CI5<-subset(OlyLarvaeKMforR, Location=="CI5")

#make surv object
survCI5 <-Surv(time = CI5$day, CI5$dead, type = "right")

# fit model and plot without random effect
sfCI5 <- survfit(survCI5 ~ Treatment, data = CI5)
summary(coxph(survCI5 ~ Treatment, data = CI5))
ggsurvplot(sfCI5, conf.int = TRUE)

#plot Km curves
#darker color=14c, lighter color=20c
ggsurvplot(sfCI5, data=CI5, conf.int=T,  risk.table=F, pval=F,legend=c("right"),
           legend.labs=c("14C","20C"),legend.title="Treatment", 
           palette =  c('blue4', 'steelblue'),   
           risk.table.height=.25,xlab="Time (days)", size=0.7, break.time.by = 3, break.y.by=.2, ggtheme = theme_bw() +  theme(
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),  
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank()
           ))


#COX
#cox model sample code with interaction, this is the model for the main analysis
coxCI5<-coxph(survCI5~Treatment, data=CI5)
coxCI5##<here results!!!, p<0.0001
summary(coxCI5)
#Concordance= 0.575  (se = 0.003 )
#Likelihood ratio test= p=<2e-16
#Wald test            =   p=<2e-16
#Score (logrank) test = p=<2e-16
survdiff(formula=Surv(day,dead)~Treatment, data=CI5)
#Test non parametric (logrank test) p<0.0001 
# Chisq= 406  on 1 degrees of freedom, p= <2e-16 
surv_pvalue(sfCI5) 
anova(coxCI5)
#p<0.0001
cox.zph(coxCI5)
plot(cox.zph(coxCI5)) 
ggforest(coxCI5, data=CI5)
#sig dfference between temps at CI5 p<0.001 HR=0.62






#DB#############
##DB############
DB<-subset(OlyLarvaeKMforR, Location=="DB")

#make surv object
survDB <-Surv(time = DB$day, DB$dead, type = "right")

# fit model and plot without random effect
sfDB <- survfit(survDB ~ Treatment, data = DB)
summary(coxph(survDB ~ Treatment, data = DB))
ggsurvplot(sfDB, conf.int = TRUE)

#plot Km curves
#darker color=14c, lighter color=20c
ggsurvplot(sfDB, data=DB, conf.int=T,  risk.table=F, pval=F,legend=c("right"),
           legend.labs=c("14C","20C"),legend.title="Treatment", 
           palette =  c('darkred', 'red'),   
           risk.table.height=.25,xlab="Time (days)", size=0.7, break.time.by = 3, break.y.by=.2, ggtheme = theme_bw() +  theme(
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),  
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank()
           ))


#COX
#cox model sample code with interaction, this is the model for the main analysis
coxDB<-coxph(survDB~Treatment, data=DB)
coxDB##<here results!!!, p<0.0001
summary(coxDB)
#Concordance= 0.575  (se = 0.003 )
#Likelihood ratio test=   p=<2e-16
#Wald test            =  p=<2e-16
#Score (logrank) test =   p=<2e-16
#Test no parametric (logrank test) p<0.0001 chisq=4053
survdiff(formula=Surv(day,dead)~Treatment, data=DB)
#Chisq= 331  on 1 degrees of freedom, p= <2e-16 
surv_pvalue(sfDB) 
anova(coxDB)
#p<0.0001
cox.zph(coxDB)
plot(cox.zph(coxDB)) 
ggforest(coxDB, data=DB)
#sig dfference between temps at DB p<0.001 HR=0.59







##PW############
PW<-subset(OlyLarvaeKMforR, Location=="PW")

#make surv object
survPW <-Surv(time = PW$day, PW$dead, type = "right")

# fit model and plot without random effect
sfPW <- survfit(survPW ~ Treatment, data = PW)
summary(coxph(survPW ~ Treatment, data = PW))
ggsurvplot(sfPW, conf.int = TRUE)

#plot Km curves
#darker color=14c, lighter color=20c
ggsurvplot(sfPW, data=PW, conf.int=T,  risk.table=F, pval=F,legend=c("right"),
           legend.labs=c("14C","20C"),legend.title="Treatment", 
           palette =  c('darkgoldenrod', 'darkgoldenrod1'),   
           risk.table.height=.25,xlab="Time (days)", size=0.7, break.time.by = 3, break.y.by=.2, ggtheme = theme_bw() +  theme(
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),  
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank()
           ))


#COX
#cox model sample code with interaction, this is the model for the main analysis
coxPW<-coxph(survPW~Treatment, data=PW)
coxPW##<here results!!!, p<0.0001
summary(coxPW)
#Concordance= 0.575  (se = 0.003 )
#Likelihood ratio test= 696.8  on 1 df,   p=<2e-16
#Wald test            = 687.9  on 1 df,   p=<2e-16
#Score (logrank) test = 708  on 1 df,   p=<2e-16
#Test no parametric (logrank test) p<0.0001 chisq=4053
survdiff(formula=Surv(day,dead)~Treatment, data=PW)
#Chisq= 675  on 1 degrees of freedom, p= <2e-16 
surv_pvalue(sfPW) 
anova(coxPW)
#p<0.0001
cox.zph(coxPW)
plot(cox.zph(coxPW)) 
ggforest(coxPW, data=PW)
#sig dfference between temps at PW p<0.001 HR=0.62





#