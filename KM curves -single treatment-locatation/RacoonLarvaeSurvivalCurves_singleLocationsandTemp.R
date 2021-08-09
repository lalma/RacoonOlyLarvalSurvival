#kaplan meier survival

install.packages("survival")
installed.packages("survminer")
install.packages("rms")

library(survival)
library(survminer)
library(ggplot2)
library(rms)

##CI20############
library(readxl)



# day dead in one column 
Funcio <- with(OlyLarvaeKMforR_CI20, Surv(DayDead,Status))
View(Funcio)

# Graph the survival distribution of totes you show them sense to have in compte tractament
# Kaplan-Meier estimator 
Survival0 <- survfit(Funcio~1, data=OlyLarvaeKMforR_CI20)
summary(Survival0)

##Kaplan-Meier: comparem la distribucio de supervivencia en els diferents treatment (Natural-Advanced)
Survival = survfit(Funcio~TempAsFactor+Location, data=OlyLarvaeKMforR_CI20)
summary(Survival)

#another type of graph

ggsurvplot(Survival, data=OlyLarvaeKMforR_CI20, conf.int=TRUE, conf.int.alpha=0.1, pval=TRUE,legend=c("right"),
           legend.labs=c("CI20-14C","CI20-20C"),legend.title="Treatment", 
           palette = c("springgreen4", "springgreen2"),   
           risk.table.height=.25,xlab="Larval Age (days)")

#cox model sample code with interaction, this is the model for the main analysis
cox<-coxph(Funcio~TempAsFactor, data=OlyLarvaeKMforR_CI20)
cox##<here results!!!
summary(cox)



##CI5############
library(readxl)
OlyLarvaeKMforR_CI5 <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/larvae/OlyLarvaeKMforR_CI5.xlsx")
View(OlyLarvaeKMforR_CI5)


# day dead in one column 
Funcio <- with(OlyLarvaeKMforR_CI5, Surv(DayDead,Status))
View(Funcio)

# Graph the survival distribution of totes you show them sense to have in compte tractament
# Kaplan-Meier estimator 
Survival0 <- survfit(Funcio~1, data=OlyLarvaeKMforR_CI5)
summary(Survival0)

##Kaplan-Meier: comparem la distribucio de supervivencia en els diferents treatment (Natural-Advanced)
Survival = survfit(Funcio~TempAsFactor+Location, data=OlyLarvaeKMforR_CI5)
summary(Survival)

#another type of graph

ggsurvplot(Survival, data=OlyLarvaeKMforR_CI5, conf.int=TRUE, conf.int.alpha=0.1, pval=TRUE,legend=c("right"),
           legend.labs=c("CI5-14C","CI5-20C"),legend.title="Treatment", 
           palette = c("royalblue3", "dodgerblue"),   
           risk.table.height=.25,xlab="Larval Age (days)")

#cox model sample code with interaction, this is the model for the main analysis
cox<-coxph(Funcio~TempAsFactor, data=OlyLarvaeKMforR_CI5)
cox##<here results!!!
summary(cox)




##DB############
library(readxl)
OlyLarvaeKMforR_DB <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/larvae/OlyLarvaeKMforR_DB.xlsx")
View(OlyLarvaeKMforR_DB)


# day dead in one column 
Funcio <- with(OlyLarvaeKMforR_DB, Surv(DayDead,Status))
View(Funcio)

# Graph the survival distribution of totes you show them sense to have in compte tractament
# Kaplan-Meier estimator 
Survival0 <- survfit(Funcio~1, data=OlyLarvaeKMforR_DB)
summary(Survival0)

##Kaplan-Meier: comparem la distribucio de supervivencia en els diferents treatment (Natural-Advanced)
Survival = survfit(Funcio~TempAsFactor+Location, data=OlyLarvaeKMforR_DB)
summary(Survival)

#another type of graph

ggsurvplot(Survival, data=OlyLarvaeKMforR_DB, conf.int=TRUE, conf.int.alpha=0.1, pval=TRUE,legend=c("right"),
           legend.labs=c("DB-14C","DB-20C"),legend.title="Treatment", 
           palette = c( "red","indianred"),   
           risk.table.height=.25,xlab="Larval Age (days)")



#cox model sample code with interaction, this is the model for the main analysis
cox<-coxph(Funcio~TempAsFactor, data=OlyLarvaeKMforR_DB)
cox##<here results!!!
summary(cox)


##PW############
library(readxl)
OlyLarvaeKMforR_PW <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/larvae/OlyLarvaeKMforR_PW.xlsx")
View(OlyLarvaeKMforR_PW)


# day dead in one column 
Funcio <- with(OlyLarvaeKMforR_PW, Surv(DayDead,Status))
View(Funcio)

# Graph the survival distribution of totes you show them sense to have in compte tractament
# Kaplan-Meier estimator 
Survival0 <- survfit(Funcio~1, data=OlyLarvaeKMforR_PW)
summary(Survival0)

##Kaplan-Meier: comparem la distribucio de supervivencia en els diferents treatment (Natural-Advanced)
Survival = survfit(Funcio~TempAsFactor+Location, data=OlyLarvaeKMforR_PW)
summary(Survival)

#another type of graph

ggsurvplot(Survival, data=OlyLarvaeKMforR_PW, conf.int=TRUE, conf.int.alpha=0.1, pval=TRUE,legend=c("right"),
           legend.labs=c("PW-14C","PW-20C"),legend.title="Treatment", 
           palette = c("orange2","gold1"),   
           risk.table.height=.25,xlab="Larval Age (days)")


#cox model sample code with interaction, this is the model for the main analysis
cox<-coxph(Funcio~TempAsFactor, data=OlyLarvaeKMforR_PW)
cox##<here results!!!
summary(cox)


##20C############
library(readxl)
OlyLarvaeKMforR_20C <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/larvae/OlyLarvaeKMforR_20C.xlsx")
View(OlyLarvaeKMforR_20C)


# day dead in one column 
Funcio <- with(OlyLarvaeKMforR_20C, Surv(DayDead,Status))
View(Funcio)

# Graph the survival distribution of totes you show them sense to have in compte tractament
# Kaplan-Meier estimator 
Survival0 <- survfit(Funcio~1, data=OlyLarvaeKMforR_20C)
summary(Survival0)

##Kaplan-Meier: comparem la distribucio de supervivencia en els diferents treatment (Natural-Advanced)
Survival = survfit(Funcio~TempAsFactor+Location, data=OlyLarvaeKMforR_20C)
summary(Survival)

#another type of graph

ggsurvplot(Survival, data=OlyLarvaeKMforR_20C, conf.int=TRUE, conf.int.alpha=0.1, pval=TRUE,legend=c("right"),
           legend.labs=c("CI20-20C","CI5-20C", "DB20C", "PW20C"),legend.title="Treatment", 
           palette = c("springgreen2","dodgerblue","indianred","gold1"),   
           risk.table.height=.25,xlab="Larval Age (days)") 



#cox model sample code with interaction, this is the model for the main analysis
cox<-coxph(Funcio~Location, data=OlyLarvaeKMforR_20C)
cox##<here results!!!
summary(cox)



###14C###
library(readxl)
OlyLarvaeKMforR_14C <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/larvae/OlyLarvaeKMforR_14C.xlsx")
View(OlyLarvaeKMforR_14C)


# day dead in one column 
Funcio <- with(OlyLarvaeKMforR_14C, Surv(DayDead,Status))
View(Funcio)

# Graph the survival distribution of totes you show them sense to have in compte tractament
# Kaplan-Meier estimator 
Survival0 <- survfit(Funcio~1, data=OlyLarvaeKMforR_14C)
summary(Survival0)

##Kaplan-Meier: comparem la distribucio de supervivencia en els diferents treatment (Natural-Advanced)
Survival = survfit(Funcio~TempAsFactor+Location, data=OlyLarvaeKMforR_14C)
summary(Survival)

#another type of graph

ggsurvplot(Survival, data=OlyLarvaeKMforR_14C, conf.int=TRUE, conf.int.alpha=0.1, pval=TRUE,legend=c("right"),
           legend.labs=c("CI20-14C","CI5-14C", "DB14C", "PW14C"),legend.title="Treatment", 
           palette = c("springgreen4", "royalblue3", "red","orange2"),   
           risk.table.height=.25,xlab="Larval Age (days)")

#cox model sample code with interaction, this is the model for the main analysis
cox<-coxph(Funcio~Location, data=OlyLarvaeKMforR_14C)
cox##<here results!!!
summary(cox)



