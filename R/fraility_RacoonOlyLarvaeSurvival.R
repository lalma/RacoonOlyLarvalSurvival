#Load libraries.
library(survminer)
library(ggplot2)
library(survival)
library(frailtyEM)
library(tidyverse)
library(here)
library(readxl)


# Select dataset, import my dataset here, has the same headings as pauls dataset for simplicity
OlyLarvaeKMforR <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/larvae/oyster/larvae survival stats/GitHub/RacoonOlyLarvalSurvival/OlyLarvaeKMforR.xlsx")#  dataset is d
d <- OlyLarvaeKMforR
View(d)

#make surv object
surv = Surv(time = d$day, d$dead, type = "right")

# coxph
# fit model and plot without random effect
sf <- survfit(surv ~ Treatment+Location, data = d)
summary(coxph(surv ~ Treatment+Location, data = d))

#basic KM plot
ggsurvplot(sf, conf.int = TRUE)

#Plot KM curves- will prob use this one for paper. Use code from R file RaccoonLarvaeSurvialCurves. contains all data
#if we use the subset with 2000 lines, the confidence interval is much fatter
ggsurvplot(sf, conf.int=T, risk.table=F, pval=F,legend=c("right"),
           legend.labs=c("CI20-14C","CI5-14C","DB-14C","PW-14C", "CI20-20C", "CI5-20C", "DB-20C", "PW-20C"),legend.title="Treatment", 
           palette =  c('darkgreen', 'blue4', 'darkred', 'darkgoldenrod', '#33CC66','steelblue','red','darkgoldenrod1'),   
           xlab="Time (days)", size=0.7, break.time.by = 3,  ggtheme = theme_bw() +  theme(
             panel.grid.major.y = element_blank(),
             panel.grid.minor.y = element_blank(),  
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank()
           ))

#Best frailty distribution
#find the frailty distribution with the maximium likelihood
frailtyDist <- data.frame(name = c("gamma", "stablePos", "invGausian", "nonCenGamma"),
                          dist = c("gamma", "stable", "pvf", "pvf"), m = c(0, 0, -0.5, 1))
dLike <- NULL

#here is where code runs a long time.Advice from Paul--
#1. take it out of the loop so you are just running one distribution at a time. Then combine them at the end to find the best.
#removed for loop
#2. mess with the em_control options in the emfrail_control option of the emfrail function. It looks like there is a fast_fit option that may help and you can control the umber of interactions and tolerance of the algorithm.
#emfrail_control=(fast_fit = TRUE)
#3. Maybe try bootstrapping your data so you are working with a smaller data set for each fit. It would probably be a good idea to  just try a random subset of the data to start, just to test that the model is set up OK. That will also give you and idea of how long you might expect it to take to run your entire data set (if if takes x minutes to run 400 datapoints it will take y hours to to run 38,400 ...)
#subsample 2000 lines at random, no replacement 

### bootstrap small subsample, 2000 lines from 38400
d <- OlyLarvaeKMforR
d2000<-d[sample(nrow(d), 2000), ]
#  dataset is d
d <- d2000


#added location as a second variable in addition to temp. cluster(group) tests random effect from replicates 

#running first dist- name=nonCenGamma, dist=pvf, m=1, start 3:00 pm 7-14-21, found finished next morning 9:30 AM
mLogLike <- emfrail(surv ~ Treatment + Location+ cluster(Group), 
                    distribution = emfrail_dist(dist="pvf", pvfm = 1),
                    emfrail_control=(fast_fit = TRUE),data = d)$loglik[2]
dTemp <- data.frame(dist = "nonCenGamma", logLike = mLogLike)
dLike <- rbind(dLike, dTemp)
View(dLike) #dist	logLike = nonCenGamma
#mLogLike=-9504.3757472149
summary(mLogLike)

#running second dist- name=invGausian, dist=pvf, m=-0.05, start 2:30 pm 7-15-21, was finished by morning 7-16-21 at 10am
mLogLikeII <- emfrail(surv ~ Treatment + Location+ cluster(Group), 
                    distribution = emfrail_dist(dist="pvf", pvfm = -0.05),
                    emfrail_control=(fast_fit = TRUE),data = d)$loglik[2]
dTempII <- data.frame(dist = "invGausian", logLike = mLogLikeII)
View(dTempII)
dLikeII <- NULL
dLikeII <- rbind(dLikeII, dTempII)
#mLogLikeII=-9504.3821743224
summary(mLogLikeII)


#running third dist- name=stablePos, dist=stable, m=0, start 4:00 pm 7-16-21 finish 7:30
mLogLikeIII <- emfrail(surv ~ Treatment + Location+ cluster(Group), 
                      distribution = emfrail_dist(dist="stable", pvfm = 0),
                      emfrail_control=(fast_fit = TRUE),data = d)$loglik[2]
dTempIII <- data.frame(dist = "stablePos", logLike = mLogLikeIII)
View(dTempIII)
dLikeIII <- NULL
dLikeIII <- rbind(dLikeIII, dTempIII)
#mLogLikeIII=-9504.87408829784
summary(mLogLikeIII)


#running fourth dist- name=gamma, dist=gamma, m=0, start 7:30pm pm 7-16-21 
mLogLikeIIII <- emfrail(surv ~ Treatment + Location+ cluster(Group), 
                       distribution = emfrail_dist(dist="gamma", pvfm = 0),
                       emfrail_control=(fast_fit = TRUE),data = d)$loglik[2]
dTempIIII <- data.frame(dist = "gamma", logLike = mLogLikeIIII)
View(dTempIIII)
dLikeIIII <- NULL
dLikeIIII <- rbind(dLikeIIII, dTempIIII)
#mLogLike=-9504.38193559969
summary(mLogLikeIIII)

#indentify the model frailty distribution with the maxium likelihood
#since forloop was not used, created dataset with all distributions and their logLike values to enter into bestDist
dLike <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/larvae/oyster/larvae survival stats/GitHub/RacoonOlyLarvalSurvival/dLike.xlsx")
bestDist <- dLike$dist[dLike$logLike == max(dLike$logLike)]
#best fit was nonCenGamma

# fit model with Group random effect using best fit frailty distribu1tion
#will take a while to run- start 4:15 7-21-21, finish by morning 7-21-21
fme <- emfrail(surv ~ Treatment + Location+ cluster(Group), 
               distribution = emfrail_dist(dist = frailtyDist$dist[frailtyDist$name == bestDist], 
  
  #fme <- emfrail(surv ~ Treatment + cluster(Group), 
 # distribution = emfrail_dist(dist = frailtyDist$dist[frailtyDist$name == bestDist],
  #                            pvfm = frailtyDist$m[frailtyDist$name == bestDist] ),
  #data = d)                                         
                                                                                    pvfm = frailtyDist$m[frailtyDist$name == bestDist] ),
#Both the Commenges-Andersen test for heterogeneity and the one-sided likelihood ratio test deems the random effect highly significant.
#The Commenges-Andersen score test for heterogeneity (Commenges and Andersen 1995) is implemented in frailtyEM.
summary(fme)
#Score test for heterogeneity: p-val 0.335
#Commenges-Andersen test for heterogeneity: p-val  0.335 
#If the null hypothesis of no unobserved heterogeneity is not rejected, it might be preferable to employ simpler Cox-type models.
#LRT: 1/2 * pchisq(0.991), p-val 0.16
#which to report in paper?

#frailty plot - shows variation in frailty terms
autoplot(fme, type = "frail")
#By default, the predict function creates predictions for each row of newdata or for each value of autopiolt separately
newdata = data.frame(Treatment= unique(d$Treatment), Location = unique(d$Location))

#new data of treatments for survival plots
dNew <- data.frame(Treatment = unique(d$Treatment))

#made a new matrix called dNew, which has the unique treatments and locations. 
#not sure if this is the correct format. need help here.
#this data sheet has one row called Treatment and one row called Location, 8 rows, shows all possible combos
dNew <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/larvae/oyster/larvae survival stats/GitHub/RacoonOlyLarvalSurvival/dNew.xlsx")

#predictions (survival probabilities) of treatments 
pred <- predict(fme, newdata = dNew, conf_int = "adjusted")

#create a data frame of the prections for plotting all the treatment curves
#changed from dTemp to dLike which has all of the distribution outcomes
#need help here. not sure hot to integrate both response veriables into this loop
dPlot <- NULL
for(i in 1:length(pred)){
  dTemp <- pred[[i]] %>%
    mutate(Treatment = unique(d$Treatment)[i])
  dPlot <- rbind(dPlot, dTemp)
}

View(dPlot)
# plot curve
ggplot(dPlot, aes(time, survival_m)) +
  geom_step(aes(color = Treatment)) +
  ylim(0,1) +
  geom_blank()
