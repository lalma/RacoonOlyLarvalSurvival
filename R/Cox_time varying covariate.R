library(survival)
library(survminer)

library(readxl)
SURV <- read_excel("C:/Users/Lindsay/Desktop/SURV.xlsx")
View(SURV)
#show time points in time at which an individual died
cut.points <- unique(SURV$time[SURV$death == 1])
cut.points

#duplicate 4 line per individual, times 0-1, 1-7, 7-10, 10-13 =time0 and time
SURV2 <- survSplit(data = SURV, cut = cut.points, end = "time", start = "time0", event = "death")
View(SURV2)

##stop here, this data is what you need


#run the cox model on orginal data- estimates one treatment male-21yrs
model.1 <- coxph(Surv(DayDead, Status) ~ TempAsFactor+Location, data = OlyLarvaeKMforR)
covs <- data.frame(Location = "CI20", TempAsFactor = "14C")
covs
summary(survfit(model.1, newdata = covs, type = "aalen"))
#output gives us probability of survival for CI20 14C for each day, i.e. a larvae has a .708 chance of survivind to day 4, but 0.163 chance of surviving to day 15

model.2<-coxph(Surv(time0, time, death))

#accomidate time varying coeffiecients and fit the model
#add a new column, lt_age, --the hazard ratio varies by a factor of log(time)
#ok not applicable here becase my independent veriales are not numeric
SURV2$lt_age <- SURV2$Location * log(SURV2$Location)
SURV2

#idetify last ID number
last <- SURV2$id[which.max(SURV2$time)]
last
#time 0, time1, and death day of last observation
intervals <- SURV2[SURV2$id == last, c("time0", "time", "death")]
intervals

#specific treatment (21 yers female) add age and sex to the last indivudual dataframe
covs <- data.frame(age = 21, female = 0, intervals)
#add log age column
covs$lt_age <- covs$age * log(covs$time)
covs

summary(survfit(model.2, newdata = covs, individual = TRUE))


##############

cut.points <- unique(SURV$time[SURV$death == 1])
SURV2 <- survSplit(data = SURV, cut = cut.points, end = "time", start = "time0", event = "death")
SURV2 <- SURV2[order(SURV2$id), ]

SURV2

model.1 <- coxph(Surv(time, death) ~ female + age, data = SURV)
covs <- data.frame(age = 21, female = 0)
summary(survfit(model.1, newdata = covs, type = "aalen"))
SURV2$lt_age <- SURV2$age * log(SURV2$time1)
model.2 <- coxph(Surv(time0, time1, death) ~ female + age + lt_age, data = SURV2)
SURV2
model.2<-coxph(formula=Surv(time0, time, death)~female + age, data=SURV2)
summary(model.2)

summary(survfit(model.2, newdata = covs, individual = TRUE))
summary(survfit(model.2, newdata = covs, type = "aalen"))




