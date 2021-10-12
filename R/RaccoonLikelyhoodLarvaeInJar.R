## <- double ## = LA notes

# Alma musseel experiment
# likelihood of larvae in jar

#library
library(tidyverse)
library(here)
library(readxl)
library(dplyr)

#jar and sample volumes (ml)
vol_jar <- 100 
vol_sample <- 6 

#fraction of water sampled
# this is the probably of successfully get a larva from the jar 
# in a binomial process
p <- vol_sample / vol_jar
#p=0.06

# vector of the potential actual number of larvae in a jar
# this is the number of "trials" or chances of sampling a larvae 
# in a binomial process

# create at data frame of sample probabilities
# the like_of_sample is the probably of obtaining the sample count 
# given the actual number of larvae.
# Thus, the probability is the likelihood of the sample given the jar total

# maximum number if larvae ever counted in a 6ml sample (can change this to the actual value)
max_count <- 71
# total number of larvae you think there could possibly be in a jar 
# (since target initial abundance is 800, 2000 should be plenty 
#no penalty for guessing big except a waste of computer time
max_possible_larvae <- 2000
#data frame of the 
# prob_count_given_jar is the likelihood of getting a particular count value given some actual number of larva in jar
#prob_jar_given_count is the probably of how many larve are in the jar given a count value (bayes therom assuming uniform prior)
#cummulative_prob is useful for getting random samples
d_jar_dist <- data.frame(sample_count = rep(0:max_count, each = max_possible_larvae + 1)) %>%
  mutate(jar_actual_larvae = rep(0:max_possible_larvae, times = max_count + 1)) %>%
  mutate(prob_count_given_jar = pbinom(sample_count, jar_actual_larvae, p) - 
           pbinom(sample_count-1, jar_actual_larvae, p)) %>%
  group_by(sample_count) %>%
  mutate(prob_jar_given_count = prob_count_given_jar / sum(prob_count_given_jar)) %>%
  mutate(cummulative_prob = cumsum(prob_jar_given_count)) %>%
  ungroup() %>%
  {.}
#View(d_jar_dist)

#plot of the prob distribuion
d_jar_dist %>%
  #filter(sample_count == 0) %>%
  ggplot(aes(jar_actual_larvae, prob_jar_given_count)) +
  geom_step(aes(colour = as.factor(sample_count))) +
  labs(colour = "Sample count") +
  xlab("Larvae in jar") +
  ylab("Probability of jar given count") +
  #xlim(0,100) +
  theme_bw()

#plot of the cumulative prob
d_jar_dist %>%
  #filter(sample_count == 0) %>%
  ggplot(aes(jar_actual_larvae, cummulative_prob)) +
  geom_step(aes(colour = as.factor(sample_count))) +
  labs(colour = "Sample count") +
  xlab("Larvae in jar") +
  ylab("Cummulative probability") +
  #xlim(0,100) +
  theme_bw()

#function that returns a vector (length = n) of random number of larvae in a jar given 6ml subsample count
r_jar <- function(n, count, dist){
  rj <- NULL
  for(i in 1:n){
    r = runif(1)
    rj_temp <- dist %>%
      filter(sample_count == count &
               cummulative_prob >= r &
               lag(cummulative_prob) < r ) %>%
      pull(jar_actual_larvae) %>%
      {.}
    
    if(is_empty(rj_temp)){
      rj_temp <- 0
    }
    rj <- c(rj, rj_temp)
  }
  
  return(rj)
}


#quick look at output of the r_jar function
# sampled distributions if 6ml sample contained 0, 10 or 30 larvae
data.frame(r0 = r_jar(100, 0, d_jar_dist), 
           r10 = r_jar(100, 10, d_jar_dist),
           r30 = r_jar(100, 30, d_jar_dist)) %>%
  pivot_longer(cols = starts_with("r"),names_to = "sample_count", values_to = "jar_count") %>%
  ggplot(aes(jar_count)) +
  geom_histogram() +
  facet_wrap(vars(sample_count), scales = "free")


#read in the real data
# count data for 6ml samples 
#d_count <- read_excel("data/real data.xlsx") %>%
d_count <- read_excel(here("data","real data.xlsx")) %>%
  arrange(jar_id, day) %>%
  group_by(jar_id) %>%
  mutate(delta_count = count - lag(count)) %>%
  ungroup()
#vector of unique jar IDs = 48
all_jars <- unique(d_count$jar_id)


#find problem jars with large increase in count from one time step to the next
problem_jars <- d_count %>%
  filter(delta_count >= 10 ) %>%
  pull(jar_id) %>%
  unique() %>%
  {.}

good_jars <- all_jars[!(all_jars %in% problem_jars)]


#problem jars=28 31 32 33 34 35 45 48
#Jar 28- real variation. Had some samples with 9, 9, 9, 11, 14 larvae total per sample with a high number of live in each
#Jar 31- had a single 1 ml sample where 18 were alive on day 10 which brought the count up
#Jar 32- typo, had one jar with 22 instead of 2
#Jar 33- On day 3 there were a particurlay large number of larvae per ml: 18,15,15,9. Maybe interns didnt mix correctly?
#Jar 34- Not anything to explain here, thats just how the counts came out
#Jar 35- On day 12 there were a particurlay large number of larvae per ml. Maybe interns didnt mix correctly?
#Jar 45- On day 1 got 2 1 ml samples with only 1 or 2 larvae, brought count down.
#Jar 48- Not anything to explain here, thats just how the counts came out





#Create simulated jar sample time series
# n_rep = number of replicate series to create
# This should be >=500 but 5 is engouhg to show how it works
n_rep <- 5
# empty frame to hold result
d_sim <- NULL
#pick which jars to run
#run_jars <- jar
run_jars <- problem_jars
#run_jars <- good_jars
#loop through all the jars picked
#time the loop
start_time <- Sys.time()
for(i in 1:length(run_jars)){
  #print jar to track progress
  print(paste("jar",run_jars[i]))
  #use data from one jar
  d_jar <- d_count %>%
    filter(jar_id == run_jars[i])
  #vector of raw jar sample count
  count <- d_jar$count
  n_count <- length(count)
  # vector the same lenght as count to hold random draw data 
  #(this gets over-written)
  r_count <- count
  # initialize rep counter to track number of simulated series
  rep_counter <- 1
  # while to so keep repeating until you get enough decreasing time series
  while(rep_counter <= n_rep) {
    #create a simulated jar counter time series
    for(j in 1:n_count){
      r_count[j] <- r_jar(1, count[j], d_jar_dist)
    }
    #test whether the series is monotonically decreasing
    is_valid <-  all(if_else(r_count <= lag(r_count), TRUE, FALSE), na.rm = TRUE)
    # if decreasing, add the result to the data frame and increment rep counter
    if(is_valid){
      d_temp <- data.frame(day = d_jar$day,
                           raw_count = count, 
                           sim_count = r_count) %>%
        mutate(jar_id = run_jars[i],
               rep_id = rep_counter,
               treatment = d_jar$treatment[1],
               site = d_jar$treatment[1])
      d_sim <- rbind(d_sim, d_temp)
      #print rep counter to track progress
      print(paste("rep_counter", rep_counter))
      rep_counter = rep_counter + 1
    }
  }
}
end_time <- Sys.time()
end_time - start_time

# plot the first 5 simulated series for all jars
d_sim %>%
  filter(rep_id <= 5) %>%
  ggplot(aes(day, sim_count)) +
  geom_line(aes(colour = as.factor(rep_id))) +
  facet_wrap(vars(jar_id))

#write simulated series to file
d_sim %>%
  arrange(jar_id, re, day) %>%
  write_csv(here("output", "d_sim.csv"))


# plot the first 5 simulated series for all jars
d_sim %>%
  filter(rep_id <= 5) %>%
  ggplot(aes(day, sim_count)) +
  geom_line(aes(colour = as.factor(rep_id))) +
  facet_wrap(vars(jar_id))

#write simulated series to file
d_sim %>%
  arrange(jar_id, rep_id, day) %>%
<<<<<<< HEAD
  write_csv(here("output", "d_sim_pj_200.csv"))
=======
  write_csv(here("output", "d_sim_pj_300.csv"))

  #number of days counted
  #CI20-5
  #CI5- 6
  #DB-7
  #PW-6
  


  


#Number of days we counted per site/treatment
#CI20- (5 days) 1,4,7,10,14
#CI5- (6 days) 1,3,6,9,11,14
#DB- (7 days) 1,3,5,7,10,12,14
#PW- (6 days) 1,4,7,10,12,14
#=24 days*48 jars*500=576,000
  
#number of days counted
#CI20-5
#CI5- 6
#DB-7
#PW-6


  

#Next steps on this.
# simulate a 1000 valid (monotoncially decreasing) time series of counts for each jar
# The above code makes valid series, but need to modify it so you get a 1000 for each jar of the real data
# treat the jars like you have 1000 replicate experiments
# expand the jar counts with 0/1 scoring per individual for running cox model like you did before
# you and run the analysis based on live count or dead count (for live count, subtract dead based on day1 count)
# run the cox model on each of the 1000 data sets and store the outpu hazard ration coefficients() a vector with 1000 values) 
#you can acually just save all 1000 model outputs (not just coffecients) in case we want to look at other values
#sort the coeffecinets from smallest to largest - the 2.5% and 97.5% quantiles will give the 95% confidence intervals. 
# the mean is the expected value of the hazard ratio

#after getting this to work with the simple cox model, can look at coxme or frailty_em to deal with mixed effects

# One issue is how to deal with the day 0 jar simumalion. Easiest (and maybe best?) solution is just start 
# your survival analysis on day 1 when you start the 6ml sampling.
#alternarive is to figure out the distibution of initial stocking density based on your stocking method.
#the target intial stocking was 800 per jar, but there is liekly a lot of noise around that based on sampling

#the reason why the jar sampling is an issue has to do with whether larvae in the jar are distributed randomly or uniformly. 
#If the larvae were uniformly distributed, your 6ml sample would exactly reflect what is in the jar
#However, larvae are (at best) randomly distributed in the jar, so sampling probability matters
#The larger the fraction of the jar you actually count, the less sampling probably matters (uniform assumption gets close enough)

#The actuall confidence intervals will be a bit wider than estimated with this method because of basic uncertainy in estiminating cox model
#however, with n = ~ 800, this should not be a big contribution and the CI should be pretty close.
