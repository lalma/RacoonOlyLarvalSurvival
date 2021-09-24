## <- double ## = LA notes

# Alma musseel experiment
# likelihood of larvae in jar

#library
library(tidyverse)
library(here)
library(readxl)

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
max_count <- 81
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
View(d_jar_dist)

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

#fake count data for 6ml samples 
##fake data is actually real data -but DB location only 
## include 14 days from 6 jars 2 temps
##84 rows long
library(readxl)
d_fake <- read_excel(here("data/RaccoonLikelyhoodLarvaeInJar","real data.xlsx"))
View(d_fake)

#create simulted data
#n_rep is the number of replicate trajectories
n_rep <- 50 
d_sim <- NULL
for(i in 1:nrow(d_fake)){
  d_sim <- rbind(d_sim, data.frame(treatment = rep(d_fake$treatment[i], n_rep),
                                   jar_id = rep(d_fake$jar_id[i], n_rep),
                                   day = rep(d_fake$day[i], n_rep),
                                   jar_count = r_jar(n_rep, d_fake$count[i], d_jar_dist),
                                   replicate = 1:n_rep))
}
##60,000 rows here
##takes a while to run 
View(d_sim)
write.csv(d_sim, "d_sim.csv")

#add the original sample counts to the dataframe so seen the input for the random draw
#sort for viewing dataframe
d_sim_sorted <- d_sim%>%
  full_join(d_fake) %>%
  arrange(jar_id, replicate, day)


View(d_sim_sorted)
write.csv(d_sim_sorted, "d_sim_sorted.csv")


# plot the first three replicates from the 3 jars
# problem is that most of the simulated series are not monotonically decreasing
d_sim_sorted %>%
  filter(replicate <= 3) %>%
  ggplot(aes(day, jar_count)) +
  geom_step(aes(colour = as.factor(replicate))) +
  facet_wrap(vars(jar_id, replicate))



#filter to find only data sets that are montonicaly decreasing
# this is computationally a pretty inefficient way to do this, but relatively quick to to code
d_sim_decreasing <- d_sim_sorted %>%
  group_by(jar_id, replicate) %>%
  mutate(is_decreasing = if_else(jar_count <= lag(jar_count), TRUE, FALSE)) %>%
  mutate(is_decreasing = if_else(is.na(is_decreasing), TRUE, is_decreasing)) %>%
  mutate(is_all_decreasing = all(is_decreasing)) %>%
  filter(is_all_decreasing) %>%
  ungroup() %>%
  {.}

View(d_sim_decreasing)


#plot the decreasing series 
#of the 300 series orginally created, not that many turn out to be monotonically decreasing
d_sim_decreasing %>%
  ggplot(aes(day, jar_count)) +
  geom_step(aes(colour = as.factor(replicate))) +
  facet_wrap(vars(jar_id, replicate))

ggsave("decresing_CI20only_50reps.png")

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
