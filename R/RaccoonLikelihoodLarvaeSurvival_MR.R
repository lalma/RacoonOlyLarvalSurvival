#Lindsay's oysters

setwd("~/Documents/Github/RacoonOlyLarvalSurvival")
library(tidyverse)


# read in simulated data
#example is just on one jar - need to loop through all reps of all the jars

library(readxl)
database <- read_excel("d_sim_all.xlsx")
View(database)

#create a data frame for the expanded data
#populate day and status with default assumption that they are alive at the last sample day

numreps <- as.numeric(max(database$rep_id)) #number of replicates
numjars <- as.numeric(max(database$jar_id)) # number of jars

dead_index <- 1 #everyone is alive at the beginning. this has to =1 because witchcraft

#create an empty list to store each category's response in
replist <- vector(mode = "list", length = length(numreps * numjars))

for (i in 1:numjars) { #for each of the 48 jars
  #i is basically jar number
  jar <- filter(database, jar_id == i) #filter out the ith jar
  
  for(j in 1:numreps) { #for each of the 500 reps in each jar
    #j is the replicate it's on
    count <- filter(jar, rep_id == j)
    d_expand <- data.frame(jar_id = rep(count$jar_id[1], times = count$sim_count[1]))  %>% #create a data frame of that jar
      #the data frame should be the length of the number of simulated larvae on day 1 aka first line of the database
      mutate(rep_id = count$rep_id[1],  #create the number of rows that corresponds to the 
             #number of simulated larvae on day 1
             treatment = count$treatment[1], 
             site = count$site[1], 
             day = count$day[nrow(count)],
             status = 0)
    
    for(z in 2:(nrow(count))) { #days
      new_dead <- count$sim_count[z-1] - count$sim_count[z] #get number that died on this new timepoint
      d_expand$day[dead_index : nrow(d_expand)] <- count$day[z] #repeat that timepoint the number 
      #of times of new_dead i.e. the number that died that day
      d_expand$status[dead_index : nrow(d_expand)] <- 1 #change those numbers to 1
      dead_index <- dead_index + new_dead #adding new number of dead larvae to the current tally 
      #from the previous day
    }
    d_expand$status[nrow(d_expand) : dead_index] <- 0 #put the correct number of 0's at the bottom
    dead_index <- 1 #reset this to 1 between replicates
    replist[[(((i - 1) * 500) + j)]] <- d_expand #put this d_expand dataframe into the i*jth spot on the list
  }
}


cox <- bind_rows(replist)
write.csv(cox, file = "output/cox.csv")
