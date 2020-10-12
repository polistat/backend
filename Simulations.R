library(tidyverse)
library(dplyr)
library(leaps)
library(useful)

args <- commandArgs(trailingOnly=TRUE)
dir <- args[1]
currentDate <- as.Date(args[2])

state_cor <- as.matrix(read_csv(paste(dir, "StateCorrelationWith7", sep="/")) %>% select(-c(X1)))

states <- read.csv(paste("AveragedPolls - ", currentDate, ".csv", sep = "")) %>% 
  rename(State = StateName, mean = StateMean, variance = Variance, electoral_votes = ElectoralVotes) 
states <- cbind(states, state_outcome = rep(0, nrow(states)), distance = rep(0, nrow(states))) %>% 
  mutate(sd = variance^(1/2)) %>% 
  select(-c("variance", "X")) %>% 
  select(c(1,2,7,3,4,5,6))

states_length <- c(1:nrow(states))
cor_sum <- rowSums(state_cor)

row.rnorm <- function(x,y) rnorm(1, mean = x, sd = y)





#variables
num_simulations <- 1000000
electoral_counts <- as.data.frame(matrix(c(0,0), ncol = 2, nrow = 1, dimnames = list(NULL, c("rep_electoral_counts", "dem_electoral_counts"))))
outcome_counts <- as.data.frame(matrix(c(0,0,0), ncol = 3, nrow = 1, dimnames = list(NULL, c("rep_win_counts", "dem_win_counts", "split_win_counts"))))

electoral_bins <- (matrix(rep(0, 538), ncol = 1, nrow = 538))

stateResults <- cbind(
  data.frame(State = states$State, Mean = rep(0, nrow(states)), BidenWins = rep(0, nrow(states)), TrumpWins = rep(0, nrow(states))),
  cbind(
    states$electoral_votes,
    states$BPI
  )
) %>% 
  rename(ElectoralVotes = "1", BPI = "2")


sim_count <- 1

while(sim_count <= num_simulations){
  
  #resetting
  electoral_counts$rep_electoral_counts = 0
  electoral_counts$dem_electoral_counts = 0
  

  states$state_outcome = mapply(row.rnorm, x = states$mean, y = states$sd)
  

  states$distance = states$state_outcome - states$mean
  shifts <- t(states$distance %*% state_cor)/cor_sum
  states$state_outcome + shifts
  
  

  for(i in states_length){
    if(states$state_outcome[i] > 0.5){
      electoral_counts$rep_electoral_counts = electoral_counts$rep_electoral_counts + states$electoral_votes[i]
      stateResults$TrumpWins[i] = stateResults$TrumpWins[i] + 1
    } else if(states$state_outcome[i] < 0.5){
      electoral_counts$dem_electoral_counts = electoral_counts$dem_electoral_counts + states$electoral_votes[i]
      stateResults$BidenWins[i] = stateResults$BidenWins[i] + 1
    }
    
    stateResults$Mean[i] = stateResults$Mean[i] + states$state_outcome[i]
    

    
  }
  
  if(electoral_counts$rep_electoral_counts >= 270){
    outcome_counts$rep_win_counts = outcome_counts$rep_win_counts + 1
  } else if(electoral_counts$dem_electoral_counts >= 270){
    outcome_counts$dem_win_counts = outcome_counts$dem_win_counts + 1
  } else {
    outcome_counts$split_win_counts = outcome_counts$split_win_counts + 1
  }
  
  electoral_bins[electoral_counts$rep_electoral_counts, 1] = electoral_bins[electoral_counts$rep_electoral_counts, 1] + 1
  
  sim_count = sim_count + 1
}

stateResults$Mean <- stateResults$Mean/num_simulations

rep_win_percentage = outcome_counts$rep_win_counts[1]/num_simulations * 100
dem_win_percentage = outcome_counts$dem_win_counts[1]/num_simulations * 100
split_win_percentage = outcome_counts$split_win_counts[1]/num_simulations * 100

win_counts <- data.frame(rep_win_percentage, dem_win_percentage, split_win_percentage)



write.csv(stateResults, file = paste("StateOutcomes - ", currentDate, ".csv", sep = ""))
write.csv(outcome_counts, file = paste("NationalOutcomes - ", currentDate, ".csv", sep = ""))
write.csv(win_counts, file = paste("NationalWins - ", currentDate, ".csv", sep = ""))
write.csv(electoral_bins, file = paste("ElectoralBins - ", currentDate, ".csv", sep = ""))




