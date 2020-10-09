
knitr::opts_chunk$set(echo = FALSE, warning = FALSE)
library(tidyverse)
library(dplyr)
library(leaps)
library(useful)



dat1 <- read_csv("Enter demographic data here - Data to CSV.csv")
dat2 <- read_csv("Enter demographic data here - clark stuff.csv")
state_cor <- as.matrix(read_csv("StateCorrelationWith7") %>% select(-c(X1)))

dat1[is.na(dat1)] <- 0
dat2[is.na(dat2)] <- 0

states <- cbind(dat2$State, as.data.frame(matrix(0, nrow = 51, ncol = 5))) %>% rename(State = "dat2$State", mean = V1, sd = V2, electoral_votes = V3, state_outcome = V4, distance = V5) %>% mutate(electoral_votes = dat2$ElectoralVotes, mean = rep(0.5, 51), sd = rep(0.05, 51))
states_length <- c(1:nrow(states))
cor_sum <- rowSums(state_cor)

row.rnorm <- function(x,y) rnorm(1, mean = x, sd = y)



print(Sys.time())

#variables
num_simulations <- 1000000
electoral_counts <- as.data.frame(matrix(c(0,0), ncol = 2, nrow = 1, dimnames = list(NULL, c("rep_electoral_counts", "dem_electoral_counts"))))
outcome_counts <- as.data.frame(matrix(c(0,0,0), ncol = 3, nrow = 1, dimnames = list(NULL, c("rep_win_counts", "dem_win_counts", "split_win_counts"))))

sim_count <- 1

while(sim_count <= num_simulations){
  
  #resetting
  electoral_counts$rep_electoral_counts = 0
  electoral_counts$dem_electoral_counts = 0
  
  #main
  #for(i in states_length){
  #  states$state_outcome[i] = rnorm(1, mean=states$mean[i], sd=states$sd[i])
  #}
  states$state_outcome = mapply(row.rnorm, x = states$mean, y = states$sd)
  
  #adding after the fact
  states$distance = states$state_outcome - states$mean
  shifts <- t(states$distance %*% state_cor)/cor_sum
  states$state_outcome + shifts
  
  
  #convert to electoral votes
  for(i in states_length){
    if(states$state_outcome[i] > 0.5){
      electoral_counts$rep_electoral_counts = electoral_counts$rep_electoral_counts + states$electoral_votes[i]
    } else if(states$state_outcome[i] < 0.5){
      electoral_counts$dem_electoral_counts = electoral_counts$dem_electoral_counts + states$electoral_votes[i]
    }
  }
  
  if(electoral_counts$rep_electoral_counts >= 270){
    outcome_counts$rep_win_counts = outcome_counts$rep_win_counts + 1
  } else if(electoral_counts$dem_electoral_counts >= 270){
    outcome_counts$dem_win_counts = outcome_counts$dem_win_counts + 1
  } else {
    outcome_counts$split_win_counts = outcome_counts$split_win_counts + 1
  }
  
  sim_count = sim_count + 1
}

rep_win_percentage = outcome_counts$rep_win_counts[1]/num_simulations * 100
dem_win_percentage = outcome_counts$dem_win_counts[1]/num_simulations * 100

print(paste("Republican Win%: ", rep_win_percentage))
print(paste("Democratic Win%: ", dem_win_percentage))

print(Sys.time())


