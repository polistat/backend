library(tidyverse)
library(dplyr)
library(leaps)
library(useful)
library(stringr)
library(metafor)


args <- commandArgs(trailingOnly=TRUE)
dir <- args[1]
currentDate <- as.Date(args[2])


polls <- read.csv(paste(dir, "538.csv", sep="/")) %>% 
  select(-c("Poll", "Source", "End.Date")) %>%
  transmute(
    State = State,
    Sample.Size = Sample.Size,
    Sample.Type = Sample.Type,
    Start.Date = as.Date(Start.Date, format = "%m/%d/%y"), 
    Biden = Biden, 
    Trump = Trump) %>% 
  mutate(diff = (100 - Biden - Trump)/2) %>% 
  transmute(
    State = State,
    Sample.Size = Sample.Size,
    Sample.Type = Sample.Type,
    Start.Date = Start.Date,
    Biden = (Biden + diff)/100, 
    Trump = (Trump + diff)/100) 


polls <- polls[polls$Start.Date >= as.Date("2020-08-12", format = "%Y-%m-%d") & polls$Start.Date <= currentDate, ]

polls$State <- gsub("Maine CD-1", "Maine", polls$State)
polls$State <- gsub("Nebraska CD-1", "Nebraska", polls$State)
polls$State <- gsub("Nebraska CD-3", "Nebraska", polls$State)
polls <- arrange(polls, State, Sample.Type, desc(Start.Date))
polls <- na.omit(polls)

priors <- read.csv(paste(dir, "Priors.csv", sep="/")) %>% 
  rename(PriorsMean = StateMean)

demographics <- read.csv(paste(dir, "Standardized_State_priors.csv", sep="/")) %>% 
  rename(StateName = State)



z.test.method <- function(state, date, size, population, biden, trump) {
  
  df <- data.frame(state, date, size, population, biden, trump)
  numBlocks <- trunc((as.numeric(currentDate - min(date)))/10)
  
  if(numBlocks < 0){
    numBlocks <- 0
  }
  

  
  if(numBlocks == 0){
    if(nrow(df) == 0){
      return(paste(state, 0, 0, 0, 0, sep = ", "))
    } else if(nrow(df) == 1){
      return(paste(state, trump, (trump*biden/size)^(1/2), 1, size, sep = ", "))
    }
    else{
      block1 <-  escalc(xi = round(size*trump), ni = size, measure = "PR")
      block1$vi <- block1$vi + (1/30 * (1 - (trump + biden)))^2
      block1.rma <- rma(yi = block1$yi, vi = block1$vi)
      
      mean1 <-  block1.rma$beta[1]
      sd1 <-  block1.rma$se 
    }
    return(paste(state[1], mean1, sd1, length(block1$size), sum(block1$size), sep = ", "))
  }
  else if(numBlocks == 1){
    block1df <- df
    
    block1 <- escalc(data = block1df, xi = round(size*trump), ni = size, measure = "PR")
    block1$vi <- block1$vi + (1/30 * (1 - (block1df$trump + block1df$biden)))^2
    block1.rma <- rma(yi = block1$yi, vi = block1$vi)
    
    mean1 <-  block1.rma$beta[1]
    sd1 <-  block1.rma$se 
    return(paste(state[1], mean1, sd1, length(block1$size), sum(block1$size), sep = ", "))
  } 
  else{
    blocks <- c(1:(numBlocks-1))
    block.final <- 0
    
    for(i in blocks){
      
      if(i == 1){
        block1df <- df[df$date < currentDate - (numBlocks - i)*10, ] %>% transmute(
          state = state,
          date = currentDate - (numBlocks - i + 1)*10, 
          size = size, 
          population = population, 
          biden = biden, 
          trump = trump)
      } else{
        block1df <- df[df$date >= currentDate - (numBlocks - i + 1)*10 & df$date < currentDate - (numBlocks - i)*10, ]
      }
      block2df <- df[df$date >= currentDate - (numBlocks - i)*10 & df$date < currentDate - (numBlocks - i - 1)*10, ]
      

      
      if(nrow(block1df) > 0 & nrow(block2df) == 0){
        df <- arrange(rbind(
          setdiff(df, block1df),
          block1df %>% transmute(
            state = state,
            date = date + 10, 
            size = size, 
            population = population, 
            biden = biden, 
            trump = trump)
        ), 
        desc(date))
        
      } else {
        
        if(nrow(block1df) == 1){
          mean1 <- block1df$trump
        } else {
          block1 <- escalc(data = block1df, xi = round(size*trump), ni = size, measure = "PR")
          block1$vi <- block1$vi + (1/30 * (1-(block1df$trump+block1df$biden)))^2
          block1.rma <- rma(yi = block1$yi, vi = block1$vi)
          mean1 <-  block1.rma$beta[1]
        }
        
        if(nrow(block2df) == 1){
          mean2 <- block2df$trump
        } else {
          block2 <- escalc(data = block2df, xi = round(size*trump), ni = size, measure = "PR")
          block2$vi <- block2$vi + (1/30 * (1-(block2df$trump+block2df$biden)))^2
          block2.rma <- rma(yi = block2$yi, vi = block2$vi)
          mean2 <-  block2.rma$beta[1]
        }
        
        ztest <- prop.test(
          x = c(mean1*sum(block1df$size), mean2*sum(block2df$size)), 
          n = c(sum(block1df$size), sum(block2df$size))
        )
        

        
        if(ztest$p.value >  0.05){
          df <- arrange(rbind(
            setdiff(df, block1df),
            block1df %>% transmute(
              state = state,
              date = date + 10, 
              size = size, 
              population = population, 
              biden = biden, 
              trump = trump)
          ), 
          desc(date))
        }
        
      }
      
      if(i == (numBlocks-1)){
        block2df <- df[df$date >= currentDate - (numBlocks - i)*10 & df$date < currentDate - (numBlocks - i - 1)*10, ]
        block2 <- escalc(data = block2df, xi = round(size*trump), ni = size, measure = "PR")
        block2$vi <- block2$vi + (1/30 * (1-(block2df$trump+block2df$biden)))^2
        block2 <- rma(yi = block2$yi, vi = block2$vi)
        

        
        block.finaldf <- block2df
        block.final <- block2
      }
    }

    return(paste(state[1], block.final$beta[1], block.final$se, length(block.finaldf$size), sum(block.final$size), sep = ", "))
    
  }
}




results <- polls %>% 
  group_by(State) %>% 
  summarize(result = z.test.method(state = State, date = Start.Date, size = Sample.Size, population = Sample.Type, biden = Biden, trump = Trump)) 

results <- full_join(priors,
                     as.data.frame(str_split(results$result, ", ", simplify = TRUE)) %>% 
                       transmute(
                         StateName = V1,
                         PollsMean = as.numeric(as.character(V2)),
                         Variance = (as.numeric(as.character(V3))^2),
                         NumPolls = as.numeric(as.character(V4)),
                         Sample.Size = as.numeric(as.character(V5))
                       ),
                     by = "StateName") 
results[is.na(results)] <- 0


results <- results %>% 
  mutate(StateMean = 1.92/pi * atan(0.65 * NumPolls) * PollsMean + (1 - 1.92/pi * atan(0.65 * NumPolls)) * PriorsMean/100)
results <- left_join(results, demographics, by = "StateName") %>% 
  transmute(
    StateName = StateName,
    StateMean = StateMean,
    Variance = Variance + ((0.6 * (CoVI + 2.06))/100)/4 + (((as.numeric(as.Date("2020-11-03") - currentDate))/7)^(1/2)/400)/4,
    ElectoralVotes = ElectoralVotes,
    BPI = BPI
  )

write.csv(results, file = paste("AveragedPolls - ", currentDate, ".csv", sep = ""))

