
library(tidyverse)
library(dplyr)
library(leaps)
library(useful)
library(stringr)
library(metafor)


currentDate <- as.Date(scan(file = "currentDate.txt", what = "character"))



polls <- read.csv("pollsData.csv") %>% 
  select(-c("Poll", "Source", "End.Date")) %>%
  transmute(
    State = State,
    Sample.Size = Sample.Size,
    Sample.Type = Sample.Type,
    Start.Date = as.Date(Start.Date, format = "%m/%d/%y"), 
    Biden = (Biden + (100 - Biden - Trump)/2)/100, 
    Trump = (Trump + (100 - Biden - Trump)/2)/100)

polls <- polls[polls$Start.Date >= as.Date("2020-08-12", format = "%Y-%m-%d"), ]
polls <- arrange(polls, State, Sample.Type, desc(Start.Date))
polls <- na.omit(polls)





z.test.method <- function(state, date, size, population, biden, trump) {
  
  df <- data.frame(state, date, size, population, biden, trump)
  numBlocks <- trunc((as.numeric(currentDate - min(date)))/10)
  
  if(numBlocks < 0){
    numBlocks <- 0
  }
  
  #print(numBlocks)
  
  if(numBlocks == 0){
    if(nrow(df) == 0){
      return(paste(state, "NA", "NA", sep = ", "))
    } else if(nrow(df) == 1){
      return(paste(state, trump, (trump*biden/size)^(1/2), sep = ", "))
    }
    else{
      block1 <-  escalc(xi = round(size*trump), ni = size, measure = "PR")
      block1$vi <- block1$vi + (1/30 * (1 - (trump + biden)))^2
      block1.rma <- rma(yi = block1$yi, vi = block1$vi)
      
      mean1 <-  block1.rma$beta[1]
      sd1 <-  block1.rma$se 
    }
    return(paste(state[1], mean1, sd1, sep = ", "))
  }
  else if(numBlocks == 1){
    block1df <- df
    
    block1 <- escalc(data = block1df, xi = round(size*trump), ni = size, measure = "PR")
    block1$vi <- block1$vi + (1/30 * (1 - (block1df$trump + block1df$biden)))^2
    block1.rma <- rma(yi = block1$yi, vi = block1$vi)
    
    mean1 <-  block1.rma$beta[1]
    sd1 <-  block1.rma$se 
    return(paste(state[1], mean1, sd1, sep = ", "))
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
      
      #print(i)
      #print(block1df)
      #print(block2df)
      
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
        
        #print(ztest$p.value)
        
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
        
        #print(block2df)
        
        block.final <- block2
      }
    }
    
    return(paste(state[1], block.final$beta[1], block.final$se, sep = ", "))
    
  }
}


results <- polls %>% 
  group_by(State) %>% 
  summarize(result = z.test.method(state = State, date = Start.Date, size = Sample.Size, population = Sample.Type, biden = Biden, trump = Trump)) 

results <- as.data.frame(str_split(results$result, ", ", simplify = TRUE)) %>% rename(State = V1, Mean = V2, Variance = V3)

write.csv(results, file = "AveragedPolls")

