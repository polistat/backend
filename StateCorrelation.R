library(tidyverse)
library(dplyr)
library(leaps)
library(useful)

demographics <- read.csv("Standardized_State_priors.csv")
data_state <- demographics %>% 
  select(-c("BPI", "CoVI"))

# converting into long form

data_state_long <- data_state %>% 
  group_by(State) %>% 
  gather(predictor, value, 2:(ncol(.))) %>% 
  ungroup()


# scale and spread
data_state_long <- data_state_long %>%
  group_by(predictor) %>%
  # scale all predictors so they can be compared
  mutate(value = (value - min(value, na.rm=TRUE))/(max(value, na.rm=TRUE) - min(value, na.rm=TRUE))) %>%
  
  # spreading
  spread(State, value) %>% 
  na.omit() %>%
  ungroup() %>%
  dplyr::select(-predictor)

# the math for how this is calculated can be found on the cor() documentation
correlation_matrix_7 <- cor(data_state_long)  

diag(correlation_matrix_7) <- 0

write.csv(correlation_matrix_7, file = "StateCorrelationWith7")
