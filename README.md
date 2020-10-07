# polistat-backend-2020
Home for code for our model.
## simulation.py
This code is one of the final steps for our model. After we finish averaging polls and conducting priors to set a baseline vote for each state, we take those datapoints and run it through this code. For each simulation, we select a random number, which is equivalent to the national voter turnout. then for each state, we select a random number from a normal distribution determined by the state's lean and its variance minus the turnout variance. We add the random turnout number to the state number to determine to whom the state's electoral votes go to.
## CorrelationSchemes.py
This code is to be run during simulations. Two separate schemes for correlating states are described.
## Simulations_with_Correlation.py
Implementation of Correlations with simulations
## Simulations.Rmd
Implementation of Correlations with simulations
