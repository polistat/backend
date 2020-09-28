# polistat-backend-2020

## simulation.py
This code is one of the final steps for our model. After we finish averaging polls and conducting priors to set a baseline vote for each state, we take those datapoints and run it through this code. For each simulation, we select a random number, which is equivalent to the national voter turnout. then for each state, we select a random number from a normal distribution determined by the state's lean and its variance minus the turnout variance. We add the random turnout number to the state number to determine to whom the state's electoral votes go to.

### Variables:
- **num_simulations:** The number of simulations to run
- **electoral counts:** A 1x2 matrix, whose first cell contains the electoral votes going to candidate 1, and whose second cell contains the electoral votes going to candidate 2.
- **outcome counts:** A 1x3 matrix, whose first cell counts the number of times candidate 1 wins, whose second cell counts the number of times candidate 2 wins, and whose third cell contains the number of times neither candidate wins (probably unnecessary for our model)
- **turnout_mean:** The average national voter turnout. The purpose of this is to provide some level of variance that is the same for all states
    -We might need to do some transformations on this number to adhere to how we are representing the states's leans
- **turnout_variance:** The variance for national voter turnout
- **states:** A 51x4 matrix, whose rows represent individual states. The first column contains the state's name, the second column contains the state's lean, the third column contains the state's variance, and the fourth column contains the state's electoral votes
- **turnout_shift:** A random number calculated to provide some level of variance for all states during individual simulations
- **state_outcome:** The shifted outcome of the election in a state selected from a normal distribution
- **candidate_1_win_percentage:** the number of wins counted for candidate 1 divided by the number of simulations, multiplied by 100
- **candidate_2_win_percentage:** the number of wins counted for candidate 2 divided by the number of simulations, multiplied by 100

