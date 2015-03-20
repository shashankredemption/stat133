# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){
  x <- matrix(nrow = n.doctors, ncol = n.days)
  x[,1] <- initial.doctors
  for(i in 1:(n.days-1)){
    first <- sample(1:n.doctors, 1)
    second <- sample(1:n.doctors, 1)
    n.rv <- x[,i]
    while(first == second){
        second <- sample(1:n.doctors, 1)
      }
      y<-c(x[first,i], x[second,i])
      if(y[1]== 1 && y[2] == 0){
        p <- p*100
        z <- sample(1:100, 1)
        if (p >= z){
          n.rv[second] <- 1
        }
      }
      if(y[1]== 0 && y[2] == 1){
        p <- p*100
        z <- sample(1:100, 1)
        if (p >= z){
          n.rv[first] <- 1
        }
      }
    x[,i+1] <- n.rv
    }
  return(x)
}

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)

