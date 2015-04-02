#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  noCar <- 1-p
  redCar <- p/2
  blueCar <- p/2
  probs <-c(noCar, redCar, blueCar)
  x <- c(0, 1, 2)
  state <- sample(x,size = r*c, replace = TRUE, prob=probs)
  m <- matrix(state, nrow = r, ncol = c)
  return(m)
}

image(m, axes = FALSE, col = c("white", "red", "blue"))

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){
  grid.new <- TRUE
  m.new.red <- bml.step.red(m)
  m.new <- bml.step.blue(m.new.red)
  if(identical(m.new, m)){
    grid.new <- FALSE
  } 
  return(list(m.new, grid.new))
}


bml.step.red <- function(m){
  m.new <- m
  for(x in 1:nrow(m)){
    for(y in 1:ncol(m)){
      if(m[x, y] == 1){
        if(x+1 > nrow(m)){
          if(m[1,y] == 0){
            m.new[x,y]=0
            m.new[1,y]=1
          }
        }
        else if(m[(x+1),y] == 0){
          m.new[x,y]=0
          m.new[x+1,y]=1
        }
      }
    }
  }
  return(m.new)
}

bml.step.blue <- function(m){
  m.new <- m
  for(x in 1:nrow(m)){
    for(y in 1:ncol(m)){
      if(m[x, y] == 2){
        if(y+1 > nrow(m)){
          if(m[x,1] == 0){
            m.new[x,y]=0
            m.new[x,1]=2
          }
        }
        else if(m[x,(y+1)] == 0){
          m.new[x,y]=0
          m.new[x,y+1]=2
        }
      }
    }
  }
  return(m.new)
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  m <- bml.init(r, c, p)
  changed <- TRUE
  counter = 0
  while(changed){
    temp <- bml.step(m)
    m <- temp[[1]]
    print(m)
    changed <- temp[[2]]
    print(changed)
    if(changed){
      counter <- counter + 1
  }
    print(counter)
    Sys.sleep(1)
    image(m, axes = FALSE, col = c("white", "red", "blue"))
  }
}
