#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

bml.sim <- function(r, c, p){
  m <- bml.init(r, c, p)
  m.start <- m
  changed <- TRUE
  counter = 0
  while(changed && counter < 10000){
    temp <- bml.step(m)
    m <- temp[[1]]
    #print(m)
    changed <- temp[[2]]
    if(identical(m.start, m)){
      print("LOOPING")
    }
    #print(changed)
    if(changed){
      counter <- counter + 1
    }
    #print(counter)
    #Sys.sleep(1)
    #image(m, axes = FALSE, col = c("white", "red", "blue"))
  }
  return(list(m, counter))
}
total <- data.frame(density= numeric(0), mean= numeric(0))
results1v1 <- bml.sim(10,10,.2) #10000
results1v2 <- bml.sim(10,10,.2) #10000
results1v3 <- bml.sim(10,10,.2) #10000
results1v4 <- bml.sim(10,10,.2) #10000
results1v5 <- bml.sim(10,10,.2) #10000
average_vector1 <- c(results1v1[[2]],results1v2[[2]], results1v3[[2]], results1v4[[2]], results1v5[[2]])
row<- (c(.2, mean(average_vector1)))
total <- rbind(total, row)
results1<- list(results1v1, results1v2, results1v3,results1v4,results1v5)

results2v1 <- bml.sim(10,10,.4) #10000
results2v2 <- bml.sim(10,10,.4) #10000
results2v3 <- bml.sim(10,10,.4) #10000
results2v4 <- bml.sim(10,10,.4) #10000
results2v5 <- bml.sim(10,10,.4) #10000
average_vector2 <- c(results2v1[[2]],results2v2[[2]], results2v3[[2]], results2v4[[2]], results2v5[[2]])
row<- (c(.4, mean(average_vector2)))
total <- rbind(total, row)
results2 <- list(results2v1, results2v2, results2v3,results2v4,results2v5)


results3v1 <- bml.sim(10,10,.6) #203
results3v2 <- bml.sim(10,10,.6) #58
results3v3 <- bml.sim(10,10,.6) #16
results3v4 <- bml.sim(10,10,.6) #90
results3v5 <- bml.sim(10,10,.6) #10000
average_vector3 <- c(results3v1[[2]],results3v2[[2]], results3v3[[2]], results3v4[[2]], results3v5[[2]])
row<- (c(.6, mean(average_vector3)))
total <- rbind(total, row)
results3 <- list(results3v1, results3v2, results3v3,results3v4,results3v5)

results4v1 <- bml.sim(10,10,.8) #7
results4v2 <- bml.sim(10,10,.8) #8
results4v3 <- bml.sim(10,10,.8) #40
results4v4 <- bml.sim(10,10,.8) #11
results4v5 <- bml.sim(10,10,.8) #14
average_vector4 <- c(results4v1[[2]],results4v2[[2]], results4v3[[2]], results4v4[[2]], results4v5[[2]])
row<- (c(.8, mean(average_vector4)))
total <- rbind(total, row)
results4 <- list(results4v1, results4v2, results4v3,results4v4,results4v5)

plot(total, xlab = "Density", ylab="Runs needed", main="Density And Gridlock in BML")
save(results1,results2,results3,results4, file="bml_figures.rdata")