function() { 
  R6Class("scenario.OddRatio1",
                              inherit = Scenario,
                              public = list(
                                
                                initialize = function(){
                                  
                                  # Distribution parameters
                                  probT <- ScenarioParameter$new("probability Treatment", Support$new(Domain$new(0, 1), "continuous"))
                                  probC <- ScenarioParameter$new("probability Control", Support$new(Domain$new(0, 1), "continuous"))
                                  
                                  descriptionLatex <- "
                                  This scenario simulates a vector of Odd Ratios \\(Z_{OR}\\) given the vector of probability for the treatment arm \\(P_T\\) and  control arm \\(P_C\\) from the PsaSample.</br> 
                                  The \\(n\\) represent the sample size in each arm: \\(n_C\\) anf \\(n_T\\). The total sample size required is then twice the ideal sample size shown on the plot.   </br> 
                                  
                                  In order to generate an odd ratio  a count of event is generated for each treatment arm:</br> 
                                   \\[
                                   z_{P_T} \\sim Bin(n, P_T)
                                  \\]
                                 and 
                                  \\[
                                   z_{P_C} \\sim Bin(n, P_C)
                                  \\]
                                  Then the new samples' Odd Ratios can be derived as 
                                  \\[
                                   Z_{OR} = \\dfrac{z_{P_T} / z_{P_C}}{(n_T - z_{P_C}) / (n_C - z_{P_T})}  
                                  \\]"
                                  
                                  super$initialize("scenario.OddRatio1", "Odd Ratio", ScenarioParameterList$new(list(probT, probC)), descriptionLatex)
                                },
                                simulate = function(simSize, size, cache, reps){
                                  parsprobT <- private$paramList$getValueOfParameterWithName("probability Treatment",simSize, cache, reps)
                                  parsprobC <- private$paramList$getValueOfParameterWithName("probability Control",simSize, cache, reps)
                                  
                                  simparT <- rbinom(simSize * reps, size, parsprobT)
                                  simparC <- rbinom(simSize * reps, size, parsprobC)
                                  simOr <- (simparT / (size-simparT)) / (simparC / (size-simparC))
                                  # Regenerate value when OR is na or infinity
                                  mistake <- is.na(simOr)|is.infinite(abs(simOr))
                                  while(sum( mistake)>0){
                                    
                                    simparT[mistake] <- rbinom(length(mistake), size, parsprobT[ mistake])
                                    simparC[mistake] <- rbinom(length(mistake), size, parsprobC[ mistake])
                                    simOr <- (simparT / (size-simparT)) / (simparC / (size-simparC))
                                    mistake <- is.na(simOr)|is.infinite(abs(simOr))
                                  }
                                  if(sum(is.na(simOr))>0 || sum(is.infinite(abs(simOr)))>0){
                                    
                                    stop("cerr: Some simulated values were equal to infinity. Please chose bigger sample sizes.")
                                  }
                                  simulated  <- matrix(ncol=1,nrow=simSize * reps)
                                  simulated[,1]<-simOr

                                  return(simulated)
                                  
                                },getSimulatedParameterName = function(){
                                  return(list(private$paramList$getParameterWithName("probability Treatment")$getValue()$getDist(),
                                              private$paramList$getParameterWithName("probability Control")$getValue()$getDist()))
                                }))$new()
}
