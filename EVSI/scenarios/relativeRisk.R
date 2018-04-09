function() { 
  
  R6Class("scenario.RelativeRisk1 ",
                                  inherit = Scenario,
                                  public = list(
                                    
                                    initialize = function(){
                                      
                                      # Distribution parameters
                                      probT <- ScenarioParameter$new("probability Treatment", Support$new(Domain$new(0, 1), "continuous"))
                                      probC <- ScenarioParameter$new("probability Control", Support$new(Domain$new(0, 1), "continuous"))
                                      
                                      descriptionLatex <- "
                                      This scenario simulates a vector of Relative Risk \\(Z_{RR}\\) given the vector of probability for the treatment arm \\(P_T\\) and  control arm \\(P_C\\) from the PsaSample.</br> 
                                      The \\(n\\) represent the sample size in each arm: \\(n_C\\) anf \\(n_T\\). The total sample size required is then twice the ideal sample size shown on the plot.   </br> 
                                      
                                      In order to generate a Relative Risk, a count of event is generated for each treatment arm:</br> 
                                      \\[
                                      z_{P_T} \\sim Bin(n_T, P_T)
                                      \\]
                                      and 
                                      \\[
                                      z_{P_C} \\sim Bin(n_C, P_C)
                                      \\]
                                      Then the new samples' Relative Risks can be derived as 
                                      \\[
                                      Z_{RR} = \\dfrac{z_{P_T}/n_T}{z_{P_C}/n_C}  
                                      \\]"
                                      
                                      super$initialize("scenario.RelativeRisk1 ", "Relative Risk",ScenarioParameterList$new(list(probT, probC)),descriptionLatex)
                                    },
                                    simulate = function(simSize, size, cache, reps){
                                      parsprobT <- private$paramList$getValueOfParameterWithName("probability Treatment",simSize, cache, reps)
                                      parsprobC <- private$paramList$getValueOfParameterWithName("probability Control",simSize, cache, reps)
                                      
                                      simProbParT <- rbinom(simSize * reps, size, parsprobT) / size
                                      simProbParC <- rbinom(simSize * reps, size, parsprobC) / size
                                      simRr <- ( simProbParT / simProbParC)
                                      
                                      mistake <- is.na(simRr)|is.infinite(abs(simRr))
                                      while(sum( mistake)>0){
                                        
                                        simProbParT[mistake] <- rbinom(length(mistake), size, parsprobT[ mistake]) / size
                                        simProbParC[mistake] <- rbinom(length(mistake), size, parsprobC[ mistake]) / size

                                        simRr <- ( simProbParT / simProbParC)
                                        mistake <- is.na(simRr)|is.infinite(abs(simRr))
                                      }
                                      
                                      if(sum(is.na(simRr))>0 || sum(is.infinite(abs(simRr)))>0){
                                        stop("cerr: Some simulated values were equal to infinity. Please chose bigger sample sizes.")
                                      }
                                      simulated  <- matrix(ncol=1,nrow=simSize * reps)
                                      simulated[,1]<-simRr
                                      return(simulated)
                                      
                                    }))$new()
}
