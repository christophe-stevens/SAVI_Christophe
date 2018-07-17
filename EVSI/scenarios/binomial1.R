function() { 
  R6Class("scenario.binomial1",
                              inherit = Scenario,
                              public = list(
                                
                                initialize = function(){
                                  
                                  # Distribution parameters
                                  prob <- ScenarioParameter$new("probability", Support$new(Domain$new(0, 1), "continuous"))
                                  
                                  descriptionLatex <- "
                                  This scenario simulates a vector of probability \\(Z_p\\) given a vector of probability \\(P\\)from the PsaSample.</br> 
                                  \\(n\\) is the new sample size parameters.  
                                  
                                  \\[
                                    Z_P \\sim \\dfrac{Bin(n, P)}{n} 
                                  \\]"
                                  super$initialize("scenario.binomial1", "Single probability", ScenarioParameterList$new(list(prob)), descriptionLatex)
                                },
                                simulate = function(simSize, size, cache, reps){
                                  
                                  parsProb <- private$paramList$getValueOfParameterWithName("probability",simSize, cache, reps)
                                  
                                  simulated  <- matrix(ncol=1,nrow=simSize * reps )
                                  simulated[,1]<-rbinom(simSize * reps, size, parsProb) / size
                                  return(simulated)
                                },getSimulatedParameter = function(cache){
                                  parsprob<- cache$psaSampleList$getPsaSampleWithName(private$paramList$getParameterWithName("probability")$getValue()$getDist())$getData()

                                  SimulatedMatrix <- matrix(ncol=1,byrow=F, c(parsprob) )
                                  
                                  return(SimulatedMatrix)
                                }))$new()
}

