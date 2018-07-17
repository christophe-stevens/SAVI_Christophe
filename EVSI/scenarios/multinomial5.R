function() { 
  R6Class("scenario.multinomial5",
                                 inherit = Scenario,
                                 public = list(
                                   
                                   initialize = function(){
                                     
                                     # Distribution parameters
                                     state1 <- ScenarioParameter$new("state 1", Support$new(Domain$new(0, +Inf), "continuous"))
                                     state2 <- ScenarioParameter$new("state 2", Support$new(Domain$new(0, +Inf), "continuous"))
                                     state3 <- ScenarioParameter$new("state 3", Support$new(Domain$new(0, +Inf), "continuous"))
                                     state4 <- ScenarioParameter$new("state 4", Support$new(Domain$new(0, +Inf), "continuous"))
                                     state5 <- ScenarioParameter$new("state 5", Support$new(Domain$new(0, +Inf), "continuous"))
                                     
                                     lsParams <- list(state1, state2, state3, state4, state5)
                                     super$initialize("scenario.multinomial5", "Transition probabilties (5 states)",ScenarioParameterList$new(lsParams))
                                   },
                                   simulate = function(simSize, size, cache, reps){
                                     
                                     parsState1 <- private$paramList$getValueOfParameterWithName("state 1", simSize, cache, reps)
                                     parsState2 <- private$paramList$getValueOfParameterWithName("state 2", simSize, cache, reps)
                                     parsState3 <- private$paramList$getValueOfParameterWithName("state 3", simSize, cache, reps)
                                     parsState4 <- private$paramList$getValueOfParameterWithName("state 4", simSize, cache, reps)
                                     parsState5 <- private$paramList$getValueOfParameterWithName("state 5", simSize, cache, reps)
                                     
                                     dfParams <- data.frame(parsState1, parsState2, parsState3, parsState4, parsState5)
                                     total.individual <- apply( dfParams,1, sum)
                                     probs <- ( dfParams / total.individual) 
                                     
                                     simulatedMultinom <- t(apply(as.matrix(probs),1, FUN=function(x){
                                       return(rmultinom(1, size , x)/size)
                                     } ))
                                     
                                     simulated  <- matrix(ncol=5,nrow=simSize * reps)
                                     simulated[,1:5] <- simulatedMultinom
                                     
                                     return(simulated)
                                     
                                   },getSimulatedParameter = function(cache){
                                     
                                     parsprob1 <- cache$psaSampleList$getPsaSampleWithName(private$paramList$getParameterWithName("state 1")$getValue()$getDist())$getData()
                                     parsprob2 <- cache$psaSampleList$getPsaSampleWithName(private$paramList$getParameterWithName("state 2")$getValue()$getDist())$getData()
                                     parsprob3 <- cache$psaSampleList$getPsaSampleWithName(private$paramList$getParameterWithName("state 3")$getValue()$getDist())$getData()
                                     parsprob4 <- cache$psaSampleList$getPsaSampleWithName(private$paramList$getParameterWithName("state 4")$getValue()$getDist())$getData()
                                     parsprob5 <- cache$psaSampleList$getPsaSampleWithName(private$paramList$getParameterWithName("state 5")$getValue()$getDist())$getData()
                                     
                                     SimulatedMatrix <- matrix(ncol=5,nrow=length(parsprob1),byrow=F, c(parsprob1,parsprob2,parsprob3,parsprob4,parsprob5) )
                                     
                                     
                                     return(SimulatedMatrix)
                                   }))$new()
}
