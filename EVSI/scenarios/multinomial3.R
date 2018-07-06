function() { 
  R6Class("scenario.multinomial3",
                                 inherit = Scenario,
                                 public = list(
                                   
                                   initialize = function(){
                                     
                                     # Distribution parameters
                                     state1 <- ScenarioParameter$new("state 1", Support$new(Domain$new(0, +Inf), "continuous"))
                                     state2 <- ScenarioParameter$new("state 2", Support$new(Domain$new(0, +Inf), "continuous"))
                                     state3 <- ScenarioParameter$new("state 3", Support$new(Domain$new(0, +Inf), "continuous"))
                                     
                                     lsParams <- list(state1, state2, state3)
                                     super$initialize("scenario.multinomial3", "Transition probabilties (3 states)",ScenarioParameterList$new(lsParams))
                                   },
                                   simulate = function(simSize, size, cache, reps){
                                     
                                     parsState1 <- private$paramList$getValueOfParameterWithName("state 1", simSize, cache, reps)
                                     parsState2 <- private$paramList$getValueOfParameterWithName("state 2", simSize, cache, reps)
                                     parsState3 <- private$paramList$getValueOfParameterWithName("state 3", simSize, cache, reps)
                                     
                                     dfParams <- data.frame(parsState1, parsState2, parsState3)
                                     total.individual <- apply( dfParams,1, sum)
                                     probs <- ( dfParams / total.individual) 
                                     
                                     simulatedMultinom <- t(apply(as.matrix(probs),1, FUN=function(x){
                                       return(rmultinom(1, size , x)/size)
                                     } ))
                                     
                                     simulated  <- matrix(ncol=3,nrow=simSize * reps)
                                     simulated[,1:3] <- simulatedMultinom
                                     
                                     return(simulated)
                                     
                                   },getSimulatedParameterName = function(){
                                     return(list(private$paramList$getParameterWithName("state 1")$getValue()$getDist(),
                                                 private$paramList$getParameterWithName("state 2")$getValue()$getDist(),
                                                 private$paramList$getParameterWithName("state 3")$getValue()$getDist()))
                                   }))$new()
}
