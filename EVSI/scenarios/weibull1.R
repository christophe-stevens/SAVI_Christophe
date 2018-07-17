function() { 
  R6Class("scenario.weibull",
          inherit = Scenario,
          public = list(
            
            initialize = function(){
              
              # Distribution parameters
              timetoEvent <- ScenarioParameter$new("Time-to-event", Support$new(Domain$new(0, +Inf), "continuous"))
              rate <- ScenarioParameter$new("Shape parameter", Support$new(Domain$new(0, +Inf), "continuous"))
              
              descriptionLatex <- "
              This scenario simulates a time to an event: \\(Z_T\\) given a time to event in the PsaSample \\(T\\) and a specified shape parameter\\(k\\). </br>
              The scale parameter of the likelihood \\(\\lambda \\) is derived by using the observed time-to-event as the expectation of a weibull distribution:
              
              \\[
              \\lambda = \\dfrac{T}{\\Gamma(1+\\dfrac{1}{k})}
              ]\\
              Obtaining a new sample is now
              \\[
              Z_T \\sim weibul(\\lambda, k)
              \\]"
              
              super$initialize("scenario.weibull","Time to Event (weibull with known shape)", ScenarioParameterList$new(list(timetoEvent, rate)),descriptionLatex)
            },
            simulate = function(simSize, size, cache, reps){
              parTimeToEvent <- private$paramList$getValueOfParameterWithName("Time-to-event",simSize, cache, reps)
              parShape <- private$paramList$getValueOfParameterWithName("Shape parameter",simSize, cache, reps)
              
              parScale <- parTimeToEvent / gamma(1+1/parShape)
              
              sims <- apply(data.frame(shape=parShape, scale=parScale),1, FUN =function(x) median(rweibull(size, x[1] , x[2] )))
              
              simulated  <- matrix(ncol=1,nrow=simSize * reps)
              simulated[,1] <- sims
              
              return(simulated)
              
            },getSimulatedParameter = function(cache){
              parsprob<- cache$psaSampleList$getPsaSampleWithName(private$paramList$getParameterWithName("Time-to-event")$getValue()$getDist())$getData()
              
              SimulatedMatrix <- matrix(ncol=1,byrow=F, c(parsprob) )
              
              return(SimulatedMatrix)
            }))$new()
}