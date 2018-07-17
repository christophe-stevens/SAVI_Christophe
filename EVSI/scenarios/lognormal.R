function() { 
  R6Class("scenario.lognormal",
          inherit = Scenario,
          public = list(
            
            initialize = function(){
              
              # Distribution parameters
              expectMeanlog <- ScenarioParameter$new("Exponentiated Mean parameter", Support$new(Domain$new(0, +Inf), "continuous"))
              varianceLognorm <- ScenarioParameter$new("Population log variance", Support$new(Domain$new(0, Inf), "continuous"))
              
              descriptionLatex <- "
              This scenario simulate value originating from a lognormal distribution with fixed variance
              \\[
              Z_Q \\sim logNormal(\\logmu_Q, \\logsigma^2 \\times n)
              \\]"
              
              super$initialize("scenario.lognormal","Lognormal", ScenarioParameterList$new(list(expectMeanlog,varianceLognorm)),descriptionLatex)
            },
            simulate = function(simSize, size, cache, reps){
              parsexpectMeanlog  <- private$paramList$getValueOfParameterWithName("Exponentiated Mean parameter",simSize, cache, reps)
              parsvarianceLognorm <- private$paramList$getValueOfParameterWithName("Population log variance",simSize, cache, reps)
              
              mu <- parsexpectMeanlog - (parsvarianceLognorm / size)/2
              simLognorm <- rnorm(simSize * reps, mu , sqrt(parsvarianceLognorm/size) )
              
              simulated  <-  matrix(ncol=1,nrow=simSize * reps)
              simulated[,1] <- simLognorm 
              
              return(simulated)
              
            },getSimulatedParameter = function(cache){
              par<- cache$psaSampleList$getPsaSampleWithName(private$paramList$getParameterWithName("Exponentiated Mean parameter")$getValue()$getDist())$getData()
              
              SimulatedMatrix <- matrix(ncol=1,byrow=F, c(par) )
              
              return(SimulatedMatrix)
            }))$new()
  
}