function() { 
  R6Class("scenario.logitnormal",
                                inherit = Scenario,
                                public = list(
                                  
                                  initialize = function(){
                                    
                                    # Distribution parameters
                                    invlogit <- ScenarioParameter$new("Inverse logit of mean parameter", Support$new(Domain$new(-Inf, +Inf), "continuous"))
                                    varianceLogitNorm <- ScenarioParameter$new("Population variance", Support$new(Domain$new(0, Inf), "continuous"))
                                    
                                    descriptionLatex <- "
                                   This scenario simulates a vector of  value constrained between 0 and 1: \\(z_Q\\) given a vector of mean \\(\\mu_Q\\) from the PsaSample and a specified population variance \\(\\sigma^2\\).</br>
                                   The new sample variance \\(\\sigma^2_z\\) is the population variance times the new sample size \\(n\\).

                                    \\[
                                    Z_Q \\sim logitNormal(\\mu_Q, \\sigma^2 \\times n)
                                    \\]"
                                    
                                    super$initialize("scenario.logitnormal","Logit Normal", ScenarioParameterList$new(list(invlogit,varianceLogitNorm)),descriptionLatex)
                                  },
                                  simulate = function(simSize, size, cache, reps){
                                    parsInvlogit <- private$paramList$getValueOfParameterWithName("Inverse logit of mean parameter",simSize, cache, reps)
                                    parsVarianceLogitNorm <- private$paramList$getValueOfParameterWithName("Population variance",simSize, cache, reps)
                                    
                                    parslogit <- log( parsInvlogit/(1-parsInvlogit) ) 
                                    simLogitNorm <- rnorm(simSize * reps, parslogit , sqrt(parsVarianceLogitNorm/size) )
                                    
                                    simulated  <- matrix(ncol=1,nrow=simSize * reps)
                                    simulated[,1] <- simLogitNorm 
                                    
                                    return(simulated)
                                    
                                  },
                                  getSimulatedParameterName = function(){
                                    return(list(private$paramList$getParameterWithName("Inverse logit of mean parameter")$getValue()$getDist()))
                                  }))$new()
}