
ScenarioParameterList<- R6Class("scenarioParameterList", public = list( initialize = function(parameterList){
                                  private$parameters <- parameterList
                                },
                                getParametersCount = function(){
                                  return(length(private$parameters))
                                },
                                getParameterAt = function(index){
                                  return(private$parameters[[index]])
                                },
                                getParameterWithName = function(name){
                                  paramNames <- unlist(lapply(private$parameters, FUN = function(x) x$getName()))
                                  paramOfInterest <- self$getParameterAt( which(paramNames == name))
                                  return(paramOfInterest)
                                },
                                getValueOfParameterWithName = function(name, sim, cache, reps){
                                  paramNames <- unlist(lapply(private$parameters, FUN = function(x) x$getName()))
                                  paramOfInterest <- self$getParameterAt( which(paramNames == name))
                                  if(paramOfInterest$getValue()$getType() == 2){
                                    initials <- rep(paramOfInterest$getValue()$getValue(), sim)
                                    extended <- initials[rep(seq_len(length(initials)), each=reps)]
                                    return(extended)
                                  }else if (paramOfInterest$getValue()$getType() == 3){
                                    initials <- cache$psaSampleList$getPsaSampleWithName(paramOfInterest$getValue()$getValue())$getData()[1:sim]
                                    extended <- initials[rep(seq_len(length(initials)), each=reps)]
                                    return(extended)
                                  } else{
                                    return(NULL)
                                  }
                                },
                                getAllParameters = function(){
                                  return(private$parameters)
                                },
                                getErrorMsg = function(){
                                  return(paste(unlist(lapply(private$parameters, FUN = function(x) x$getErrors())), collapse=""))
                                }
                                ),
                                private = list( parameters = list(),
                                                deep_clone = function(name, value){
                                                  if(name == "parameters"){
                                                    listParam <- list()
                                                    for(par in private$parameters){
                                                      listParam[[length(listParam)+1]] <- par$clone(deep=TRUE)
                                                    }
                                                    return(listParam)
                                                  }else{
                                                    return(value)
                                                  }
                                                }
                                )
)

