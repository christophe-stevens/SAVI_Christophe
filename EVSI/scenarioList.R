ScenarioList <- R6Class("scenarioList", 
                        public = list(
                          initialize = function(){
                            dat.files  <- list.files(path="./EVSI/scenarios/",
                                                     recursive=T,
                                                     full.names=T)
                            sapply(dat.files, FUN = function(x){
                              fn <- dget(x)
                              private$add(fn())
                            })
                           
                          },
                          addScenario = function(scenario){
                            private$add(scenario)
                          },
                          getScenariosNames = function(){
                            return(unlist(lapply(private$scenariosList, FUN =function(x) x$getName())))
                          },
                          getScenariosFriendlyNames = function(){
                            return(unlist(lapply(private$scenariosList, FUN =function(x) x$getFriendlyName())))
                          },
                          getScenarioAt = function(index){
                            return(private$scenariosList[[index]])
                          },
                          getScenarioWithName = function(name){
                            return(self$getScenarioAt(which(self$getScenariosNames()==name)))
                          },
                          getScenarioWithFriendlyName = function(name){
                            return(self$getScenarioAt(which(self$getScenariosFriendlyNames()==name)))
                          }
                        ),
                        private = list(
                          scenariosList = list(),
                          add = function(dist){
                            private$scenariosList[[ length(private$scenariosList) + 1]] <- dist
                          }
                        ))
