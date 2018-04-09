
Scenario <- R6Class("scenario",  public = list( initialize = function(name, friendlyName,  paramList, descriptionLatex = ""){
                                                                        private$name <- name
                                                                        private$friendlyName <- friendlyName
                                                                        private$paramList <- paramList
                                                                        private$descriptionLatex <- descriptionLatex
                                                        },
                                                        getErrorMsg = function(){
                                                                        if(!is.null(private$paramList)){
                                                                          return(private$paramList$getErrorMsg())
                                                                        }else{
                                                                          return("")
                                                                        }
                                                        },
                                                        simulate = function(simSize, size, cache, reps){
                                                                        cat("not implemented in Super class!")
                                                        },
                                                        getName = function(){
                                                                        return(private$name)
                                                        },
                                                        getParamList = function(){
                                                                        return(private$paramList)
                                                        },
                                                        getParam = function(paramName){
                                                                      return(private$paramList$getParameterWithName(paramName))
                                                        },
                                                        getDomId = function(){
                                                                        return(gsub("\\.","",gsub(" ", "", private$name)))
                                                        },
                                                        getFriendlyName = function(){
                                                                        return(private$friendlyName)
                                                        },
                                                        getDescriptionLatex = function(){
                                                          return(private$descriptionLatex)
                                                        }
                                                    )
                                        , private = list( name = NULL,
                                                         paramList = NULL,
                                                         friendlyName = NULL,
                                                         descriptionLatex = "")
                        )

