
ScenarioParameter <- R6Class("scenarioParameter", public = list( initialize = function(name, support){
                                                                    private$name <- name
                                                                    private$support <- support
                                                                    },
                                                 getName = function(){
                                                                    return(private$name)
                                                 },
                                                 getSupport = function(){
                                                                    return(private$support)
                                                 },
                                                 setValue =function(val){
                                                                    private$value <- val
                                                 },
                                                 getValue =function(val){
                                                                    return(private$value)
                                                 },
                                                 couldContain = function(psaParam){
                                                                     if(psaParam$getSupport()$getDomain()$getMin() >= self$getSupport()$getDomain()$getMin() && 
                                                                        psaParam$getSupport()$getDomain()$getMax() <= self$getSupport()$getDomain()$getMax() && 
                                                                        !(psaParam$getSupport()$getType()== "continuous" && self$getSupport()$getType()=="discrete")){
                                                                       return(T)
                                                                     }else{
                                                                       return(F)
                                                                     }
                                                 },
                                                 isHidden = function(){
                                                                      return(private$hidden)
                                                 },
                                                 getErrors =function(){

                                                                    errorMessagesList <- list()
                                                                    if(  is.null(private$value) 
                                                                             || is.null(private$value$getValue())
                                                                             || is.na(private$value$getValue())){
                                                                             errorMessagesList[[ length( errorMessagesList) + 1 ]] <- "cannot be empty or contains alpha numeric characters" 
                                                                    }else{
                                                                            if(private$value$getType() == 2){
                                                                              if(private$value$getValue() < private$support$getDomain()$getMin()){
                                                                                errorMessagesList[[ length( errorMessagesList) + 1 ]] <-  paste(private$name, "should be >", private$support$getDomain()$getMin())
                                                                              }
                                                                              if(private$value$getValue() > private$support$getDomain()$getMax()){
                                                                                errorMessagesList[[ length( errorMessagesList) + 1 ]] <- paste(private$name, "should be <", private$support$getDomain()$getMax())
                                                                              }
                                                                              type <- ifelse(private$value$getValue() %% 1 == 0, "discrete", "continuous")
                                                                              if(private$support$getType() == "discrete" && type == "continuous"){
                                                                                errorMessagesList[[ length( errorMessagesList) + 1 ]] <- paste(private$name, "should be ", private$support$getType())
                                                                              }
                                                                            }
                                                                    }
                                                                    return(paste(unlist(errorMessagesList),collapse=", "))
                                                              },
                                                 toString = function(){
                                                   return(paste(private$name," ", private$value$toString()))
                                                 }
                                                 ),
                                  private = list( name = NULL,
                                                  support = NULL, 
                                                  value = NULL,
                                                  hidden = NULL,
                                                  typeChoice = F
                                                  )
                     )
