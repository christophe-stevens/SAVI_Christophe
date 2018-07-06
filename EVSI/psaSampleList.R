
PsaSampleList <- R6Class("psaSampleList", public = list(
                    initialize = function(theta=NULL){
                      if(!is.null(theta)){
                        psaNames  <- colnames(theta)
                        for (col in 1:dim(theta)[2]){
                          self$add(PsaSample$new(psaNames[col], col, theta[,col]))
                        }
                      }
                    },
                    add = function(newPsaSample) {
                      if(is.null(private$parameters)){
                        private$parameters <-list()
                      }
                      private$parameters[[length(private$parameters)+1]] <- newPsaSample
                    },
                    getPsaSampleNames= function(){
                      return(unlist(lapply(private$parameters, FUN = function(x) x$getName())))
                    },
                    getPsaSampleAtIndex = function(index){
                      return(private$parameters[[index]])
                    },
                    getPsaSampleWithName = function(name){
                      return(self$getPsaSampleAtIndex(which(self$getPsaSampleNames()==name)))
                    },
                    getPsaSampleMatchingParameter= function(param){
                      matchingPsaSampleList <- PsaSampleList$new()
                      for(currPsa in private$parameters){
                        if(param$couldContain(currPsa)){
                          matchingPsaSampleList$add(currPsa$clone())
                        }
                      }
                      return(matchingPsaSampleList)
                    },
                    # Return the size of the shortest psaSample in the PsaSampleList
                    getMinRowCount = function(){
                      return(min(unlist(lapply(private$parameters, FUN = function(x) x$getSize()))))
                    },
                    getSize = function(){
                      return(length(private$parameters))
                    }
                  ),
                  private = list(parameters=list())
)
