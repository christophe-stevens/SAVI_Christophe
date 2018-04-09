
PsaSample <- R6Class("psaSample", public = list( initialize = function(name, index, data){
                                                             private$data <- data
                                                             private$name <- name
                                                             private$index <-index
                                                             
                                                             type <- NULL
                                                             if (sum( private$data %% 1 > rep(0, length(private$data)) ) == 0){
                                                               type <- 'discrete'
                                                             }else {
                                                               type <- 'continuous'
                                                             }
                                                             private$support <- Support$new(Domain$new(min(data), max(data)), type)
                                                             
                                                 },
                                                 getName = function(){
                                                            return(private$name)
                                                 },
                                                 getIndex = function(){
                                                            return(private$index)
                                                 },
                                                 getMean = function(){
                                                            return(mean(private$data))
                                                 },
                                                 getSD = function(){
                                                            return(sd(private$data))
                                                 },
                                                 getData = function(){
                                                            return(private$data)
                                                 },
                                                 getSupport = function(){
                                                            return(private$support)
                                                 },
                                                 getSize = function(){
                                                            return(length(private$data))
                                                 }
                                               )
                                , private = list( name = NULL,
                                                  index = NULL,
                                                  data = NULL,
                                                  support = NULL
                                )
                     )
