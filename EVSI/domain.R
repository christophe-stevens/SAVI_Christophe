
Domain <- R6Class("domain", public = list( initialize = function(min, max){
                                                            private$min <- min
                                                            private$max <- max
                                                        },
                                           getMin = function(){
                                                            return(private$min)
                                                        },
                                           getMax = function(){
                                                            return(private$max)
                                                        }
                                           ),
                            private = list( min = NULL, 
                                            max = NULL)
                  )

