Support <- R6Class("support", public =  list( initialize = function(domain, type){
                                  private$domain <-domain
                                  private$type <- type
                                },
                                getDomain = function(){
                                  return(private$domain)
                                },
                                getType = function(){
                                  return(private$type)
                                },
                                getStep = function(){
                                  return(ifelse(private$type=="discrete",1,0.0001))
                                }
                                ),
                                private = list( domain = NULL,
                                                type = NULL
                                )
              )
