
Value <- R6Class("value", public =  list( initialize = function(){
                              private$values <-   reactiveValues(dist= NULL , num = 0 , type = 1)
                            },
                            setNum = function(num){
                              private$values$num <- num
                            },
                            setDist = function(dist){
                              private$values$dist <- dist
                            },
                            setType = function(type){
                              # 2 is num
                              # 3 is dist
                              private$values$type <- type
                            },
                            getNum =function(){
                              return(isolate(private$values$num))
                            },
                            getDist = function(){
                              return(isolate(private$values$dist))
                            },
                            getType = function(){
                              return(isolate(private$values$type))
                            },
                            getValue = function(){
                              if(private$values$type == 3){
                                return(self$getDist())
                              }else if(private$values$type==2){
                                return(self$getNum())
                              }else{
                                return(NULL)
                              }
                            },
                            toString  = function(){
                              if(private$values$type == 2){
                                return(paste(" num = ", private$values$num ))
                              }else if(private$values$type == 3){
                                return(paste(" dist = ", private$values$dist))
                              } else {
                                return(paste(" deriv "))
                              }
                            }),
                            private = list( values = NULL)
)