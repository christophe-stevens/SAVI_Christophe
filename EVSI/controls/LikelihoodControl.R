
#' @param likelihood The scenario Object To add
#' @param cache the cache for accessing other parameters
#' @param input the input to setup observers
#' @param output The output to display errors and messages
#' 
likelihoodControl <- function(scenario, cache, input, output) {
  
  # Create DomIds for parameters components
  messageDomId <- paste(scenario$getDomId(), "msg", sep="")
  
  # Get parameters list and 
  parameters <- scenario$getParamList()$getAllParameters()
  parameterCount = length(parameters)
  #columnsPerparameter = 12/(parameterCount+1)
  
  # Setup components size (depends on number of parameters)
  newtags <- tag("div", varArgs=list(class="row", style="border-bottom:  1px white solid;padding-top:25px; margin-bottom:10px;", id = scenario$getDomId() ))
  events <- list()
  
  
  # Loop through parameters and create html/js script
  for(param in scenario$getParamList()$getAllParameters()){
   
                  coltagparRow <- tag("div", varArgs=list(class="row"))
    
                  # Create DomIds for parameters components
                  paramDomId <- paste(scenario$getDomId(), param$getName(), sep="")
                  choiceDomId <- paste(paramDomId,"choice",sep="")
                  choiceNumDomId <- paste(choiceDomId ,"num",sep="")
                  choiceDistDomId <- paste(choiceDomId ,"dist",sep="")
                  
                  # Set divs for components 
                  coltagparChoice<- tag("div", varArgs=list(class=paste("col-sm-",6 ,sep="")))
                  coltagpar <- tag("div", varArgs=list(class=paste("col-sm-",6 ,sep="")))
                  
                  # Get support variable for parameter
                  min <- param$getSupport()$getDomain()$getMin()
                  max <- param$getSupport()$getDomain()$getMax()
                  step <- param$getSupport()$getStep()
                  
                  # Find suitable psa Sample for parameters and display numeric choice if none. 
                  psaSampleListMatchingParam <- cache$psaSampleList$getPsaSampleMatchingParameter(param)
                  # Create  a new value for this parameter. 
                  # (CANNOt BE CREATED BEFORE/ Problem duplicate reactiveValues)
                  param$setValue( Value$new() )
                  # If no suitable psa param for parameters display numeric choice. 
                  # otherwise display psa sample with default value set to first PSA sample of the list. 
                  if(psaSampleListMatchingParam$getSize()==0){
                    param$getValue()$setType(2)
                    param$getValue()$setNum(0)
                  }else {
                    param$getValue()$setType(3)
                    param$getValue()$setDist(psaSampleListMatchingParam$getPsaSampleAtIndex(1)$getName())
                  }
                  
                  # Add the numeric choice component
                  coltagpar <- tagAppendChild(coltagpar, conditionalPanel(paste("input['", choiceDomId, "'] == 2", sep=""),
                                                                          numericInput(choiceNumDomId, "value:", 
                                                                                       value = param$getValue()$getNum(), 
                                                                                       min = min, max = max, step = step)))
                  
                  # Add the distribution choice component
                  coltagpar <- tagAppendChild(coltagpar, conditionalPanel(paste("input['", choiceDomId, "'] == 3", sep=""),
                                                                          selectInput(choiceDistDomId, "value", 
                                                                                      choices = psaSampleListMatchingParam$getPsaSampleNames(), 
                                                                                      selected = param$getValue()$getDist())))

                  
                  # Add a switch between Numeric and Distribution choice
                  coltagparChoice <- tagAppendChild(coltagparChoice,  radioButtons(choiceDomId, label =  tags$u(param$getName()),
                                                                 choices = list("num." = 2, "dist." = 3), 
                                                                 selected = param$getValue()$getType()))
                  
                  ## Check Value input when numerical
                  local({ 
                    choiceNumDomId_str <- paste0(choiceNumDomId)
                    choiceDomId_str <- paste0(choiceDomId)
                    choiceDistDomId_str <- paste0(choiceDistDomId)
                    paramName_str <- paste0(param$getName())
                    likelihood_str <- paste0(scenario$getName())
                    messageDomId_str <- paste0(messageDomId)
                    
                    observeEvent(input[[choiceNumDomId_str]],{
                      # fetch parameters ( since it is a loop the environment changes! )
                      currentParam <- scenario$getParamList()$getParameterWithName(paramName_str)
                      
                      # Update value
                      currentParam$getValue()$setType(input[[choiceDomId_str]])
                      currentParam$getValue()$setNum(input[[choiceNumDomId_str]])
                      
                      # Debug purpose
                      # print(paste(choiceDomId_str, " is ",  paramIni$getValue()$toString()))
                      
                      # Update errors message on screen
                      output[[messageDomId_str]]<-reactive({
                        scenario$getErrorMsg()
                      })
                    })
                    
                    observeEvent(input[[choiceDomId_str]],{
      
                      # fetch parameters ( since it is a loop the environment changes! )
                      currentParam <- scenario$getParamList()$getParameterWithName(paramName_str)
                      
                      # Update value
                      currentParam$getValue()$setType(input[[choiceDomId_str]])
                      
                      # Debug purpose
                      # print(paste(choiceDomId_str, " is ",  paramIni$getValue()$toString()))
                      
                      # Update errors message on screen
                      output[[messageDomId_str]]<-reactive({
                        scenario$getErrorMsg()
                      })
                    })
                    
                    observeEvent(input[[choiceDistDomId_str]],{
                      
                      # fetch parameters ( since it is a loop the environment changes! )
                      currentParam <- scenario$getParamList()$getParameterWithName(paramName_str)
                      
                      # Update value
                      currentParam$getValue()$setType(input[[choiceDomId_str]])
                      currentParam$getValue()$setDist(input[[choiceDistDomId_str]])
                      
                      # Debug purpose
                      # print(paste(choiceDomId_str, " is ",  paramIni$getValue()$toString()))
                      
                      # Update errors message on screen
                      output[[messageDomId_str]]<-reactive({
                        scenario$getErrorMsg()
                      })
                    })
                    
                  })
                  
                  
            
                  ## Add new parameter' s compnents to the html script 
                  coltagparRow  <-  tagAppendChild(coltagparRow, coltagparChoice)
                  coltagparRow <-  tagAppendChild(coltagparRow, coltagpar)
                  newtags <- tagAppendChild(newtags, coltagparRow )

    
  }
 
  # Add remove button to the likelihood pane
  #removeDomId <- paste(likelihood$getDomId(), "delete", sep="")
  #coltag <- tag("div", varArgs=list(class=paste("col-sm-",columnsPerparameter ,sep="")))
  #coltag  <- tagAppendChild(coltag, actionButton(removeDomId, "remove", style = "margin-top:25px;"))
  #newTags <- tagAppendChild(newtags,coltag)
  
  # Add Message text zone for the likelihood pane
  coltagparMsgRow <- tag("div", varArgs=list(class="row"))
  coltagparMsgCol <- tag("div", varArgs=list(class=paste("col-sm-",12 ,sep=""),style="color:red;"))
  coltagparMsgCol  <- tagAppendChild(coltagparMsgCol , textOutput(messageDomId))
  coltagparMsgRow <- tagAppendChild(coltagparMsgRow, coltagparMsgCol )
  newTags <- tagAppendChild(newtags, coltagparMsgRow)
  
  return(newTags)
}
