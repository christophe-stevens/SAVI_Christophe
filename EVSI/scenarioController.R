
ScenarioController  <- R6Class("scenarioController", public= list(
                                                errorMsg ="", 
                                                getErrorMsg = function(){
                                                  # Get errors that exists for the input values of likelihood 
                                                  # distribution parameters. 
                                                  #
                                                  # Returns:
                                                  #   String, return a comma separated string of errors
                                                  return(private$scenario$getErrorMsg())
                                                },
                                                setScenario = function(scenario, cache, input, output){
                                                    private$scenario <- scenario$clone(TRUE)
                                                    private$drawScenario(cache, input, output)
                                                },
                                                clearScenario = function(cache, input, output){
                                                  # Remove all likelihood pane on the evsi calculator. 
                                                  #
                                                  # Args:
                                                  #   cache: all the objects stored for current user.
                                                  #   input: Input object to listen to the view.
                                                  #   output: Output object list to manipulate view.
                                                  #
                                                  # Returns:
                                                  #   void, doesn't return anything, it remove a likelihood from the screen
                                                  removeUI(selector = paste("#",private$scenario$getDomId(), sep=""))
                                                  private$scenario <- NULL
                                                },
                                                calculateEVPI = function(session, cache, input, replications){
                                                  cat("CALCULATE EVPI")
                                                  SimulatedMatrix <-  private$scenario$getSimulatedParameter(cache)
                                                  
                                                  lambda <- input$lambdaOverall # re-run if labmda changes
                                                  inb   <- createInb(cache$costs, cache$effects, lambda)
                                                  inb <- inb[rep(seq_len(dim(inb)[1]), each=replications),]
                                                  
                                                  N <- cache$psaSampleList$getMinRowCount() 
                                                  
                                                  progress <- shiny::Progress$new(session, min = 1, max = 1)
                                                  on.exit(progress$close())
                                                  progress$set(message = 'Calculation in progress',
                                                               detail = 'Please wait...')
                                                  
                                                  SimulatedMatrix <- SimulatedMatrix[rep(seq_len(dim(SimulatedMatrix)[1]), each=replications),]

                                                  SimulatedData  <- as.data.frame(SimulatedMatrix )
                                                  colnames(SimulatedData ) <- LETTERS[1:dim(SimulatedData)[2]]

                                                  if(dim(SimulatedData)[2]<=4){
                                                    results <- private$calculateEVSIGAM(inb,SimulatedData, replications, N)
                                                  }else{
                                                    # use GP
                                                    if(replications>1)
                                                      stop("cerr: When more than 4 parameters are used, replications cannot be used. (due to matrix size)")
                                                    results <- private$calculateEVSIGP(inb,SimulatedData, replications, N, session)
                                                  }
                                                 
                                                  return(results)
                                                },
                                                calculateEVSI = function(session, cache, input, nsample, replications){
                                                   lambda <- input$lambdaOverall # re-run if labmda changes
                                                   inb   <- createInb(cache$costs, cache$effects, lambda)
                                                   
                                                   N <- cache$psaSampleList$getMinRowCount() 
                                                   
                                                   # Initialise result array to contain evsi estimation by sample size
                                                   output.evsi.by.sample.size <- matrix(rep(length(nsample) * 3), ncol = 3, nrow = length(nsample))
                                                   
                                                   # Set Progress bar
                                                   progress <- shiny::Progress$new(session, min = 1, max = length(nsample))
                                                   on.exit(progress$close())
                                                   progress$set(message = 'Calculation in progress',
                                                                detail = 'Please wait...')
                                                   
                                                   # Replicate inb. 
                                                   inb <- inb[rep(seq_len(dim(inb)[1]), each=replications),]
                                                   for ( curr.n in 1:length(nsample) )
                                                   {
                                                      SimulatedData <- private$simulate(N, nsample[curr.n], cache, replications)
                                                      
                                                      resultsForNs <- list()
                                                      # Use GAM for fewer than 5 variables
                                                      if(dim(SimulatedData)[2]<=4){
                                                        resultsForNs <- private$calculateEVSIGAM(inb,SimulatedData, replications, N)
                                                      }else{
                                                        # use GP
                                                        if(replications>1)
                                                          stop("cerr: When more than 4 parameters are used, replications cannot be used. (due to matrix size)")
                                                        resultsForNs <- private$calculateEVSIGP(inb,SimulatedData, replications, N, session)
                                                      }
                                                      
                                                      output.evsi.by.sample.size[curr.n, 1]<- nsample[curr.n]
                                                      output.evsi.by.sample.size[curr.n, 2]<- resultsForNs$evsi
                                                      output.evsi.by.sample.size[curr.n, 3]<-  resultsForNs$SE
                                                      
                                                      # Update user on progress 
                                                      progress$set(curr.n)
                                                      
                                                   }
                                                   
                                                   return(output.evsi.by.sample.size)
                                                 }),
                                private = list( scenario = NULL, 
                                                drawScenario = function(cache, input, output){
                                                  # Insert a new likelihood pane on the evsi calculator. 
                                                  #
                                                  # Args:
                                                  #   cache: all the objects stored for current user.
                                                  #   input: Input object to listen to the view.
                                                  #   output: Output object list to manipulate view.
                                                  #
                                                  # Returns:
                                                  #   void, doesn't return anything, it adds a likelihood to screen
                                                  insertUI(selector = "#scenario",
                                                           where = "beforeEnd",
                                                           ui =  likelihoodControl(private$scenario, cache, input, output),
                                                           immediate	= T )
                                                  output$has.likelihoods <- reactive({
                                                    cache$scenarioController$hasScenario
                                                  })
                                                },
                                                simulate = function(simSize,  size, cache, reps){
                                                  # Assume only one likelihood by scenario
                                                  simulatedMatrix <- private$scenario$simulate(simSize, size, cache, reps)
                                                  simulatedMatrix  <- as.data.frame(simulatedMatrix )
                                                  colnames(simulatedMatrix ) <- LETTERS[1:dim(simulatedMatrix)[2]]
                                                  
                                                  return(simulatedMatrix )
                                                },
                                                getGamFormulaForDataFrameNames =function(LikelihoodsNames){
                                                  form <- paste(LikelihoodsNames, ",", sep="", collapse="")
                                                  form <- substr(form, 1, nchar(form) - 1)
                                                  if (length(LikelihoodsNames) > 4 ) {
                                                    form <- paste("te(", form, ",k=-1)", sep="") # restrict to 4 knots if 4 params
                                                  } else if (length(LikelihoodsNames) == 4 ) {
                                                    form <- paste("te(", form, ",k=4)", sep="") # restrict to 4 knots if 4 params
                                                  } else {
                                                    form <- paste("te(", form, ")", sep="")    
                                                  }
                                                  return(form)
                                                },
                                                calculateEVSIGAM = function(inb, SimulatedData, replications, N){
                                                  
                                                  D <- ncol(inb)
                                                  
                                                  # Simulation size 
                                                  s <- 10^3
                                                  
                                                  
                                                  g.hat <- beta.hat <- Xstar <- V <- tilde.g <- vector("list", D)
                                                  g.hat[[1]] <- 0
                                                  
                                                  
                                                  regression.model <- private$getGamFormulaForDataFrameNames(colnames(SimulatedData))
                                                  
                                                  for( d in 2:D) {
                                                    dependent <- inb[, d]
                                                    
                                                    data <- cbind(dependent, SimulatedData )
                                                    
                                                    f <- update(formula(dependent~.), formula(paste(".~", regression.model)))
                                                    model <- gam(f, data = data)
                                                    g.hat[[d]] <- model$fitted
                                                    beta.hat[[d]] <- model$coef
                                                    Xstar[[d]] <- predict(model,type="lpmatrix")
                                                    V[[d]] <- model$Vp
                                                  }
                                                  # Compute EVSI
                                                  perfect.info <- mean(do.call(pmax, g.hat)) 
                                                  baseline <- max(unlist(lapply(g.hat, mean)))
                                                  evsi <- perfect.info - baseline
                                                  rm(g.hat); gc()
                                                  
                                                  
                                                  for(d in 2:D) {
                                                    sampled.coef <- mvrnorm(s, beta.hat[[d]], V[[d]])
                                                    tilde.g[[d]] <- sampled.coef%*%t(Xstar[[d]])  
                                                  }
                                                  
                                                  tilde.g[[1]] <- matrix(0, nrow = s, ncol = N * replications)
                                                  rm(V, beta.hat, Xstar, sampled.coef);gc()
                                                  
                                                  sampled.perfect.info <- rowMeans(do.call(pmax, tilde.g))
                                                  sampled.baseline <- do.call(pmax, lapply(tilde.g, rowMeans)) 
                                                  rm(tilde.g); gc()
                                                  sampled.evsi <- sampled.perfect.info - sampled.baseline
                                                  SE <- sd(sampled.evsi)

                                                  return(list(evsi= evsi , SE=SE))
                                                  
                                                },
                                                calculateEVSIGP = function(inb, SimulatedData, replications, N, session) {

                                                  maxSample <- min(5000, nrow(inb)) # to avoid trying to invert huge matrix
                                                  inb <- as.matrix(inb[1:maxSample, ])
                                                  D <- ncol(inb)
                                                  s <- 10^3
                                                  
                                                  input.matrix <- as.matrix(SimulatedData[1:maxSample, , drop=FALSE])
                                                  colmin <- apply(input.matrix, 2, min)
                                                  colmax <- apply(input.matrix, 2, max)
                                                  colrange <- colmax - colmin
                                                  input.matrix <- sweep(input.matrix, 2, colmin, "-")
                                                  input.matrix <- sweep(input.matrix, 2, colrange, "/")
                                                  N <- nrow(input.matrix)
                                                  p <- ncol(input.matrix)
                                                  H <- cbind(1, input.matrix)
                                                  q <- ncol(H)
                                                  
                                                  
                                                  
                                                  m <- min(30 * p, 250)
                                                  m <- min(nrow(inb), m)
                                                  setForHyperparamEst <- 1:m # sample(1:N, m, replace=FALSE)
                                                  hyperparameters <- estimate.hyperparameters(inb[setForHyperparamEst, ], 
                                                                                              input.matrix[setForHyperparamEst, ], session)
                                                  
                                                  V <- g.hat <- vector("list", D)
                                                  g.hat[[1]] <- rep(0, N)

                                                  for(d in 2:D)
                                                  {
                                                    print(paste("estimating g.hat for incremental NB for option", d, "versus 1"))
                                                    delta.hat <- hyperparameters[[d]][1:p]
                                                    nu.hat <- hyperparameters[[d]][p+1]
                                                    A <- makeA.Gaussian(input.matrix, delta.hat)
                                                    Astar <- A + nu.hat * diag(N)
                                                    Astarinv <- chol2inv(chol(Astar))
                                                    rm(Astar); gc()
                                                    AstarinvY <- Astarinv %*% inb[, d]
                                                    tHAstarinv <- t(H) %*% Astarinv
                                                    tHAHinv <- solve(tHAstarinv %*% H + 1e-7* diag(q))
                                                    betahat <- tHAHinv %*% (tHAstarinv %*% inb[, d])
                                                    Hbetahat <- H %*% betahat
                                                    resid <- inb[, d] - Hbetahat
                                                    g.hat[[d]] <- Hbetahat+A %*% (Astarinv %*% resid)
                                                    AAstarinvH <- A %*% t(tHAstarinv)
                                                    sigmasqhat <- as.numeric(t(resid) %*% Astarinv %*% resid)/(N - q - 2)
                                                    V[[d]] <- sigmasqhat*(nu.hat * diag(N) - nu.hat ^ 2 * Astarinv +
                                                                            (H - AAstarinvH) %*% (tHAHinv %*% t(H - AAstarinvH)))
                                                    rm(A, Astarinv, AstarinvY, tHAstarinv, tHAHinv, betahat, Hbetahat, resid, sigmasqhat);gc()
                                                  }
                                                  perfect.info <- mean(do.call(pmax, g.hat)) 
                                                  baseline <- max(unlist(lapply(g.hat, mean)))
                                                  
                                                  evsi <- perfect.info - baseline
                                                  
                                                  print("Computing standard error via Monte Carlo")
                                                  tilde.g <- vector("list", D)
                                                  tilde.g[[1]] <- matrix(0, nrow=s, ncol=N * replications)     
                                                  
                                                  
                                                  for(d in 2:D) {
                                                    tilde.g[[d]] <- mvrnorm(s, g.hat[[d]][1:(min(N, 1000))], V[[d]][1:(min(N, 1000)), 1:(min(N, 1000))])
                                                  }
                                                  
                                                  sampled.perfect.info <- rowMeans(do.call(pmax, tilde.g))
                                                  sampled.baseline <- do.call(pmax, lapply(tilde.g, rowMeans)) 
                                                  rm(tilde.g);gc()
                                                  sampled.partial.evpi <- sampled.perfect.info - sampled.baseline
                                                  SE <- sd(sampled.partial.evpi)

                                                  rm(V, g.hat);gc()
                                                  return(list(evsi=evsi, SE=SE))
                                                  
                                                })
                    )
