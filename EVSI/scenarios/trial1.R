function() { 
  
  R6Class("scenario.trial1",
                           inherit = Scenario,
                           public = list(
                             
                             initialize = function(){
                               
                               # Distribution parameters
                               probT <- ScenarioParameter$new("probability arm 1", Support$new(Domain$new(0, 1), "continuous"))
                               probC <- ScenarioParameter$new("probability arm 2", Support$new(Domain$new(0, 1), "continuous"))
                               
                               descriptionLatex <- "
                                This scenario simulates a trial where a probability of efficacy or safety is obtained for each treatment arm. </br>
                                First counts of events for the treatment arm (\\(Z_{P_T}\\))  are generated via a binomial distribution based on the \\(P_T\\) PSA sample parameter and half of the new trial sample size \\(n_T\\).</br>
                                Then the simulated count of events are divided by half the new trial sample size \\(n_T\\). 
                                \\[
                                    Z_{P_T} \\sim \\dfrac{Bin(n_T, P_T)}{n_T} 
                               \\]
                               Simulated probabilities for the other treatment/control arm is obtained similarily
                               \\[
                                    Z_{P_C} \\sim \\dfrac{Bin(n_C, P_C)}{n_C} 
                               \\]
                               The total sample size required is then twice the ideal sample size shown on the plot. 
                               "
                               
                               super$initialize("scenario.trial1", "Two probabilities", ScenarioParameterList$new(list(probT, probC)), descriptionLatex)
                             },
                             simulate = function(simSize, size, cache, reps){
                               parsprobT <- private$paramList$getValueOfParameterWithName("probability arm 1",simSize, cache, reps)
                               parsprobC <- private$paramList$getValueOfParameterWithName("probability arm 2",simSize, cache, reps)
                               
                               simProbParT <- rbinom(simSize * reps, size, parsprobT) / size
                               simProbParC <- rbinom(simSize * reps, size, parsprobC) / size
                               
                               simulated  <- matrix(ncol=2,nrow=simSize * reps)
                               simulated[,1]<-simProbParT
                               simulated[,2]<-simProbParC
                               
                               return(simulated)
                               
                             }))$new()
}
