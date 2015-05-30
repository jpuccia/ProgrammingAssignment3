best <- function(state, outcome) {
        result <- "no data"
        
        ## Read outcome data
        hospitalData <- read.csv("outcome-of-care-measures.csv", 
                                 colClasses = "character")
        
        ## If the given state is valid...
        if (!validState(state, hospitalData)){
                stop("invalid state")
        }
        else{
                ## Return hospital name in that state with lowest 30-day death 
                ##... rate
                result <- get30DayMortalityRateByRank(state, 
                                                    outcome, 
                                                    hospitalData,
                                                    rank = "best")
        }
        
        ## Return the name of the hospital with the best mortailty rate
        ##... for the given state and outcome
        result
}
