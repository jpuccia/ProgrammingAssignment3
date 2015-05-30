rankhospital <- function(state, outcome, num = "best") {
        result <- "no data"
        
        ## Read outcome data
        hospitalData <- read.csv("outcome-of-care-measures.csv", 
                                 colClasses = "character")
        
        ## If the given state is valid...
        if (!validState(state, hospitalData)){
                stop("invalid state")
        }
        else{
                ## Return hospital name in that state with the given
                ##... num ranking
                result <- get30DayMortalityRateByRank(state, 
                                                      outcome, 
                                                      hospitalData,
                                                      rank = num)
        }
        
        ## Return the name of the hospital with the given mortailty rate
        ##... ranking for the given state and outcome
        result
}