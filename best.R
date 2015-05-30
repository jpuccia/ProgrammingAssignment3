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
                result <- getBest30DayMortalityRate(state, 
                                                    outcome, 
                                                    hospitalData)
        }
        
        ## Return the name of the hospital with the best mortailty rate
        ##... for the given state and outcome
        result
}

validState <- function(state, hospitalData){
        ## Get the distinct state names from the hospitalData
        states <- unique(hospitalData[,7])
        state %in% states
}

getBest30DayMortalityRate <- function(state, outcome, hospitalData){
        ## Get the outcomeIndex from the given outcome if valid
        outcomeIndex <- getOutcomeIndex(outcome)
        
        ## Set indexes for state and hospital name
        stateIndex <- 7
        hospitalNameIndex <- 2
        
        ## Coerce mortality data to numeric and suppress an warnings
        hospitalData[,outcomeIndex] <- suppressWarnings(as.numeric(hospitalData[,outcomeIndex]))
        
        ## Filter the data to only include rows with non-NA mortality data
        hospitalData <- hospitalData[complete.cases(hospitalData[,outcomeIndex]),]
        
        ## Filter the data for the state we're interested in
        stateData <- hospitalData[hospitalData[,stateIndex] == state,]
        
        ## Sort the stateData by the motality rate and the hospital name
        stateData <- stateData[order(stateData[,outcomeIndex], stateData[,hospitalNameIndex]),]

        ## Return the name of the hospital in the first row
        stateData[1,hospitalNameIndex]
}

getOutcomeIndex <- function(outcome){
        result <- 0
        if (outcome == "heart attack"){
                result <- 11
        }
        else if(outcome == "heart failure"){
                result <- 17
        }
        else if(outcome == "pneumonia"){
                result <- 23
        }
        else{
                stop("invalid outcome")
        }
        
        result
}
