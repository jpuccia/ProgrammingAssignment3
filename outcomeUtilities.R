get30DayMortalityRateByRank <- function(state, outcome, hospitalData, rank = "best"){
        hospitalName <- "NA"
        
        ## Get the outcomeIndex from the given outcome if valid
        outcomeIndex <- getOutcomeIndex(outcome)
        
        ## compute the rankIndex based on the relative or specific ranking
        rankIndex <- getRankIndex(rank)
        
        ## Set indexes for state and hospital name
        stateIndex <- 7
        hospitalNameIndex <- 2
        
        ## Coerce mortality data to numeric and suppress an warnings
        hospitalData[,outcomeIndex] <- suppressWarnings(as.numeric(hospitalData[,outcomeIndex]))
        
        ## Filter the data to only include rows with non-NA mortality data
        hospitalData <- hospitalData[complete.cases(hospitalData[,outcomeIndex]),]
        
        ## Filter the data for the state we're interested in
        stateData <- hospitalData[hospitalData[,stateIndex] == state,]
        
        if (rankIndex <= nrow(stateData)){
                ## Sort the stateData by the motality rate and the hospital name
                if (!is.numeric(rank) && rank == "worst"){
                        stateData <- stateData[order(-stateData[,outcomeIndex], stateData[,hospitalNameIndex]),]
                }
                else{
                        stateData <- stateData[order(stateData[,outcomeIndex], stateData[,hospitalNameIndex]),]
                }
                
                ## Return the name of the hospital in the first row
                hospitalName <- stateData[rankIndex,hospitalNameIndex]
        }
        
        hospitalName
}

getRankIndex <- function(rank){
        rankIndex <- -1
        
        if (is.numeric(rank)){
                rankIndex <- as.integer(rank)
        }
        else if (rank == "best" || rank == "worst"){
                rankIndex <- 1
        }
}

## validState
##
## Description: Validates a two character state against data 
##... from the outcome-of-care-measures.csv file.
##
## Parameters:
##      state: A two character state abbreviation
##      hospitalData: A dataframe of data from the 
##... outcome-of-care-measures.csv file of Programming Assignment 3.
##
## Returns: TRUE if state exists as a state in the hospitalData dataframe
##
validState <- function(state, hospitalData){
        ## Get the distinct state names from the hospitalData
        states <- unique(hospitalData[,7])
        state %in% states
}

## getOutcomeIndex
##
## Description: Returns a column index that corresponds to a valid 
##... outcome phrase.
## 
## Parameters:
##      outcome: The outcome phrase that correspond to a column index in the
##... outcome-of-care-measures.csv file.
## 
## Returns:
##      The column index corresponding to a valid outcome phrase, or
##... an "invalid outcome" error is thrown if the given outcome phrase
##... is invalid
## 
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
