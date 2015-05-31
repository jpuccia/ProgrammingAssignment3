rankall <- function(outcome, num = "best") {
        ## Validate the parameters first for efficiency
        outcomeIndex <- getOutcomeIndex(outcome)
        rankIndex <- getRankIndex(num)
        
        ## Set indexes for state and hospital name
        stateIndex <- 7
        hospitalNameIndex <- 2
        
        ## Read outcome data with only the hospital, state and outcome data
        hospitalData <- 
                read.csv("outcome-of-care-measures.csv", 
                colClasses = "character")[,c(hospitalNameIndex,stateIndex,outcomeIndex)]
        
        ## Rename columns by number to something simpler
        names(hospitalData)[1] <- "hospital"
        names(hospitalData)[2] <- "state"
        names(hospitalData)[3] <- "outcome"
        
        ## Coerce outcome data to numeric and suppress an warnings
        hospitalData$outcome <- suppressWarnings(as.numeric(hospitalData$outcome))
        
        ## Filter the data to only include rows with non-NA outcome data
        hospitalData <- hospitalData[complete.cases(hospitalData$outcome),]

        ## For each state, find the hospital of the given rank
        sHospitalData <- split(hospitalData, hospitalData$state)
        result <- do.call(rbind, lapply(sHospitalData, function(x) getStateRank(x, num)))

        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        result
}

getStateRank <- function(stateHospitalData, rank){
        ## Get the rank index        
        rankIndex <- getRankIndex(rank)
        
        ## Sort the data based on the rank
        if (rankIndex <= nrow(stateHospitalData)){
                ## Sort the stateData by the motality rate and the hospital name
                if (!is.numeric(rank) && rank == "worst"){
                        stateHospitalData <- stateHospitalData[order(-stateHospitalData$outcome, stateHospitalData$hospital),]
                }
                else{
                        stateHospitalData <- stateHospitalData[order(stateHospitalData$outcome, stateHospitalData$hospital),]
                }
                
                ## Return the name of the hospital in the first row
                result <- stateHospitalData[rankIndex,c("hospital", "state")]
        }
        else{
                result <- stateHospitalData[1,c("hospital", "state")]
                result$hospital <- "<NA>"
        }
        
        result
}
