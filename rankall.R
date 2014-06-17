rankall <- function(outcome, num = "best") {
    ## Read outcome data
    rankAllTable<-read.csv("outcome-of-care-measures.csv")
    
    ## Check that state and outcome are valid
    uniqueStates<-as.character(rankAllTable[,7])
    uniqueOutcomes<-as.character(c("heart attack","heart failure","pneumonia"))
    
    # Kill the function if given invalid inputs
    if(!(state %in% uniqueStates)) {stop("invalid state")}
    if(!(outcome %in% uniqueOutcomes)) {stop("invalid outcome")}
    
    # map outcome to proper columns
    if(outcome == "heart attack") {
        colIndex = 11
    }
    else if(outcome == "heart failure") {
        colIndex = 17
    }
    else if (outcome == "pneumonia") {
        colIndex = 23
    }
    else colIndex = NULL
    
    ## stringRemove is a static string for now...should probably do more than spot checks for invalid values
    stringRemove = "Not Available"
    #remove any row that does not appear to have data
    rankAllTable<-subset(rankAllTable, rankAllTable[,colIndex] != stringRemove)

## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the ## (abbreviated) state name
}