rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    rankTable<-read.csv("outcome-of-care-measures.csv")
    
    ## Check that state and outcome are valid
    uniqueStates<-as.character(rankTable[,7])
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
    
    # cut rankTable by state
    rankTable<-subset(rankTable, State == state)
    
    # cut out invalid rows on selected outcome
    # handle NA's?
    stringRemove = "Not Available"
    rankTable<-subset(rankTable, rankTable[,colIndex] != stringRemove)
    
    maxRow<-nrow(rankTable)
    
    # out invalid rows
    
    # count hospitals to determine "worst" number
    
    # Translate strings and change to numeric on rank
    numTranslate = numeric()
    numTranslate<-if(num == "best") {
        1
        #as.numeric(numTranslate)
    }
    else if (num == "worst") {
        as.numeric(maxRow)
    }
    else as.numeric(num)

    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    ## break ties
}