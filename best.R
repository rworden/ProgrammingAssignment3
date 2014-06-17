best <- function(state, outcome) {
    #read in table, set up unique states, hard code outcomes
    outcomeTable<-read.csv("outcome-of-care-measures.csv")
    uniqueStates<-as.character(unique(outcomeTable[,7]))
    uniqueOutcomes<-as.character(c("heart attack","heart failure","pneumonia"))
    
    # kill the function and throw an error if inputs are invalid
    if(!(state %in% uniqueStates)) { stop("invalid state")}
    if(!(outcome %in% uniqueOutcomes)) { stop("invalid outcome")}
    
    # map input texts to column indexes in outcomeTable
    ## TODO: check these
    if(outcome == "heart attack") {
        colIndex = 11
       ## colRemoveIndex = 16
    }
    else if(outcome == "heart failure") {
        colIndex = 17
       ## colRemoveIndex = 22
    }
    else if (outcome == "pneumonia") {
        colIndex = 23
        ##colRemoveIndex = 28
    }
    else colIndex = NULL ##& colRemoveIndex = NULL
    
    ## stringRemove is a static string for now...should probably do more than spot checks for invalid values
    stringRemove = "Not Available"
    
    # remove all other states
    outcomeTableState<-subset(outcomeTable, State==state )
    
    #remove any row that does not appear to have data
    outcomeTableTotal<-subset(outcomeTableState, outcomeTableState[,colIndex] != stringRemove)
    
    #return index value of minimum rate (use which.min, which I'm not using because only takes first value
    ## and coerces ties to be resolved)
    minRate<-min(as.numeric(as.character(outcomeTableTotal[,colIndex])))

    # return ALL rows where colIndex == minRate
    tiePopulate<-subset(outcomeTableTotal,as.numeric(as.character(outcomeTableTotal[,colIndex])) == minRate)
    ## Return hospital name in that state with lowest 30-day death rate
    tiePopulate<-as.character(tiePopulate$Hospital.Name)
}
