best <- function(state, outcome) {
        ## Read outcome data
        curdata <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        
        ## Check that state and outcome are valid 
        if (!state %in% unique(curdata[, 7])) {
                stop("State not found !")
        }
        
        # Return the hospital with the lowest death rate in 30 days
        # subset based on the state from the user
        OutcomeDeath <- subset(curdata[,c(1:10,grep('Death',names(curdata)))],curdata$State==state)
        
        # Subset by only hospitals .20 at the beginning of the string
        OutcomeDeathHospital <- OutcomeDeath[,c(1:10,grep("^Hospital.30",names(OutcomeDeath)))]
        
        # Subset again based on user input of type of death -- outcome
        # Repalce the . with space
        names(OutcomeDeathHospital) <- (lapply(names(OutcomeDeathHospital), function(x) {gsub("\\.", " ", x)}))
        # Has only 2 columns, Hospital name and outcome to check for rate
        FinalOutcome <- subset(OutcomeDeathHospital[,c(2,grep(outcome, tolower(names(OutcomeDeathHospital))))])
        FinalOutcome[,2]<-as.numeric(FinalOutcome[,2])
        FinalIdx <- which.min(as.double(FinalOutcome[,2]))
        
        #FinalOutcome[FinalIdx, "Hospital Name"]
        
        ## Return hospital name in that state with lowest 30-day death rate
        toString(FinalOutcome[FinalIdx, "Hospital Name"])
        
}