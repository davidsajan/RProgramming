library(plyr)
rankall <- function(outcome, num="best") {
        ## Read outcome data
        curdata <- read.csv("outcome-of-care-measures.csv",colClasses = "character",na.strings="Not Available")
        
        OutcomeDeath <- curdata[,c(1:10,grep('Death',names(curdata)))]
        
        # Subset by only hospitals.30 at the beginning of the string
        OutcomeDeathHospital <- OutcomeDeath[,c(1:10,grep("^Hospital.30",names(OutcomeDeath)))]
        
        # Subset again based on user input of type of death -- outcome
        # Repalce the . with space
        names(OutcomeDeathHospital) <- (lapply(names(OutcomeDeathHospital), function(x) {gsub("\\.", " ", x)}))
        # Has only 2 columns, Hospital name and outcome to check for rate
        FinalOutcome <- subset(OutcomeDeathHospital[,c(2,7,grep(outcome, tolower(names(OutcomeDeathHospital))))])
        FinalOutcome[,3]<-as.numeric(FinalOutcome[,3])
        
        # Arrage the data using the plyr function first based on Rate and then Hospital Name
        names(FinalOutcome) <- c("HospitalName", "State","Rate")
        NewOrder <- arrange(FinalOutcome,Rate,State)
        
        # An internal function to get the state rank for a particular case of state
        GetStateRank <- function(state){
                tmpdf <- NewOrder[NewOrder[, 2] == state, ]
                allhospital <- nrow(tmpdf)
                switch(num, best = {
                        num = 1
                }, worst = {
                        num = allhospital
                })
                if (num > allhospital) {
                        result = NA
                }
                res = order(tmpdf[, 3], tmpdf[, 1])
                result = tmpdf[res, ][num, 1]
                c(result, state)
        }
        # Recursively get the data by calling sthe GetStateRank function.
        FinalData = do.call(rbind, lapply(unique(curdata$State), GetStateRank))
        FinalData = FinalData[order(FinalData[, 2]), ]
        rownames(FinalData) = FinalData[, 2]
        colnames(FinalData) = c("hospital", "state")
        data.frame(FinalData)

}