library(plyr)
rankhospital <- function(state, outcome, num="best") {
        ## Read outcome data
        curdata <- read.csv("outcome-of-care-measures.csv",colClasses = "character",na.strings="Not Available")
        
        ## Check that state and outcome are valid 
        if (!state %in% unique(curdata[, 7])) {
                stop("State not found !")
        }
        
        # Return the hospital with the lowest death rate in 30 days
        # subset based on the state from the user
        OutcomeDeath <- subset(curdata[,c(1:10,grep('Death',names(curdata)))],curdata$State==state)
        
        # Subset by only hospitals.30 at the beginning of the string
        OutcomeDeathHospital <- OutcomeDeath[,c(1:10,grep("^Hospital.30",names(OutcomeDeath)))]
        
        # Subset again based on user input of type of death -- outcome
        # Repalce the . with space
        names(OutcomeDeathHospital) <- (lapply(names(OutcomeDeathHospital), function(x) {gsub("\\.", " ", x)}))
        # Has only 2 columns, Hospital name and outcome to check for rate
        FinalOutcome <- subset(OutcomeDeathHospital[,c(2,grep(outcome, tolower(names(OutcomeDeathHospital))))])
        FinalOutcome[,2]<-as.numeric(FinalOutcome[,2])
        
        # Arrage the data using the plyr function first based on Rate and then Hospital Name
        names(FinalOutcome) <- c("HospitalName", "Rate")
        NewOrder <- arrange(FinalOutcome,Rate, HospitalName)
        
        # Check if num is numeric or not and process accordingly
        if (is.numeric(num)){
                PrintData <- NewOrder[1:num,]
                PrintData$Rank = 1:dim(PrintData)[1]
                
        } else {
                
                PrintData <- NewOrder
                PrintData$Rank = 1:dim(PrintData)[1]
        }
        # Remove all NA from the data
        PrintData <- na.omit(PrintData)
        
        # if num is best or worst else print NA
        if (!is.numeric(num)){
                
                if (tolower(num) == "best"){
                        PrintData[1,1]      
                } else if (tolower(num) == "worst") {
                        PrintData[dim(PrintData)[1],1]
                }else {
                        print("NA")
                }
        } else {
                PrintData <- NewOrder[1:num,]
                PrintData$Rank = 1:dim(PrintData)[1]
                PrintData[num,1]  
                
        }
}