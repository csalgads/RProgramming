## Unzipped the data.
unzip(zipfile = "PA1.zip")
## 1 Plot the 30-day mortality rates for heart attack
     outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     head(outcome)
     ncol(outcome)
     names(outcome)
     outcome[, 11] <- as.numeric(outcome[, 11])
     ## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])
## 2 Finding the best hospital in a state
best <- function(state, outcome = "heart attack") {
     ## Read outcome data
     outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     ## Check that state and outcome are valid
     
     LStates <- unique(outcomedata[,7])
     Loutcome <- c("heart attack","heart failure","pneumonia")
     if(!state %in% LStates) { stop("invalid state") } 
     else if (!outcome %in% Loutcome){ stop("invalid outcome")}
     ## heart attack
     if (outcome == "heart attack") { ncolu = 11 } 
     ## heart failure
     else if (outcome == "heart failure") {ncolu = 17} 
     ## pneumonia
     else if (outcome == "pneumonia") { ncolu = 23}
     oldw <- getOption("warn")
     options(warn = -1)
     outcomedata[, ncolu] <- as.numeric(outcomedata[, ncolu])
     ## Return hospital name in that state with lowest 30-day death
     
     analdata <- outcomedata[,c(2,7,ncolu)]
     analdata1 <- analdata [!is.na(analdata[,3]),]
     analdata2 <- analdata1[analdata1[,2]==state,]
     Hl30d <- sort(analdata2 [analdata2[,3]== min(analdata2[,3]),1])
     options(warn = oldw)
     ## rate
     return(Hl30d[1])
}
## 3 Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = "best") {
     ## Read outcome data
     outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     ## Check that state and outcome are valid
     LStates <- unique(outcomedata[,7])
     Loutcome <- c("heart attack","heart failure","pneumonia")
     if(!state %in% LStates) { stop("invalid state") } 
     else if (!outcome %in% Loutcome){ stop("invalid outcome")}
     ## heart attack
     if (outcome == "heart attack") { ncolu = 11 } 
     ## heart failure
     else if (outcome == "heart failure") {ncolu = 17} 
     ## pneumonia
     else if (outcome == "pneumonia") { ncolu = 23}
     
     
     
     oldw <- getOption("warn")
     options(warn = -1)
     outcomedata[, ncolu] <- as.numeric(outcomedata[, ncolu])
     ## Return hospital name in that state with the given rank
     ## 30-day death rate
     analdata <- outcomedata[,c(2,7,ncolu)]
     analdata1 <- analdata [!is.na(analdata[,3]),]
     analdata2 <- analdata1[analdata1[,2]==state,]
     Hl30d <- analdata2[order(analdata2[,3],analdata2[,1]),] 
     
     Lnum <- c("best","worst")
     LHosp <- unique(Hl30d[,1])
     if(num == Lnum[1]){ num <- 1 }
     else if (num == Lnum[2]) {num <- length(LHosp)}
     else if (num > length(LHosp)) {return(NA)}
     
     options(warn = oldw)
     
     return(Hl30d[num,1])
     options(warn = oldw)
}
## 4 Ranking hospitals in all states
rankall <- function(outcome, num = "best") {
     ## Read outcome data
     outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     ## Check that state and outcome are valid
     Loutcome <- c("heart attack","heart failure","pneumonia")
     if (!outcome %in% Loutcome){ stop("invalid outcome")}
     ## heart attack
     if (outcome == "heart attack") { ncolu = 11 } 
     ## heart failure
     else if (outcome == "heart failure") {ncolu = 17} 
     ## pneumonia
     else if (outcome == "pneumonia") { ncolu = 23}
     
     ## For each state, find the hospital of the given rank
     oldw <- getOption("warn")
     options(warn = -1)
     outcomedata[, ncolu] <- as.numeric(outcomedata[, ncolu])
     LStates <- unique(outcomedata[,7])
     RH30d <- data.frame()
     Lnum <- c("best","worst")
     for(i in 1:length(LStates)){
          
          analdata <- outcomedata[,c(2,7,ncolu)]
          analdata1 <- analdata [!is.na(analdata[,3]),]
          analdata2 <- analdata1[analdata1[,2]==LStates[i],]
          if(num == Lnum[1]){ 
               nume <- 1 
               Hl30d <- analdata2[order(analdata2[,3],analdata2[,1]),]     
               RH30d <- rbind(RH30d,Hl30d[nume,c(1,2)])
          }
          
          else if (num == Lnum[2]) {
               nume <- length(analdata2[,1])
               Hl30d <- analdata2[order(analdata2[,3],analdata2[,1]),]
               RH30d <- rbind(RH30d,Hl30d[nume,c(1,2)])
          }
          else if (num > length(analdata2[,1])) {
               RH30d <- rbind(RH30d,c(NA,LStates[i]))
          }
          else {
               Hl30d <- analdata2[order(analdata2[,3],analdata2[,1]),]     
               RH30d <- rbind(RH30d,Hl30d[num,c(1,2)])
          }
          
          
     }
     
     
     ## Return a data frame with the hospital names and the
     ## (abbreviated) state namen
     
     
     return(RH30d[order(RH30d[,2]),])
     options(warn = oldw)
     
}