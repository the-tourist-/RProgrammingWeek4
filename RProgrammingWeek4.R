best <- function(state, outcome) { 
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomeData[, 11] <- as.numeric(outcomeData[, 11])

  ## Check that state and outcome are valid
  states <- unique(outcomeData$State)
  templateString <- "Hospital.30.Day.Death..Mortality..Rates.from."
  outcomes <- sub(templateString, "", names(outcomeData)[grep(templateString, names(outcomeData), fixed=T)], fixed=T)
  outcomesFriendly <- tolower(sub(".", " ", outcomes, fixed=T))

  state <- toupper(state)
  
  if (sum(outcomesFriendly==tolower(outcome))==0) {
    stop("Invalid Outcome")
  } else {
    outcomeColumn <- paste(templateString, outcomes[which(outcomesFriendly==tolower(outcome))], sep="")
  }
  
  if (sum(states==state)==0)
    stop("Invalid State")
  
  ## Return hospital name in that state with lowest 30-day death ## rate
  filteredOutcomes <- outcomeData[outcomeData$State==state, outcomeColumn]
  sort(outcomeData$Hospital.Name[which(outcomeData[outcomeColumn]==filteredOutcomes[order(as.numeric(filteredOutcomes))][1])])[1]
}

## Used for testing, will print the passed arguments and the resultant output from the "best" function
printAndTestBest <- function(state, outcome) {
  print(state)
  print(outcome)
  print(best(state, outcome))
}

## Test
printAndTestBest("TX", "heart attack")
printAndTestBest("TX", "heart failure")
printAndTestBest("MD", "heart attack")
printAndTestBest("MD", "pneumonia")
printAndTestBest("BB", "heart attack")
printAndTestBest("NY", "hert attack")