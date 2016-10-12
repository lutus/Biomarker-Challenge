#' Randomizes a budget 
#' 
#' Provides a randomized budget between 25,000 and 100,000 depending on the number of groups specified
#' 
#' @param numGroups interger number of groups participating
#'
#' 
randBudget <- function(numGroup){
  numGroup=as.integer(numGroup)
  
  #----------------budget randomizer------------------
  budgetMin=25000
  budgetMax=100000
  budgetValues=round(75000/(numGroup-1), digits = -3)
  budgetArray=rep(0, numGroup)
  budgetArray[1]=25000
  budgetArray[numGroup]=100000
  for(i in 2:numGroup-1){budgetArray[i]=25000+(i-1)*budgetValues} # populates budgets for groups
  budgetArray=sample(budgetArray, numGroup)# randomizes budget values
  return (budgetArray)
  #-------------------end budget randomizer---------------------
}