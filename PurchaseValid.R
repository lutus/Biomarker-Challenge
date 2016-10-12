#' @title PurchaseValid
#'
#' @description Function to purchase Valid Data
#' 
#' @param BCdesign string of location the location, if left out, it will search current working directory
#' @param GroupName string of the group wanting to purchase array
#' @param  SampleAdx array of sample numbers to take from population A
#' @param  SampleBdx array of sample numbers to take from population B
#' @param  AssayCost sets the cost of each validation assay
#' 
#' @export

# PurhaseValid.R
#
# purchase validation data..
#
PurchaseValid =function(BCdesign=NULL, # a class BCdesign objects
                        GroupName, # the name of the group making the purchase
                        FeatName, # the name of the feature to be validated
                        SampleAdx, #  the index of samples from popoulaton "A"
                        SampleBdx, # the index of the samples from population "B"
                        AssayCost=10 # the cost of each validation assay
                            ){
  # GroupName="Group1"; FeatName="feat117"; SampleAdx=1:50; SampleBdx=1:50; AssayCost=10
  if(is.null(BCdesign)){load(paste(getwd(),'/InstData.RData',sep=''))}
  options(scipen=999)
  
  cat("################### Purchase Validaton Assay Data ###################", fill=T)
  # identify group 
  GroupNum=which(BCdesign$Group$GroupNames==GroupName)
  if(length(GroupNum)==0) stop("GroupName not recognized")
  # identify feature
  FeatNum=which(rownames(BCdesign$ExprVals$ExprMat)==FeatName)
  if(length(FeatNum)==0) stop("FeatName not recognized")
  
  # see if they have the $ for it
  AssayFee=(length(SampleAdx)+length(SampleBdx))*AssayCost
  if(AssayFee>BCdesign$Group$balance[GroupNum]) stop("Not enough $ to pay for arrays... not running a charity!")
  cat("Validation Assay Purchase Order for:",GroupName,fill=T)
  cat("Feature ID:",FeatName,fill=T)
  cat("Total number of requested Assaays:",length(SampleAdx)+length(SampleBdx),fill=T)
  cat("at a cost of $",AssayCost," per assay",sep="",fill=T)
  cat("Total Assay Fee= $",AssayFee,sep="",fill=T)
  cat("Group Balance Before= $",BCdesign$Group$balance[GroupNum],sep="",fill=T)
  BCdesign$Group$balance[GroupNum]=BCdesign$Group$balance[GroupNum]-AssayFee
  cat("Group Balance After= $",BCdesign$Group$balance[GroupNum],sep="",fill=T)
  cat(fill=T)
  
  # get sample indices wrt ExprMat
  MatchAdx=match(paste("A",SampleAdx,sep=""),colnames(BCdesign$ExprVals$ExprMat))
  if(sum(is.na(MatchAdx))>0) stop("not all SampleAdx values match")
  MatchBdx=match(paste("B",SampleBdx,sep=""),colnames(BCdesign$ExprVals$ExprMat))
  if(sum(is.na(MatchBdx))>0) stop("not all SampleBdx values match")
  
  AssayData=BCdesign$ExprVals$ExprMat[FeatNum,c(MatchAdx,MatchBdx)]
  # add array "noise"
  AssayData=AssayData+rnorm(length(AssayData),sd=BCdesign$ArrayDesign$sd.valid)
  
  # update Instructor copy
  load(file=BCdesign$InstDataFile) # provides: MasterArray,MasterValid
  MasterValid[[GroupNum]][[FeatNum]]=c(MasterValid[[GroupNum]][[FeatNum]],AssayData)
  
  # save the updated instructor copy
  save(file=BCdesign$InstDataFile,MasterArray,MasterValid)
  
  # now, update the group array data object
  ValidData=MasterValid[[GroupNum]]
  save(file=BCdesign$Group$GroupValidFile[GroupNum],ValidData )
  
  return(BCdesign)  
}
