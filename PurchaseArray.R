#' @title PurchaseArray
#
#' @description  Function to purchase Array Data
#' 
#' @param BCdesign string of location the location, if left out, it will search current working directory
#' @param GroupName string of the group wanting to purchase array
#' @param  SampleAdx array of sample numbers to take from population A
#' @param  SampleBdx array of sample numbers to take from population B
#' @param  ArrayCost sets the cost of each array. defaults to 1000
#' 
#' @export



PurchaseArray=function(BCdesign=NULL, # a class BCdesign objects
                        GroupName, # the name of the group making the purchase
                        SampleAdx, #  the index of samples from popoulaton "A"
                        SampleBdx, # the index of the samples from population "B"
                        ArrayCost=1000 # the cost of each array
                                                ){
  # GroupName="Group1"; SampleAdx=1:50; SampleBdx=1:50; ArrayCost=1000
  if(is.null(BCdesign)){load(paste(getwd(),'/InstData.RData',sep=''))}
  
  options(scipen=999)
  
  cat("################### Purchase Array Data ###################", fill=T)
  # identify group #
  GroupNum=which(BCdesign$Group$GroupNames==GroupName)
  if(length(GroupNum)==0) stop("GroupName not recognized")
  
  # see if they have the $ for it
  ArrayFee=(length(SampleAdx)+length(SampleBdx))*ArrayCost
  if(ArrayFee>BCdesign$Group$balance[GroupNum]) stop("Not enough $ to pay for arrays... not running a charity!")
  cat("Array Purchase Order for:",GroupName,fill=T)
  cat("Total number of requested Arrays:",length(SampleAdx)+length(SampleBdx),fill=T)
  cat("at a cost of $",ArrayCost," per array",sep="",fill=T)
  cat("Total Array Fee= $",ArrayFee,sep="",fill=T)
  cat("Group Balance Before= $",BCdesign$Group$balance[GroupNum],sep="",fill=T)
  BCdesign$Group$balance[GroupNum]=BCdesign$Group$balance[GroupNum]-ArrayFee
  cat("Group Balance After= $",BCdesign$Group$balance[GroupNum],sep="",fill=T)
  cat(fill=T)
  
  # get sample indices wrt ExprMat
  MatchAdx=match(paste("A",SampleAdx,sep=""),colnames(BCdesign$ExprVals$ExprMat))
  if(sum(is.na(MatchAdx))>0) stop("not all SampleAdx values match")
  MatchBdx=match(paste("B",SampleBdx,sep=""),colnames(BCdesign$ExprVals$ExprMat))
  if(sum(is.na(MatchBdx))>0) stop("not all SampleBdx values match")
  
  ArrayData=BCdesign$ExprVals$ExprMat[,c(MatchAdx,MatchBdx)]
  # add array "noise"
  ArrayData=ArrayData+array(rnorm(dim(ArrayData)[1]*dim(ArrayData)[2],sd=BCdesign$ArrayDesign$sd.array),
                  dim=dim(ArrayData))
  
  
  # update Instructor copy
  load(file=BCdesign$InstDataFile) # provides: MasterArray,MasterValid
   
  if(length(MasterArray)<GroupNum){
    MasterArray[[GroupNum]]=ArrayData
  }
  else{
    if(length(MasterArray[[GroupNum]])==0){
      MasterArray[[GroupNum]]=ArrayData
    }
    else{
      MasterArray[[GroupNum]]=cbind(MasterArray[[GroupNum]],ArrayData)
    }
  }
  
  # save the updated instructor copy
   save(file=BCdesign$InstDataFile,MasterArray,MasterValid)
  
   # now, update the group array data object
   ArrayData=MasterArray[[GroupNum]]
   save(file=BCdesign$Group$GroupArrayFile[GroupNum],ArrayData )
   
#return(BCdesign)  
}