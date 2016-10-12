#' @title UtilPCA
#' 
#' @description Performs Principal Components analysis on array or validation data
#' 
#' @param BCdesign string file name of challenge
#' @param GroupName string group name data to be analyzed
#' @param topK number of results to return
#' 
#' @export
# PCA
#

# perform PCA on array or validation data
UtilPCA=function(BCDesign=NULL,GroupName,topK=0,DataType="Array"){
  # GroupName="Group1"; DataType="Array"; topK=10; 
  if(is.null(BCdesign)){load(paste(getwd(),'/InstData.RData',sep=''))}
  GroupNum=which(BCdesign$Group$GroupNames==GroupName)
  if(length(GroupNum)==0) stop("GroupName not recognized")
  
  if(DataType=="Array"){
    # load Array Data
    load(file=BCdesign$InstDataFile) # provides: MasterArray,MasterValid
    X=MasterArray[[GroupNum]]
    m=dim(X)[1]; n=dim(X)[2]
    
    if(topK==0){topK=m}
    else{
      sdX=apply(X,1,sd)
      Kdx=order(-sdX)[1:topK]
      X=X[Kdx,]
    }
    
    Adx= grep("A",colnames(X))
    Bdx= grep("B",colnames(X))
    
    ttl=c(paste("Group ID:",GroupName),"Pop A (black) ; Pop B (red)",
          paste(topK,"assay values"))
    
    PCfit=prcomp(t(X),scale=F)
    
    pcx1=t(X)%*%PCfit$rotation[,1]
    pcx2=t(X)%*%PCfit$rotation[,2]
    pcx3=t(X)%*%PCfit$rotation[,3]
    
    clr=rep(1,n)
    clr[Bdx]=2
    
    s3d=scatterplot3d(pcx1,pcx3,pcx2,color=clr,pch=20,cex.symbols=1.1,
                      xlab="PC1",ylab="PC3",zlab="PC2",
                      main=ttl)
    
    
    
  } # end if(DataType=="Array")
  
  
  
  if(DataType=="Valid"){
    
    cat("Validation Data PCA not currently supported")
    
  } # end if(DataType=="Valid")
  
}



