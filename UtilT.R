#' @title UtilT
#' 
#' @description Preforms T test on array or validation data
#' 
#' @param BCdesign string filename of the challenge design
#' @param GroupName string of group data 
#' @param RevealTruth logical arguement used to toggle showing true clusters
#' @param ReturnDF logical argument to toggle
#' @export
#
# UtilityFunctions.R
#
#


# perform t-test on array or validation data
UtilT=function(BCDesign=NULL,GroupName,topK=10,DataType="Array",RevealTruth=F,ReturnDF=F){
  # GroupName="Group1"; DataType="Array"; topK=10; RevealTruth=T
  if(is.null(BCdesign)){load(paste(getwd(),'/InstData.RData',sep=''))}
  
  GroupNum=which(BCdesign$Group$GroupNames==GroupName)
  if(length(GroupNum)==0) stop("GroupName not recognized")
  
  if(DataType=="Array"){
    # load Array Data
    load(file=BCdesign$InstDataFile) # provides: MasterArray,MasterValid
    X=MasterArray[[GroupNum]]
    m=dim(X)[1]; n=dim(X)[2]
    
    Adx= grep("A",colnames(X))
    Bdx= grep("B",colnames(X))
      
    myT=function(i) t.test(X[i,Adx],X[i,Bdx])$p.value
    
    p.vals=mapply(myT,1:m)
    p.adj=p.adjust(p.vals,method="bonferroni")
    q.adj=p.adjust(p.vals,method="fdr")
    
    odx=order(p.vals)
    p.df=data.frame(feature=rownames(X)[odx],
                    p.value=p.vals[odx],
                    p.bonferroni=p.adj[odx],
                    q.value=q.adj[odx])
    
    if(RevealTruth){
      p.df$TrueClust=BCdesign$ExprVals$TrueClust[odx]
      p.df$TrueDelta=BCdesign$ExprVals$TrueDelta[odx]
    }
  
    cat("============= Quick Look at Array Data =============",fill=T)
    cat("Group:",GroupName,fill=T)
    cat("",fill=T)
    print(p.df[1:topK,])
    
  } # end if(DataType=="Array")
  
  
  
  if(DataType=="Valid"){
    # load Array Data
    load(file=BCdesign$InstDataFile) # provides: MasterArray,MasterValid
    
    # identify non-empty validation results
    if(is.null(unlist(MasterValid[[GroupNum]]))) stop("No Validation Results Were Found")
    
    vL=lapply(MasterValid[[GroupNum]],length)
    vDX=which(vL!=0)
    
    myT=function(i){
      X=MasterValid[[GroupNum]][i][[1]]
      Adx= grep("A",names(X))
      Bdx= grep("B",names(X))
      return(t.test(X[Adx],X[Bdx])$p.value)
    } 
  
    p.vals=mapply(myT,vDX)
    p.adj=p.adjust(p.vals,method="bonferroni")
    q.adj=p.adjust(p.vals,method="fdr")
    
    odx=order(p.vals)
    p.df=data.frame(feature=names(p.vals)[odx],
                    p.value=p.vals[odx],
                    p.bonferroni=p.adj[odx],
                    q.value=q.adj[odx])
    
    if(RevealTruth){
      p.df$TrueClust=BCdesign$ExprVals$TrueClust[vDX[odx]]
      p.df$TrueDelta=BCdesign$ExprVals$TrueDelta[vDX[odx]]
    }
    
    if(length(p.vals)<topK) topK=length(p.vals)
    
    cat("============= Quick Look at Validation Data =============",fill=T)
    cat("Group:",GroupName,fill=T)
    cat("",fill=T)
    print(p.df[1:topK,])
    
  } # end if(DataType=="Valid")
  
  if(ReturnDF) return(p.df)
}
