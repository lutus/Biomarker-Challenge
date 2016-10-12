#' @title InitChallenge
#' 
#' @description Sets global directories for the challenge
#' 
#' @param budget  array: nGroup length vector of budget values for each group
#' @param GroupNames string: group names, if NULL, defaults to Group1, Group2, etc..
#' @param GroupDir string: master directory under which the group directories are located
#' @param GroupSubDirs, string: if NULL, then directories will be created called "Group1","Group2", etc if not NULL, then must be an nGroup list of subdirectories of GroupDir that are assumed to exist
#' @param InstDir string: this will be the master directory for instructor copies of the files
#' @param DesignFile string: name of design file
#' 
#' @export


# setupDir<-function(rtDir=getwd(), numGroup, instDir='InstrDir'){
#   if(dir.exists(paste(rtDir,instDir)))
# }

#
# InitChallenge.R
# 
# This is a much less user friendly version of Luther's set-up code
#
# Assume this is executed in the root directory..
# InitChallenge.R
# 
# This is a much less user friendly version of Luther's set-up code
#
# Assume this is executed in the root directory..

InitChallenge =function(numGroup=NULL, #number of participants
                        budget=NULL, # this will be an nGroup length vector of budget values for each group
                        GroupNames=NULL, # group names, if NULL, defaults to Group1, Group2, etc..
                        GroupDir=NULL, # this will be the master directory under which the group directories 
                                  # are located
                        GroupSubDirs=NULL, # if NULL, then directories will be created called "Group1","Group2", etc
                                            # if not NULL, then must be an nGroup list of subdirectories of GroupDir
                                            #              that are assumed to exist
                        InstDir=NULL, # this will be the master directory for instructor copies of the files
                        DesignFile=paste(system.file(package = 'biomarker'),"extdata/design1.csv", sep = '/')
                         ){
  # budget=c(25000,50000,75000,100000); GroupNames=NULL, GroupDir="Groups"; GroupSubDirs=NULL; InstDir="Inst"; DesignFile="ArrayDesign.csv"
  
  #--------------Welcome-----------------
  print("Welcome to the Biomarker Challenge.")
  print('############### Creating Design Object and Initializing Dirs ###############')
  if(is.null(numGroup)){numGroup=4}
  if(is.null(budget)){budget=randBudget(numGroup)}
  #---------  BEGIN INSERT readline ineteractive code here later --------#
  # 
  #   basically, insert (with some modifications for different variable names, etc) code from setupv2.R
  #   for now, assume that appropriate arguments have been passed into the function
  # 
  
  
  #--------- END INSERT readline ineteractive code here --------#
  
 
  #--------- Load the Array Design Information ---------#
  # the array design file has a very specific format
  mcnAnB=read.table(DesignFile,skip=3,sep=",",nrows=1)
  sdvals=read.table(DesignFile,skip=5,sep=",",nrows=1)
  ArrayPars=read.table(DesignFile,skip=7,sep=",",nrows=mcnAnB[1,1])
  colnames(ArrayPars)=c("mclust","muA","muB","rho")
  
  #--------- INITIALIZE THE INSTRUCTOR OBJECT ---------#
  #
  # Instructor Object
  #  
  
  BCdesign <- list(
    Group=list(
      numGroup=length(budget), # number of groups
      budget=budget, # group budgets
      balance=budget, # current balance for each group
      purchaseHistory=c() # purchase history
    ),
    InstDir=if(is.null(InstDir)){InstDir=getwd()}, # the instructor directory
    InstDataFile=paste(InstDir,"/InstData.RData",sep=""), # RData file for storing instructor's copy of the data 
    ArrayDesign=list(  # various Array Design values for the simulation
      mclust=mcnAnB[1,1],
      mfeat=sum(ArrayPars[,1]),
      nA=mcnAnB[1,2],
      nB=mcnAnB[1,3],
      sd.array=sdvals[1,1],
      sd.valid=sdvals[1,2],
      ArrayPars=ArrayPars
    )
  )
  
  # now, generate the "true" expression data
#  ExprMat=array(NA,dim=c(BCdesign$ArrayDesign$nfeat,BCdesign$ArrayDesign$nA+BCdesign$ArrayDesign$nA))
  
  for(i in 1:BCdesign$ArrayDesign$mclust){
    Sigma=array(rep(BCdesign$ArrayDesign$ArrayPars[i,4],BCdesign$ArrayDesign$ArrayPars[i,1]^2),
                dim=c(BCdesign$ArrayDesign$ArrayPars[i,1],BCdesign$ArrayDesign$ArrayPars[i,1]))
    diag(Sigma)=1
    XA=MASS::mvrnorm(BCdesign$ArrayDesign$nA,
               mu=rep(BCdesign$ArrayDesign$ArrayPars[i,2],BCdesign$ArrayDesign$ArrayPars[i,1]),Sigma=Sigma)
    XB=MASS::mvrnorm(BCdesign$ArrayDesign$nB,
               mu=rep(BCdesign$ArrayDesign$ArrayPars[i,3],BCdesign$ArrayDesign$ArrayPars[i,1]),Sigma=Sigma)
    
    if(i==1){
      ExprMat=t(rbind(XA,XB))
      TrueClust=rep(i,BCdesign$ArrayDesign$ArrayPars[i,1])
      TrueDelta=rep(BCdesign$ArrayDesign$ArrayPars[i,3]-BCdesign$ArrayDesign$ArrayPars[i,2],
                    BCdesign$ArrayDesign$ArrayPars[i,1])
    }
    else{
      ExprMat=rbind(ExprMat,t(rbind(XA,XB)))
      TrueClust=c(TrueClust,rep(i,BCdesign$ArrayDesign$ArrayPars[i,1]))
      TrueDelta=c(TrueDelta,rep(BCdesign$ArrayDesign$ArrayPars[i,3]-BCdesign$ArrayDesign$ArrayPars[i,2],
                                BCdesign$ArrayDesign$ArrayPars[i,1]))
    }
  }
  
  # now, permute the features (because we don't want the clusters in perfect order)
  PermDX=sample(1:BCdesign$ArrayDesign$mfeat)
  
  # put column names in place
  colnames(ExprMat)=c(paste("A",1:BCdesign$ArrayDesign$nA,sep=""),
                      paste("B",1:BCdesign$ArrayDesign$nA,sep=""))
  
  
  BCdesign$ExprVals=list(
    ExprMat=ExprMat[PermDX,],
    TrueClust=TrueClust[PermDX],
    TrueDelta=TrueDelta[PermDX],
    PermDX=PermDX
  )
 
  # put row names in place
  rownames(BCdesign$ExprVals$ExprMat)=paste("feat",1:BCdesign$ArrayDesign$mfeat,sep="")
  
   
  # now, setup the filenames for the array and validation objects
  
  if(is.null(GroupSubDirs)) GroupSubDirs=paste(rep("Group",BCdesign$Group$numGroup),
                                               1:BCdesign$Group$numGroup,sep="")
  
  if(is.null(GroupNames)) BCdesign$Group$GroupNames=paste(rep("Group",BCdesign$Group$numGroup),
                                                          1:BCdesign$Group$numGroup,sep="")
  
  if(is.null((GroupDir))){GroupDir='Groups'}
  GroupPaths=paste(GroupDir,"/",GroupSubDirs,sep="")
  BCdesign$Group$GroupArrayFile=paste(GroupPaths,"/Array.RData",sep="")
  BCdesign$Group$GroupValidFile=paste(GroupPaths,"/Valid.RData",sep="")
   
  #--------- CREATE DIRECTORIES IF NEED BE ------------#
  
  if(!(dir.exists(InstDir))) dir.create(InstDir)
  
  if(!(dir.exists(GroupDir))) dir.create(GroupDir)
  for(i in which(!(dir.exists(GroupPaths)))) dir.create(GroupPaths[i])
  
  
  #--------- SAVE EMPTY DATA OBJECTS ------------#
  
  # we will start the Array list as empty
  MasterArray=list()
  # for the validation list, we make sure that it is a list of lists of length mfeat
  MasterValid=list()
  for(i in 1:BCdesign$Group$numGroup){
    MasterValid[[i]]=vector("list",BCdesign$ArrayDesign$mfeat)
    names(MasterValid[[i]])=rownames(BCdesign$ExprVals$ExprMat)
  }
    
  save(file=BCdesign$InstDataFile,BCdesign,MasterArray,MasterValid)
  
  # return BCdesign object
  #class(BCdesign)="BCdesign"
  #return(BCdesign)
}

  
  

