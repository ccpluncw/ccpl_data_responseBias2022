require(dplyr)
library(RRW)
library(smartGridSearch)
library(chutils)

ExpName <- "respBias3"
test <- FALSE
#use multicore
useMultiCore <- TRUE
#fit free parameters
runSmartGridSearch <- TRUE
#Only run new models that have not been run before?
skipModelsWithExistingDirs <- FALSE

multicorePackages = c('RRW')

mainDir <- getwd()

############## smartGridSearch and simulation parameters
#"BIC" "AIC" "R_Square"
minimizeStat <- "BIC"
#do you want to equalize the influence of RT and pHit on the minimization statistic (TRUE) or have the influence be a function of the number of observations (FALSE)
equalizeRTandPhit <- TRUE
#the minimum number of trials per overlapRound to include in the analysis.
minN <- 30
#number of intervals to split each bounded parameter into in order to get the random values between the hi and low values
numGridIntervals <- 100
#the number of times to loop with grid search before looking for the top 10 best fits.
numGridLoops <- ifelse (test, 250, 4000)
#the number of loops for each RW step.
loopsPerRWstep <- ifelse (test, 50, 500)
#paramtable confirmation loops
optBoundLoops <- ifelse (test, 3, 20)
#The number of best fit runs from which to resize the parameter bounds.
#Here, I set it to the sqrt(total number of runs per grid loop) or 10 at minimum.
  #optParamListN <- ifelse ( trunc(sqrt(numGridLoops)) < 10, 10, trunc(sqrt(numGridLoops)) )
optParamListN <- 10
#the number of simulations to average using the final parameters.
numSimsToAverage <- ifelse (test, 10, 40)
#the number of k-fold validation runs.
kFoldNum <- 3
appendRunStats <- FALSE
######################################################

mainDir <- getwd()
inputDataFile <- "../analysisReadyData.sn.txt"
analysisReadyData<-read.table(inputDataFile, header=T, sep="\t")
tmp.df<-analysisReadyData

### direct.xvy = -1 means the lower valued object will be killed by default
### direct.xvy = 1 means the higher valued object will be killed by default
tmp.df$defaultKilled <- ifelse(tmp.df$direct.xVy < 0 , "defaultLVO","defaultHVO")

source("modelspecs_RB_kfold.r")
modelNames <- names(allModels)

fileTagShortName <- paste(ExpName, sep="_")
fileTagOutStats <- paste(format(Sys.time(), "%b_%d_%Y_%H-%M"), fileTagShortName, sep="_")

### this loop runs the smartGridSearch to optimize free parameters
if(runSmartGridSearch) {
  for (i in modelNames) {
    subDirExists <- ch.newDir (mainDir, i)
    #if the sub directory does NOT exist or you do NOT want to skip models, then run the analysis
    if(subDirExists == FALSE | skipModelsWithExistingDirs == FALSE) {
      modDir <- getwd()
      setwd(modDir)

      tmpModelList <- allModels[[i]]

      #create a file tag that will be used to save files.  it will be time and date stamped
      fileTagShortName <- paste(ExpName, i, sep="_")
      fileTag <- paste(format(Sys.time(), "%b_%d_%Y_%H-%M"), fileTagShortName, sep="_")

      runStats.list <- kfoldRRWRunSmartGridSearch(kFold = kFoldNum, tmp.df, tmpModelList, minN = minN, dataOverlapCol = "overlapRound", RwSamplesCol = "Q50", dataRtCol = "res.RT", correctCol = "correct01", correctVals = c(1,0), loopsPerRWstep = loopsPerRWstep, minimizeStat = minimizeStat, equalizeRTandPhit = equalizeRTandPhit, numLoops = numGridLoops, numIntervals = numGridIntervals, optParamListN = optParamListN, optBoundLoops = optBoundLoops, multicore = useMultiCore, multicorePackages = multicorePackages, numSimsToAverage = numSimsToAverage, fileTag = fileTag)

    }
    setwd(mainDir)
  }
}
