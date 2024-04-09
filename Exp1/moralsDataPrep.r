library(chMorals)
library(chutils)

#set up the new RT variables
#set up the new RT variables
fitCol <- "fit.RT"
resCol <- "res.RT"
useTwoParameterModel <- FALSE
overlapDataIsComplete <- TRUE
respChoiceVal <- c("Yes", "No")
item1cols <- c("Item1")
item2cols <- c("Item2")
overlapItem1cols <- c("IA1")
overlapItem2cols <- c("IB1")

# read in parameters
params<-ch.readMoralsDBfile("moralsDBfile.txt")

#set up the group and item directories
mainDir <- getwd()
ch.newDir (mainDir, params$gpSubDir)
gpDir <- getwd()
setwd(mainDir)

ch.newDir (mainDir, params$itemSubDir)
itemDir <- getwd()
setwd(mainDir)

statsOutputFile <- file.path(mainDir,paste(params$dt.set, params$statsOutputFilePrefix))
sink(statsOutputFile, append = F)
  cat("\n***** New Run ****\n\n")
sink(NULL)

### read in data
data.raw <-read.table(params$moralsTaskDataFile, header=T, sep="\t")
data.ovrlp <-read.table(params$valueOverlapDataFile, header=T, sep="\t", quote="\"")

######_____REMOVE PRACTICE TRIALS _____######
data.raw <- data.raw[data.raw$trial_type >=1, ]

### do Prep analysis
processedData <- ch.moralsDataPrep(data.raw, data.ovrlp, "sn", "keybRT", "overlap", "direction", "trial", "keyDef", respChoiceVal = respChoiceVal, item1cols = item1cols, item2cols = item2cols, overlapItem1cols = overlapItem1cols, overlapItem2cols = overlapItem2cols, statsOutputFile = statsOutputFile, params = params, overlapDataIsComplete = overlapDataIsComplete)

### Filter data
analysisReadyData <- ch.moralsFilterDataQ(processedData, "sn", "keybRT", "overlapRound", "correct",c(1,0), statsOutputFile = statsOutputFile, params = params)

### Do RT and p(Hit Analysis on Group Data - remove learning effects for the group)
analysisReadyData.gp <- ch.moralsGrpRTpHit(analysisReadyData, "trial", "keybRT", fitCol, resCol, "overlapRound", "keyDef",respChoiceVal, "correct",c(1,0), useTwoParameterModel = useTwoParameterModel, params = params)
write.table(analysisReadyData.gp, file="analysisReadyData.gp.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

### Do RT and p(Hit Analysis on individual subject Data - remove learning effects for each subject)
analysisReadyData.sn <- ch.moralsSnRTpHit(analysisReadyData.gp, "sn", "trial", "keybRT", fitCol, resCol, "overlap", "correct", c(1,0),  useTwoParameterModel = useTwoParameterModel, params = params)
write.table(analysisReadyData.sn, file="analysisReadyData.sn.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

#Do d'analysis as a group, but use the data whereby the learning effects were removed by subject
df.dPrime <- ch.moralsDprimeAnalysis(analysisReadyData.sn, "overlapRound", "correct", c(1,0), "targetPresent", c(TRUE,FALSE), resCol, params = params, filenameID = "gp")
write.table(df.dPrime, file="df.dPrime.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

#Do an item analysis on the data.  Doesn't matter whether use group or sn data - no rt analysis is done
itemAnalDat <- ch.moralsItemChoiceAnalysis(analysisReadyData.gp, "Item1", "Item2", "overlapRound", "dirOverlap","keyDef", respChoiceVal = c("Yes", "No"), params = params, saveFigures = T)

#### For experiments with catagory variable manipulations (e.g., different groups), do an analysis
#### by group
### direct.xvy = -1 means the lower valued object will be killed by default
### direct.xvy = 1 means the higher valued object will be killed by default

    ### type of scene by title
    grpFitModels <- ch.moralsPlotsByGrpsAndGetModels(analysisReadyData.sn, c("typeOfScen", "title"), resCol, "overlapRound", "keyDef", yesNoVal = c("Yes", "No"), "correct", c(1,0), "targetPresent", c(TRUE,FALSE), useTwoParameterModel = useTwoParameterModel, params = params, minNperOverlap = params$minOverlapN)
    ### and plot the data
    setwd(gpDir)
    ch.moralsPlotFitsByGrps(grpFitModels, c("typeOfScen", "title"), "overlapRound", analysisReadyData.sn, filenameID = params$dt.set)
    setwd(mainDir)

    ### type of scene
    grpFitModels2 <- ch.moralsPlotsByGrpsAndGetModels(analysisReadyData.sn, c("typeOfScen"), resCol, "overlapRound", "keyDef", yesNoVal = c("Yes", "No"), "correct", c(1,0), "targetPresent", c(TRUE,FALSE), useTwoParameterModel = useTwoParameterModel, params = params, minNperOverlap = params$minOverlapN)
    ### and plot the data
    setwd(gpDir)
    ch.moralsPlotFitsByGrps(grpFitModels2, c("typeOfScen"), "overlapRound", analysisReadyData.sn, filenameID = params$dt.set)
    setwd(mainDir)

		analysisReadyData.sn$defaultKilled <- ifelse(analysisReadyData.sn$direct.xVy < 0 , "defaultLVO","defaultHVO")
    ### type of scene
    grpFitModels2 <- ch.moralsPlotsByGrpsAndGetModels(analysisReadyData.sn, c("typeOfScen", "defaultKilled"), resCol, "overlapRound", "keyDef", yesNoVal = c("Yes", "No"), "correct", c(1,0), "targetPresent", c(TRUE,FALSE), useTwoParameterModel = TRUE, params = params, minNperOverlap = params$minOverlapN)
    ### and plot the data
    setwd(gpDir)
    ch.moralsPlotFitsByGrps(grpFitModels2, c("typeOfScen", "defaultKilled"), "overlapRound", analysisReadyData.sn, filenameID = params$dt.set)
    setwd(mainDir)
