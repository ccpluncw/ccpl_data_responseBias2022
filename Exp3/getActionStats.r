library(dplyr)
library(rstatix)
library(lmerTest)
library(optimx)

### read in files
inputDataFile <- "analysisReadyData.sn.txt"
analysisReadyData<-read.table(inputDataFile, header=T, sep="\t")
tmp.df<-analysisReadyData
analysisReadyData$sn <- as.factor(analysisReadyData$sn)

### code action as 1 and non-action as 0
tmp.df$action01 <- ifelse(tmp.df$keyDef == "Yes", 1,0)

### run a mixed model on method and agent 
AM.lmer.out <- lmer(action01~typeOfScen+title + (title+typeOfScen|sn), data=tmp.df,REML = FALSE, control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))

### get summary stats
tmp.df.sn.agentMethod <- data.frame(tmp.df %>% group_by(sn, typeOfScen, title) %>% summarize(mAct = mean(action01, na.rm = T), sdAct = sd(action01, na.rm = T), N = length(action01)))

sumAgentAct <- tmp.df.sn.agentMethod %>% group_by(typeOfScen) %>% summarize(mmAct = mean(mAct, na.rm = T), sdAct = sd(mAct, na.rm = T), N = length(mAct))
sumMethodAct <- tmp.df.sn.agentMethod %>% group_by(title) %>% summarize(mmAct = mean(mAct, na.rm = T), sdAct = sd(mAct, na.rm = T), N = length(mAct))
mOveralAct <- mean(tmp.df.sn.agentMethod$mAct, na.rm=T)
sdOveralAct <- mean(tmp.df.sn.agentMethod$sdAct, na.rm=T)

tmp.df.sn.agentMethod.t <- tmp.df.sn.agentMethod %>% group_by(sn) %>% summarize(mmAct = mean(mAct, na.rm = T), sdAct = sd(mAct, na.rm = T), N = length(mAct))
t.test.overall <- with(tmp.df.sn.agentMethod.t, t.test(mmAct, mu=0.5))
cohensD.overall <- cohens_d(tmp.df.sn.agentMethod.t, mmAct ~ 1, mu=0.5)

sink("no action bias stats.txt")
	cat("\n ****************** Mixed Model of action ******************\n\n")
	print(anova(AM.lmer.out))
	cat("\n ************ Summary Stats \n\n")
	cat("\n ****** Overall \n")
	cat("\n Overall mean probability of action: ", mOveralAct, "(SD =",sdOveralAct ,")\n")
	cat("\n ****** one sample t-test to see if Action is different from 0.5 \n\n")
	print(t.test.overall)
	cat("\n Cohen's D \n")	
	print(cohensD.overall)
	cat("\n\n ****** Agent \n\n")
	print(sumAgentAct)
	cat("\n ****** Method \n\n")
	print(sumMethodAct)
sink(NULL)

write.table(tmp.df.sn.agentMethod, file="pActionByParticipant.txt", append=FALSE, quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)
