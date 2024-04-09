##### THis code must be run AFTER the individual experiment analyses are run.  
##### So, run this code lastQS

library(dplyr)
library(rstatix)
library(pwr)

#power for inaction bias
largeInaction <- pwr.t.test(d = 0.8, sig.level = 0.05, power = 0.9, type='paired')
mediumInaction <- pwr.t.test(d = 0.5, sig.level = 0.05, power = 0.9, type='paired')

dataFile <- "pActionByParticipant.txt"

####
exp1Dir <- "./Exp1/"
exp2Dir <- "./Exp2/"
exp3Dir <- "./Exp3/"

inputDataFile <- paste(exp1Dir,dataFile, sep="")
df.exp1.sn <- read.table(inputDataFile, header=T, sep="\t")
#df.exp1.sn <- df.exp1 %>% group_by(sn) %>% summarize (mmAct = mean(mAct, na.rm = T))
df.exp1.sn$expNum <- 1

inputDataFile <- paste(exp2Dir,dataFile, sep="")
df.exp2.sn <- read.table(inputDataFile, header=T, sep="\t")
#df.exp2.sn <- df.exp2 %>% group_by(sn) %>% summarize (mmAct = mean(mAct, na.rm = T))
df.exp2.sn$expNum <- 2

inputDataFile <- paste(exp3Dir,dataFile, sep="")
df.exp3.sn <- read.table(inputDataFile, header=T, sep="\t")
#df.exp3.sn <- df.exp3 %>% group_by(sn) %>% summarize (mmAct = mean(mAct, na.rm = T))
df.exp3.sn$expNum <- 3

df.exp1_2 <- rbind(df.exp1.sn, df.exp2.sn)
df.exp2_3 <- rbind(df.exp2.sn, df.exp3.sn)
df.exp1_2$expNum <- as.factor(df.exp1_2$expNum)
df.exp2_3$expNum <- as.factor(df.exp2_3$expNum)


df.exp1_2.1 <- df.exp1_2 %>% group_by(sn,expNum) %>% summarize(mmAct = mean(mAct, na.rm = T), sdAct = sd(mAct, na.rm = T), N = length(mAct))
t.test.1v2 <- with(df.exp1_2.1, t.test(mmAct ~ expNum))
cohensD.1v2 <- cohens_d(as.data.frame(df.exp1_2.1), mmAct ~ expNum)

df.exp2_3.1 <- df.exp2_3[df.exp2_3$title == "strangle" | df.exp2_3$title == "injection",] %>% group_by(sn,expNum) %>% summarize(mmAct = mean(mAct, na.rm = T), sdAct = sd(mAct, na.rm = T), N = length(mAct))
t.test.2v3.1 <- with(df.exp2_3.1, t.test(mmAct ~ expNum))
cohensD.2v3.1 <- cohens_d(as.data.frame(df.exp2_3.1), mmAct ~ expNum)

df.exp2_3.2 <- df.exp2_3[df.exp2_3$title == "unidentified",] %>% group_by(sn, expNum) %>% summarize(mmAct = mean(mAct, na.rm = T), sdAct = sd(mAct, na.rm = T), N = length(mAct))
t.test.2v3.2 <- with(df.exp2_3.2, t.test(mmAct ~ expNum))
cohensD.2v3.2 <- cohens_d(as.data.frame(df.exp2_3.2), mmAct ~ expNum)


sink("compInactionBias.txt")
	cat("\n ****************** Exp 1 v Exp 2 ******************\n\n")
	cat("\n t-test \n")	
	print(t.test.1v2)
	cat("\n Cohen's D \n")	
	print(cohensD.1v2)
	cat("\n ****************** Exp 2 v Exp 3 ******************\n\n")
	cat("\n *********** Strangle vs injection \n\n") 
	cat("\n t-test \n")	
	print(t.test.2v3.1)
	cat("\n Cohen's D \n")	
	print(cohensD.2v3.1)
	cat("\n *********** unidentified \n\n") 
	cat("\n t-test \n")	
	print(t.test.2v3.2)
	cat("\n Cohen's D \n")	
	print(cohensD.2v3.2)
sink(NULL)
	


