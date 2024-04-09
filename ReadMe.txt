Run the analyses in the following order.  

For each experiment
	1) Open R
	2) change directories to the experiments directory
	3) Make sure all required libraries are installed
	4) run the main analysis: source("moralsDataPrep.r")
	5) when that is complete, run the following: source("getActionStats.r")

After those analyses are run for each experiment, change directories to the directory above the Experiment directories. Then run the following code: source("compBiases.r")

To run the RRW for each experiment
	1) Open R
	2) change directories to the RRW directory in the experiments directory (e.g., Exp1/RRW)
	3) Make sure all required libraries are installed
	4) run the main analysis: source("runRRW_RB2.r")
	5) when that is complete, run the following: source("runRRW_RB_kFold.r")
