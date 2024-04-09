###############################
#### MODEL SPECIFICATIONS #####
###############################

grpVars <- c("title", "typeOfScen", "defaultKilled")

#freeNSD
#Add NSD as a fixed parameter (NSD = 1)
#Here I group by the three variables but add not effects for them.  This will split the data properly
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- grpVars
  parameter <- "nSD"
  ParameterName <- "nSD"
  parameterBounds <- c(7, 0, 0.01)

  freeNSD  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

  #freeDB
  #Add DB as a free parameter
  columnName <- NULL
  df.code <- NULL
  GroupByVariables <- NULL
  parameter <- "db"
  ParameterName <- "db"
  parameterBounds <- c(0.5, 0, 0.001)

  freeDB  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

  #freeB
  #Add B as a free parameter
  #add a column for the fixed parameter.  This is needed because we will be adding another vc effect later
  x1 <- "default"
  #when the leftItem is HV0 then bias towards the lowerBound (choose LVO: bias = right Item)
  v1 <- 1

  columnName <- "BconstantColumn"
  df.code <- data.frame(logic = c(x1), value = c(v1))
  GroupByVariables <- grpVars
  parameter <- "b"
  ParameterName <- "bConstant"
  parameterBounds <- c(200, 5, 0.1)

  freeB  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)


  #now predict the split of "defaultKilled" by a shift in the startpoint (s).  Do that by adding a dummy coded dataframe (df.code)
  #Here, we input the conditional statements for coding the dummy or effect variable.  X is the conditional, V is the value.
    x1 <- "defaultKilled == 'defaultLVO'"
    #when the defaultKilled is LVI then bias towards the upperBound (save HVI: bias = no action)
    v1 <- 1
    x2 <- "defaultKilled == 'defaultHVO'"
    #when the defaultKilled is HVI then bias towards the lowerBound (save LVI: bias = no action)
    v2 <- -1
    x3 <- "default"
    v3 <- 0

    #this is the columnName of the dummy/effect variable
    columnName <- "defaultKilledSEColumn"
    #this dataframe contains the coding inforamtion of the dummy/effect variable
    df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
    #here we have the grouping variable(s) that will be used by the ddply to create the summary dataset.
    GroupByVariables <- grpVars
    #this is the name given to the parameter that will measure the effect of this dummy/effect variable
    ParameterName <- "sDKbias"
    #this is the parameter name for the RRW model. There are specific names: s, b, nSD, db, da, vc.
    parameter <- "s"
    #These are the bounds of the parameter values: c(high, low, interval)
    parameterBounds <- c(0.8, -0.8, 0.001)

    #add them to an existing model: here we add them to the simple model to create the overall Start Effect Model
    defaultKilledSE  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)


	  #defaultKilled VC
	  #Here, we input the conditional statements for coding the dummy or effect variable.  X is the conditional, V is the value.
	    x1 <- "defaultKilled == 'defaultLVO'"
	    #when the defaultKilled is LVI then bias towards the upperBound (save HVI: bias = no action)
	    v1 <- 1
	    x2 <- "defaultKilled == 'defaultHVO'"
	    #when the defaultKilled is HVI then bias towards the lowerBound (save LVI: bias = no action)
	    v2 <- -1
	    x3 <- "default"
	    v3 <- 0

	    #this is the columnName of the dummy/effect variable
	    columnName <- "defaultKilledVCColumn"
	    #this dataframe contains the coding inforamtion of the dummy/effect variable
	    df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
	    #here we have the grouping variable(s) that will be used by the ddply to create the summary dataset.
	    GroupByVariables <- grpVars
	    #this is the name given to the parameter that will measure the effect of this dummy/effect variable
	    ParameterName <- "vcDKbias"
	    #this is the parameter name for the RRW model. There are specific names: s, b, nSD, db, da, vc.
	    parameter <- "vc"
	    #These are the bounds of the parameter values: c(high, low, interval)
	    parameterBounds <- c(5,-5, 0.01)

	    #add them to an existing model: here we add them to the simple model to create the overall Start Effect Model
	    defaultKilledVC  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)


    #agentSE
    x1 <- "typeOfScen == 'personal' & defaultKilled == 'defaultLVO'"
    v1 <- 1
    x2 <- "typeOfScen == 'personal' & defaultKilled == 'defaultHVO'"
    v2 <- -1
    x3 <- "default"
    v3 <- 0

    columnName <- "agentSEColumn"
    df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
    GroupByVariables <- grpVars
    ParameterName <- "sAgent"
    parameter <- "s"
    parameterBounds <- c(0.8, -0.8, 0.001)

    agentSE  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)


    #agentVC
    x1 <- "typeOfScen == 'personal' & defaultKilled == 'defaultLVO'"
    v1 <- 1
    x2 <- "typeOfScen == 'personal' & defaultKilled == 'defaultHVO'"
    v2 <- -1
    x3 <- "default"
    v3 <- 0

    columnName <- "agentVCColumn"
    df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
    GroupByVariables <- grpVars
    ParameterName <- "vcAgent"
    parameter <- "vc"
    parameterBounds <- c(5,-5, 0.01)

    agentVC  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)
		
		#scene boundary
    x1 <- "typeOfScen == 'personal'"
    v1 <- 1
    x2 <- "default"
    v2 <- 0

    columnName <- "agentBoundaryColumn"
    df.code <- data.frame(logic = c(x1,x2), value = c(v1,v2))
    GroupByVariables <- grpVars
    ParameterName <- "bAgent"
    parameter <- "b"
    parameterBounds <- c(100, -100, 0.01)

    agentB  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)
		
    #methodSE
     #add title SE effect to overallSE effect model
       x1 <- "title == 'injection' & defaultKilled == 'defaultLVO'"
       v1 <- 1
       x2 <- "title == 'injection' & defaultKilled == 'defaultHVO'"
       v2 <- -1
       x3 <- "default"
       v3 <- 0

       columnName <- "methodSEColumn"
       df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
       GroupByVariables <- grpVars
       ParameterName <- "sMethod"
       parameter <- "s"
       parameterBounds <- c(0.8, -0.8, 0.001)

     methodSE  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)

     #methodVC
     #add title SE effect to overallSE effect model
       x1 <- "title == 'injection' & defaultKilled == 'defaultLVO'"
       v1 <- 1
       x2 <- "title == 'injection' & defaultKilled == 'defaultHVO'"
       v2 <- -1
       x3 <- "default"
       v3 <- 0

       columnName <- "methodVCColumn"
       df.code <- data.frame(logic = c(x1,x2,x3), value = c(v1,v2,v3))
       GroupByVariables <- grpVars
       ParameterName <- "vcMethod"
       parameter <- "vc"
       parameterBounds <- c(5,-5, 0.01)

     methodVC  <- rrwCreateParameterEffect(parameter = parameter, columnName = columnName, ParameterName = ParameterName, parameterBounds = parameterBounds, df.code = df.code, GroupByVariables = GroupByVariables)


#########################
### build models
#########################

simpleModelList <- NULL
simpleModelList <- rrwAddParameterEffectListToRRWModel(simpleModelList, c(freeNSD, freeDB, freeB))

dkVCModelList <- rrwAddParameterEffectListToRRWModel(simpleModelList, c(defaultKilledVC))
agentVCmodelList <- rrwAddParameterEffectListToRRWModel(simpleModelList, c(agentVC))

methodSEmodelList <- rrwAddParameterEffectListToRRWModel(simpleModelList, c(methodSE))
methodVCmodelList <- rrwAddParameterEffectListToRRWModel(simpleModelList, c(methodVC))

agentVC_MethodVCmodelList <- rrwAddParameterEffectListToRRWModel(methodVCmodelList, c(agentVC))
agentVC_MethodSEmodelList <- rrwAddParameterEffectListToRRWModel(methodSEmodelList, c(agentVC))

agentVC_MethodVC_dkVCmodelList <- rrwAddParameterEffectListToRRWModel(agentVC_MethodVCmodelList, c(defaultKilledVC))
agentVC_MethodSE_dkVCmodelList <- rrwAddParameterEffectListToRRWModel(agentVC_MethodSEmodelList, c(defaultKilledVC))

agentB_agentVC_MethodSEmodelList <- rrwAddParameterEffectListToRRWModel(agentVC_MethodSEmodelList, c(agentB))
agentB_agentVC_MethodVCmodelList <- rrwAddParameterEffectListToRRWModel(agentVC_MethodVCmodelList, c(agentB))

agentB_agentVC_MethodSE_dkVCmodelList <- rrwAddParameterEffectListToRRWModel(agentB_agentVC_MethodSEmodelList, c(defaultKilledVC))
agentB_agentVC_MethodVC_dkVCmodelList <- rrwAddParameterEffectListToRRWModel(agentB_agentVC_MethodVCmodelList, c(defaultKilledVC))



allModels <- list(agentB_agentVC_MethodSEmodelList = agentB_agentVC_MethodSEmodelList,
			agentB_agentVC_MethodVCmodelList = agentB_agentVC_MethodVCmodelList,
			agentVC_MethodSEmodelList = agentVC_MethodSEmodelList,
			agentVC_MethodVCmodelList=agentVC_MethodVCmodelList,
			simpleModelList = simpleModelList,
			agentVC_MethodVC_dkVCmodelList = agentVC_MethodVC_dkVCmodelList,
			agentVC_MethodSE_dkVCmodelList = agentVC_MethodSE_dkVCmodelList,
			agentB_agentVC_MethodSE_dkVCmodelList = agentB_agentVC_MethodSE_dkVCmodelList,
			agentB_agentVC_MethodVC_dkVCmodelList = agentB_agentVC_MethodVC_dkVCmodelList)
			
allFixedModels <- NULL
