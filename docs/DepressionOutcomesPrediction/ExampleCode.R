library(PatientLevelPrediction) 
options(andromedaTempFolder = "location with space to save big data")

outcomeIds = c(5412,5414,5415,5419:5426,5428,5431:5435)
database_name = "MDCD"

loadDirectory = paste0('PlpData_', database_name)
plpData <- loadPlpData(loadDirectory)

#******** EXAMPLE 1 *********
# create the split setting by specifying how you want to
# partition the data into development (train/validation) and evaluation (test or CV)
splitSettings <- createDefaultSplitSetting(testFraction = 0.25,
                                           trainFraction = 0.75,
                                           splitSeed = 42,
                                           nfold=3,
                                           type = 'stratified')

# specify any feature engineering that will be applied to the train data
# in this example we do not do any
featureEngineeringSettings <- createFeatureEngineeringSettings(type = 'none')

# specify whether to use normalization and removal of rare features
# preprocessSettings <- ...
preprocessSettings = createPreprocessSettings(
  minFraction = 0.001,
  normalize = T
)

# create population settings (this defines the labels in the data)
#create study population to develop model on
#require minimum of 365 days observation prior to at risk start
#no prior outcome and person must be observed for 365 after index (minTimeAtRisk)
#with risk window from 0 to 365 days after index
populationSettings <- createStudyPopulationSettings(firstExposureOnly = TRUE,
                                                    washoutPeriod = 365,
                                                    removeSubjectsWithPriorOutcome = TRUE,
                                                    priorOutcomeLookback = 99999,
                                                    requireTimeAtRisk = FALSE,
                                                    minTimeAtRisk=364,
                                                    riskWindowStart = 1,
                                                    binary = T,
                                                    includeAllOutcomes = T,
                                                    startAnchor = 'cohort start',
                                                    endAnchor = "cohort start",
                                                    restrictTarToCohortEnd = F,
                                                    # addExposureDaysToStart = FALSE,
                                                    riskWindowEnd = 365)
# addExposureDaysToEnd = FALSE)

# specify how you want the logging for the analysis
# generally this is saved in a file with the results
# but you can define the level of logging
logSettings <- createLogSettings(verbosity = 'DEBUG',
                                 timeStamp = T,
                                 logName = 'runPlp LR Log')

# specify what parts of the analysis to run:
# in this example we run everything
executeSettings <- createExecuteSettings(runSplitData = T,
                                         runSampleData = T,
                                         runfeatureEngineering = F,
                                         runPreprocessData = T,
                                         runModelDevelopment = T,
                                         runCovariateSummary = F)

# create the settings specifying any under/over sampling
# in this example we do not do any
# sampleSettings <- createSampleSettings(type = 'none')

#******** ********** *********

method = 'xgbModel'
modelSettings <- setGradientBoostingMachine(seed = 42,
                                            ntrees = c(100,300),
                                            nthread = 4,
                                            earlyStopRound = NULL,
                                            maxDepth = c(4,6,8),
                                            minRows = 2,
                                            learnRate = c(0.01,0.05,0.1,0.3))

# method = "lasso"
# modelSettings <- setLassoLogisticRegressionInR()

# method = "randomforest"
# modelSettings <- setRandomForest(seed = 42,
#                                  ntrees=list(100),
#                                  maxDepth = list(17),
#                                  minSamplesSplit = list(2, 5),
#                                  minSamplesLeaf = list(1, 10),
#                                  mtries = list('sqrt'),
#                                  maxSamples= list(NULL, 0.9),
#                                  classWeight = list(NULL))

imbalance_ratios <- c(20,10,2,1)

for (outcomeId in outcomeIds){
  
  try({
    saveDirectory = paste0('DepressionOutcomesResults/', database_name, "/", outcomeId)
    
    analysisId <- method
    sampleSettings <- createSampleSettings()
    
    finalModel <- runPlp(plpData = plpData,
                         outcomeId = as.numeric(outcomeId),
                         analysisId = analysisId,
                         populationSettings = populationSettings,
                         splitSettings = splitSettings,
                         sampleSettings = sampleSettings,
                         featureEngineeringSettings = featureEngineeringSettings,
                         preprocessSettings = preprocessSettings,
                         modelSettings = modelSettings,
                         logSettings = logSettings,
                         executeSettings = executeSettings,
                         saveDirectory = saveDirectory)
    saveRDS(finalModel$model, paste0(saveDirectory,"/", method, "/plpResult/model.rds"))
    
    i = 0
    for (numberOutcomestoNonOutcomes in 1/imbalance_ratios){
      i = i + 1
      analysisId <- paste0(method, '_undersampled_test', i)
      sampleSettings <- createSampleSettings(sampleSeed = 42, 
                                             type = 'underSample', 
                                             numberOutcomestoNonOutcomes = numberOutcomestoNonOutcomes)
      
      result <- runPlp(plpData = plpData,
                       outcomeId = as.numeric(outcomeId),
                       analysisId = analysisId,
                       populationSettings = populationSettings,
                       splitSettings = splitSettings,
                       sampleSettings = sampleSettings,
                       featureEngineeringSettings = featureEngineeringSettings,
                       preprocessSettings = preprocessSettings,
                       modelSettings = modelSettings,
                       logSettings = logSettings,
                       executeSettings = executeSettings,
                       saveDirectory = saveDirectory)
      saveRDS(result$model, paste0(saveDirectory,"/",analysisId,"/plpResult/model.rds"))
    }
    analysisId = paste0(method, "_original")
    readResults <- readRDS(paste0(saveDirectory,"/", method, "/plpResult/runPlp.rds"))
    saveResult <- cbind(analysisId, readResults$performanceEvaluation$evaluationStatistics)
    rm(readResults)
    readModel <- readRDS(paste0(saveDirectory,"/", method, "/plpResult/model.rds"))
    saveModel <- cbind(analysisId, readModel$settings$modelSettings$finalModelParameters)
    rm(readModel)
    i = 0
    for (numberOutcomestoNonOutcomes in 1/imbalance_ratios){
      i = i + 1
      analysisId <- paste0(method, '_undersampled_', numberOutcomestoNonOutcomes)
      readResults <- tryCatch(readRDS(paste0(saveDirectory,"/", method, "_undersampled_test",i,"/plpResult/runPlp.rds")))
      saveResult <- tryCatch(rbind(saveResult, cbind(analysisId, readResults$performanceEvaluation$evaluationStatistics)))
      rm(readResults)
      readModel <- tryCatch(readRDS(paste0(saveDirectory,"/", method, "_undersampled_test",i,"/plpResult/model.rds")))
      saveModel <- tryCatch(rbind(saveModel, cbind(analysisId, readModel$settings$modelSettings$finalModelParameters)))
      rm(readModel)
    }
    
    df1 <- data.frame(saveResult)
    saveRDS(df1, file = paste0(saveDirectory, "/results_undersampling.Rds"))
    df1_model <- data.frame(saveModel)
    saveRDS(df1_model, file = paste0(saveDirectory, "/models_undersampling.Rds"))
    
    #******** ********** *********
    
    i = 0
    for (numberOutcomestoNonOutcomes in 1/imbalance_ratios){
      i = i + 1
      analysisId <- paste0(method, '_oversampled_test', i)
      sampleSettings <- createSampleSettings(sampleSeed = 42, 
                                             type = 'overSample', 
                                             numberOutcomestoNonOutcomes = numberOutcomestoNonOutcomes)
      
      result <- runPlp(plpData = plpData,
                       outcomeId = as.numeric(outcomeId),
                       analysisId = analysisId,
                       populationSettings = populationSettings,
                       splitSettings = splitSettings,
                       sampleSettings = sampleSettings,
                       featureEngineeringSettings = featureEngineeringSettings,
                       preprocessSettings = preprocessSettings,
                       modelSettings = modelSettings,
                       logSettings = logSettings,
                       executeSettings = executeSettings,
                       saveDirectory = saveDirectory)
      saveRDS(result$model, paste0(saveDirectory,"/",analysisId,"/plpResult/model.rds"))
    }
    
    analysisId = paste0(method, "_original")
    readResults <- readRDS(paste0(saveDirectory,"/", method, "/plpResult/runPlp.rds"))
    saveResult <- cbind(analysisId, readResults$performanceEvaluation$evaluationStatistics)
    rm(readResults)
    readModel <- readRDS(paste0(saveDirectory,"/", method, "/plpResult/model.rds"))
    saveModel <- cbind(analysisId, readModel$settings$modelSettings$finalModelParameters)
    rm(readModel)
    i = 0
    for (numberOutcomestoNonOutcomes in 1/imbalance_ratios){
      i = i + 1
      analysisId <- paste0(method, '_oversampled_', numberOutcomestoNonOutcomes)
      readResults <- tryCatch(readRDS(paste0(saveDirectory,"/", method, "_oversampled_test",i,"/plpResult/runPlp.rds")))
      saveResult <- tryCatch(rbind(saveResult, cbind(analysisId, readResults$performanceEvaluation$evaluationStatistics)))
      rm(readResults)
      readModel <- tryCatch(readRDS(paste0(saveDirectory,"/", method, "_oversampled_test",i,"/plpResult/model.rds")))
      saveModel <- tryCatch(rbind(saveModel, cbind(analysisId, readModel$settings$modelSettings$finalModelParameters)))
      rm(readModel)
    }
    
    df2 <- data.frame(saveResult)
    saveRDS(df2, file = paste0(saveDirectory, "/results_oversampling.Rds"))
    df2_model <- data.frame(saveModel)
    saveRDS(df2_model, file = paste0(saveDirectory, "/models_oversampling.Rds"))
    
    df3 <- unique(rbind(df1,df2))
    saveRDS(df3, file = paste0(saveDirectory, "/results_all.Rds"))
    df3_model <- unique(rbind(df1_model, df2_model))
    saveRDS(df3_model, file = paste0(saveDirectory, "/models_all.Rds"))
  })
}
