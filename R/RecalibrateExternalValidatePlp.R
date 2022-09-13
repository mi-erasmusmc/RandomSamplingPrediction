# @file RecalibrateExternalValidatePlp.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


recalibrateExternalValidatePlp <- function(
  plpModel,
  plpDataVal,
  dataDev,
  databaseName = 'database 1',
  populationVal,
  numberOutcomestoNonOutcomes # inverse of imbalance ratio
){
  
  # Apply model 
  #======= 
  prediction <- tryCatch({
    predictPlp(
      plpModel = plpModel, 
      plpData = plpDataVal, 
      population = populationVal
    )},
    error = function(e){ParallelLogger::logError(e)}
  )
  
  prediction$evaluationType <- 'Validation'
  
  # Recalibrate
  #=======
  observedRisk <- sum(dataDev$Train$labels$outcomeCount)/nrow(dataDev$Train$labels)
  obsOdds <- observedRisk/ (1-observedRisk)
  meanPredictionRisk <- numberOutcomestoNonOutcomes/(numberOutcomestoNonOutcomes+1)
  meanPredictionRisk <- max(meanPredictionRisk, observedRisk)
  predOdds <- meanPredictionRisk / (1 -  meanPredictionRisk)
  
  correctionFactor <- log(obsOdds / predOdds)
  
  recalibrated <- prediction
  recalibrated$value = logitFunct(inverseLogit(recalibrated$value) + correctionFactor)
  
  recalibrated[,'evaluationType'] <- 'recalibrationInTheLargeValidation'
  prediction <- recalibrated
  attr(prediction, 'metaData')$recalibrationInTheLarge = list(correctionFactor = correctionFactor)
  
  
  # Evaluate 
  #=======
  performance <- tryCatch({
    evaluatePlp(prediction = prediction, typeColumn = 'evaluationType')
  },
  error = function(e){ParallelLogger::logError(e)}
  )
  
  # step 6: covariate summary
  labels <- tryCatch({
    populationVal %>% dplyr::select(.data$rowId, .data$outcomeCount)
  },
  error = function(e){ return(NULL) }
  )
  covariateSum <- NULL
  
  executionSummary <- list(
    ExecutionDateTime = Sys.Date(),
    PackageVersion = list(
      rVersion= R.Version()$version.string,
      packageVersion = utils::packageVersion("PatientLevelPrediction")
    ),
    PlatformDetails= list(
      platform = R.Version()$platform,
      cores = Sys.getenv('NUMBER_OF_PROCESSORS'),
      RAM = memuse::Sys.meminfo()[1]
    ) #  test for non-windows needed
  )
  
  model = list(
    model = 'recalibrated validation of model',
    settings = plpModel$settings,
    validationDetails   = list(
      analysisId = plpModel$trainDetails$analysisId, #TODO add from model
      analysisSource = '', #TODO add from model
      developmentDatabase = plpModel$trainDetails$cdmDatabaseSchema,
      cdmDatabaseSchema = databaseName,
      populationSettings = attr(populationVal, 'metaData')$populationSettings,
      outcomeId = attr(populationVal, 'metaData')$outcomeId,
      cohortId = attr(plpDataVal, 'metaData')$cohortId,
      attrition = attr(populationVal, 'metaData')$attrition,
      validationDate = Sys.Date() # is this needed?
    )
  )
  attr(model, "predictionFunction") <- 'none'
  attr(model, "saveType") <- 'RtoJson'
  class(model) <- 'plpModel'
  
  result <- list(
    model = model,
    executionSummary = executionSummary,
    prediction = prediction,
    performanceEvaluation = performance,
    covariateSummary = covariateSum
  )
  
  class(result) <- 'recalibrateValidatePlp'
  return(result)
  
}

logitFunct <- function(values){
  return(1/(1 + exp(0 - values)))
}

inverseLogit <- function(values){
  res <- log(values/(1-values))
  return(res)
}
