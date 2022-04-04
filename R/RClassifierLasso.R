# @file RClassifierLasso.R
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

#' Create setting for lasso logistic regression model using glmnet implementation
#'
#' @param nlambda     The number of lambda values (not a hyperparameter)
#'
#' @examples
#' model.lasso <- setLassoInR()
#'
#' @export
setLassoLogisticRegressionInR <- function(nlambda=100){

  ensure_installed("glmnet")
  
  if(!class(nlambda) %in% c("numeric", "integer"))
    stop('ntrees must be a numeric value >0 ')
  if(sum(nlambda < 1)>0)
    stop('ntrees must be greater that 0 or -1')
  
  param <- split(
    expand.grid(
      nlambda=nlambda
    ),
    1:(length(nlambda))
  )
  
  attr(param, 'settings') <- list(
    modelType = 'LassoLogisticRegression',
    modelName = "Lasso Logistic Regression",
    varImpRFunction = 'varImpLassoInR', 
    trainRFunction = 'fitRclassifierLasso',
    predictRFunction = 'predictLassoInR'
  )
  
  attr(param, 'saveType') <- 'RtoJson'
  
  result <- list(
    fitFunction = "fitRclassifierLassoInR",
    param = param
  )
  
  class(result) <- 'modelSettings' 
  
  return(result)
}


varImpLassoInR <- function(
  model,
  covariateMap,
  optimalParamInd
){
  
  varImp <- covariateMap
  varImp <- varImp %>% 
    dplyr::mutate(covariateValue = as.numeric(model$beta[,optimalParamInd])) %>% # check if glmnet changes variable order 
    dplyr::mutate(included = as.numeric(.data$covariateValue!=0)) %>% 
    dplyr::select(.data$covariateId, .data$covariateValue, .data$included)
  
  return(varImp)
}

predictLassoInR <- function(
  plpModel,
  data, 
  cohort
){
  
  # check plpModel is plpModel:
  if(class(plpModel) != 'plpModel'){
    stop("Model needs to be plpModel")
  }
  # check data is plpData:
  if(class(data) != 'plpData'){
    stop("Data needs to be plpData")
  }
  
  # convert
  matrixObjects <- toSparseM(
    plpData = data, 
    cohort = cohort,
    map = plpModel$covariateImportance %>% 
      dplyr::select(.data$columnId, .data$covariateId)
  )
  
  # use the include??
  newData <- matrixObjects$dataMatrix
  cohort <- matrixObjects$labels
  
  pred <- data.frame(value = stats::predict(plpModel$model, newData, type = "response", s = plpModel$settings$modelSettings$finalModelParameters[1,1]))
  prediction <- cohort
  prediction$value <- pred[,1]
  
  # fix the rowIds to be the old ones?
  # now use the originalRowId and remove the matrix rowId
  prediction <- prediction %>% 
    dplyr::select(-.data$rowId) %>%
    dplyr::rename(rowId = .data$originalRowId)
  
  attr(prediction, "metaData") <- list(modelType = attr(plpModel, "modelType"))
  
  return(prediction)
}


# this is a generic wrapper for training models using classifiers in R
fitRclassifierLassoInR <- function(trainData, param, search = 'grid', analysisId){
  
  if (!FeatureExtraction::isCovariateData(trainData$covariateData)){
    stop("Needs correct covariateData")
  }
  
  # need this for computeGridPerformance
  # add folds to labels if present:
  if(!is.null(trainData$folds$validation)){
    trainData$labels <- merge(trainData$labels, 
                              merge(trainData$folds$train, trainData$folds$validation, all=TRUE),
                              by = 'rowId', all = TRUE)
  }
  
  settings <- attr(param, 'settings')
  ParallelLogger::logInfo(paste0('Training ', settings$modelName))
  
  set.seed(settings$seed)
  
  # convert data into sparse Matrix:
  result <- toSparseM(
    trainData,
    map=NULL
  )
  
  dataMatrix <- result$dataMatrix
  labels <- result$labels
  covariateRef <- result$covariateRef
  
  # set test/train sets (for printing performance as it trains)
  start <- Sys.time()
  
  # use the new R CV wrapper for LASSO LR in R
  cvResult <- applyCrossValidationLassoInR(
    dataMatrix, 
    labels, 
    hyperparamGrid = param, 
    covariateMap = result$covariateMap,
    folds = trainData$folds
  )
  
  hyperSummary <- do.call(rbind, lapply(cvResult$paramGridSearch, function(x) x$hyperSummary))
  
  prediction <- cvResult$prediction
  
  variableImportance <- cvResult$variableImportance
  
  covariateRef <- merge(covariateRef, variableImportance, all.x = T, by = 'covariateId')
  # covariateRef$covariateValue[is.na(covariateRef$covariateValue)] <- 0 # should not be needed
  # covariateRef$included[is.na(covariateRef$included)] <- 0 # should not be needed
  
  comp <- start - Sys.time()
  
  result <- list(
    model = cvResult$model,
    
    prediction = prediction,
    
    settings = list(
      plpDataSettings = attr(trainData, "metaData")$plpDataSettings,
      covariateSettings = attr(trainData, "metaData")$covariateSettings,
      featureEngineering = attr(trainData$covariateData, "metaData")$featureEngineering,
      tidyCovariates = attr(trainData$covariateData, "metaData")$tidyCovariateDataSettings, 
      #covariateMap = covariateMap, this is in covariateImportance
      requireDenseMatrix = F,
      populationSettings = attr(trainData, "metaData")$populationSettings,
      modelSettings = list(
        model = attr(param, 'settings')$trainRFunction, 
        param = param,
        finalModelParameters = cvResult$finalParam,
        extraSettings = attr(param, 'settings')
      ),
      splitSettings = attr(trainData, "metaData")$splitSettings,
      sampleSettings = attr(trainData, "metaData")$sampleSettings
    ),
    
    trainDetails = list(
      analysisId = analysisId,
      cdmDatabaseSchema = attr(trainData, "metaData")$cdmDatabaseSchema,
      outcomeId = attr(trainData, "metaData")$outcomeId,
      cohortId = attr(trainData, "metaData")$cohortId,
      attrition = attr(trainData, "metaData")$attrition, 
      trainingTime = comp,
      trainingDate = Sys.Date(),
      hyperParamSearch = hyperSummary
    ),
    
    covariateImportance = covariateRef
  )
  
  class(result) <- "plpModel"
  attr(result, "predictionFunction") <- settings$predictRFunction
  attr(result, "modelType") <- "binary"
  attr(result, "saveType") <- attr(param, 'saveType')
  
  return(result)
}

applyCrossValidationLassoInR <- function(dataMatrix, labels, hyperparamGrid, covariateMap, folds){
  
  # fit full model to get hyperparamGrid
  trainIds <- labels$originalRowId %in% folds$train$rowId

  fullModel <- do.call(
    glmnet::glmnet,
    list(
      x = dataMatrix[trainIds,],
      y = labels[trainIds,]$outcomeCount,
      family = "binomial",
      nlambda = hyperparamGrid[[1]]
    )  
  )
  
  lambdas <- fullModel$lambda
  
  gridSearchPredictions <- list()
  length(gridSearchPredictions) <- length(lambdas)
  models <- list()
  minPred <- NULL
  
  for(gridId in 1:length(lambdas)){
    param <- fullModel$lambda[gridId]
    gridSearchPredictions[[gridId]]$param <- data.frame(lambda = param) # assuming same lambdas for every index
  }
  
  for(i in unique(folds$train$index)){
    
    trainIds <- labels$originalRowId %in% folds$train[folds$train$index != i,]$rowId
    validationIds <- labels$originalRowId %in% folds$validation[folds$validation$index == i,]$rowId
    
    models[[i]] <- do.call(
      glmnet::glmnet,
      list(
        x = dataMatrix[trainIds,],
        y = labels[trainIds,]$outcomeCount,
        lambda = lambdas,
        family = "binomial"
      )  
    )
    
    pred <- data.frame(value = stats::predict(models[[i]], dataMatrix[validationIds,], type = "response"))
    for(gridId in 1:ncol(pred)){
      prediction <- labels[validationIds,]
      attr(prediction, "metaData") <- list(modelType = "binary") # make this some attribute of model
      prediction$value <- pred[,gridId]
      # fix the rowIds to be the old ones?
      # now use the originalRowId and remove the matrix rowId
      prediction <- prediction %>% 
        dplyr::select(-.data$rowId) %>%
        dplyr::rename(rowId = .data$originalRowId)
      # save hyper-parameter cv prediction
      gridSearchPredictions[[gridId]]$prediction <- rbind(gridSearchPredictions[[gridId]]$prediction, prediction)
    }
    minPred <- min(minPred, ncol(pred))
  }
  
  # computeGridPerformance function is currently in SklearnClassifier.R
  paramGridSearch <- lapply(gridSearchPredictions, function(x) do.call(computeGridPerformance, x))  # cvAUCmean, cvAUC, param
  
  optimalParamInd <- which.max(unlist(lapply(paramGridSearch, function(x) x$cvPerformance))[1:minPred])
  
  finalParam <- paramGridSearch[[optimalParamInd]]$param
  
  cvPrediction <- gridSearchPredictions[[optimalParamInd]]$prediction
  cvPrediction$evaluationType <- 'CV'
  
  # validate final model
  validationIds <- labels$originalRowId %in% folds$validation$rowId
  
  pred <- data.frame(value = stats::predict(fullModel, dataMatrix[validationIds,], type = "response", s = finalParam[1,1]))
  prediction <- labels[validationIds,]
  param <- fullModel$lambda
  attr(prediction, "metaData") <- list(modelType = "binary") # make this some attribute of model
  prediction$value <- pred[,1]
  
  # fix the rowIds to be the old ones?
  # now use the originalRowId and remove the matrix rowId
  prediction <- prediction %>% 
    dplyr::select(-.data$rowId) %>%
    dplyr::rename(rowId = .data$originalRowId)
  
  prediction$evaluationType <- 'Train'
  
  prediction <- rbind(
    prediction,
    cvPrediction
  )
  
  variableImportance <- tryCatch(
    {do.call(
      attr(hyperparamGrid, 'settings')$varImpRFunction, 
      list(model = fullModel, covariateMap = covariateMap,
           optimalParamInd = optimalParamInd)
    )}
    ,
    error = function(e){
      ParallelLogger::logInfo('Error calculating variableImportance');
      ParallelLogger::logInfo(e);
      return(NULL)
    })
  
  result <- list(
    model = fullModel,
    prediction = prediction,
    finalParam = finalParam,
    paramGridSearch = paramGridSearch,
    variableImportance = variableImportance
  )
  
  return(result)
}
