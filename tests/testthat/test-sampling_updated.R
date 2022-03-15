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

library("testthat")
context("Sampling")

testType <- sample(c('none', 'underSample', 'overSample'), 1)
testNumberOutcomestoNonOutcomes <- 2
testSampleSeed <- sample(10000,1)

sampleSettingFunc <- function(
  type = testType,
  numberOutcomestoNonOutcomes = testNumberOutcomestoNonOutcomes,
  sampleSeed = testSampleSeed 
){
  
  result <- createSampleSettings(
    type = type ,
    numberOutcomestoNonOutcomes = numberOutcomestoNonOutcomes,
    sampleSeed = sampleSeed
  )
  
  return(result)
  
}


  
test_that("createSampleSettings works", {
  
  sampleSettings <- sampleSettingFunc()
  expect_is(sampleSettings, "sampleSettings")
  
  sampleFun <- 'sameData'
  if(testType == 'underSample'){
    sampleFun <- 'underSampleData'
  }
  if(testType == 'overSample'){
    sampleFun <- 'overSampleData'
  }
  
  expect_equal(
    attr(sampleSettings, "fun") , 
    sampleFun
  )
  
  expect_equal(
    sampleSettings$numberOutcomestoNonOutcomes, 
    testNumberOutcomestoNonOutcomes
    )
  
  expect_equal(
    sampleSettings$sampleSeed , 
    testSampleSeed 
  )
  
})


test_that("createSampleSettings expected errors", {
  
  expect_error(
    sampleSettingFunc(numberOutcomestoNonOutcomes = 'fsfd')
  )
  expect_error(
    sampleSettingFunc(numberOutcomestoNonOutcomes = -1)
  )
  
  expect_error(
    sampleSettingFunc(sampleSeed =  'fsfd')
  )
  
  expect_error(
    sampleSettingFunc(type =  'fsfd')
  )
  expect_error(
    sampleSettingFunc(type =  NULL)
  )
 
})


test_that("sampleData outputs are correct", {
  
  trainData <- createTrainData(plpData, population)
  
  sampleSettings <- sampleSettingFunc(type = 'none') 
  
  sampleTrainData <- sampleData(trainData, sampleSettings)
  
  # make sure metaData captures
  expect_equal(
    length(attr(sampleTrainData, "metaData")),
    length(attr(trainData, "metaData"))+1
  )
  
  expect_equal(
    attr(sampleTrainData, "metaData")$sampleSettings,
    sampleSettings
  )
  
  # check the data is the same:
  expect_equal(
    nrow(sampleTrainData$labels), 
    nrow(trainData$labels)
  )
  
  expect_equal(
    nrow(sampleTrainData$folds), 
    nrow(trainData$folds)
  )
  
  expect_equal(
    sampleTrainData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull(), 
    trainData$covariateData$covariates  %>% dplyr::tally() %>% dplyr::pull()
  )
  
  
})

# specific functions for sampling

 
test_that("underSampleData works", {
  
  trainData <- createTrainData(plpData, population)
  trainData$folds <- list(train = trainData$folds, validation = trainData$folds)
  
  sampleSettings <- list(
    sampleSeed = 1,
    numberOutcomestoNonOutcomes = 1
    )
  
  sampleTrainData <- underSampleData(trainData, sampleSettings)
  
  expect_true(nrow(sampleTrainData$labels) == nrow(trainData$labels))
  
  # the sampled train folds data should be smaller...
  expect_true(
    2*sum((sampleTrainData$label %>% dplyr::filter(.data$rowId %in% sampleTrainData$folds$train$rowId))$outcomeCount) == nrow(sampleTrainData$folds$train)
  )
  
  expect_true(nrow(sampleTrainData$folds$train) <= nrow(trainData$folds$train))
  expect_true(nrow(sampleTrainData$folds$train) <= nrow(trainData$folds$validation))
  expect_true(nrow(sampleTrainData$folds$validation) == nrow(trainData$folds$validation))
  expect_true(sum(sampleTrainData$folds$validation == trainData$folds$validation)==2*nrow(trainData$folds$validation))
  
  # the sampled train folds data should be a subset of the validation folds data...
  expect_true(nrow(sampleTrainData$folds$train %>% dplyr::filter(.data$rowId%in%trainData$folds$train$rowId))==nrow(sampleTrainData$folds$train))
  expect_true(nrow(sampleTrainData$folds$train %>% dplyr::filter(.data$rowId%in%trainData$folds$validation$rowId))==nrow(sampleTrainData$folds$train))

  expect_true(
    sampleTrainData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull() == trainData$covariateData$covariates  %>% dplyr::tally() %>% dplyr::pull()
  )
  
  # test setting numberOutcomestoNonOutcomes = 0: the sampled data should be same as input
  
  sampleSettings <- list(
    sampleSeed = 1,
    numberOutcomestoNonOutcomes = 0
  )
  sampleTrainData <- underSampleData(trainData, sampleSettings)
  
  expect_equal(
    nrow(sampleTrainData$labels), 
    nrow(trainData$labels)
  )
  
  expect_equal(
    nrow(sampleTrainData$folds$train), 
    nrow(trainData$folds$train)
  )
  
  expect_equal(
    sampleTrainData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull(), 
    trainData$covariateData$covariates  %>% dplyr::tally() %>% dplyr::pull()
  )
  
  # perhaps add manual data test
  
  
})

test_that("overSampleData works", {
  
  trainData <- createTrainData(plpData, population)
  trainData$folds <- list(train = trainData$folds, validation = trainData$folds)
  
  sampleSettings <- list(
    sampleSeed = 1,
    numberOutcomestoNonOutcomes = 1
  )
  
  sampleTrainData <- overSampleData(trainData, sampleSettings)
  
  # the sampled data should be larger with unique rowIds
  expect_true(2*sum(sampleTrainData$labels$outcomeCount) == nrow(sampleTrainData$labels))

  expect_true(nrow(sampleTrainData$labels) >= nrow(trainData$labels))
  expect_true(length(unique(sampleTrainData$labels$rowId))==nrow(sampleTrainData$labels))
  
  expect_true(nrow(sampleTrainData$folds$train) >= nrow(trainData$folds$train))
  expect_true(length(unique(sampleTrainData$folds$train$rowId))==nrow(sampleTrainData$folds$train))
  expect_true(nrow(sampleTrainData$folds$train) >= nrow(trainData$folds$validation))
  expect_true(nrow(sampleTrainData$folds$validation) == nrow(trainData$folds$validation))
  expect_true(sum(sampleTrainData$folds$validation == trainData$folds$validation)==2*nrow(trainData$folds$validation))
  
  # the validation folds data should be a subset of the sampled train folds data...
  expect_true(nrow(sampleTrainData$folds$validation %>% dplyr::filter(.data$rowId%in%trainData$folds$train$rowId))==nrow(sampleTrainData$folds$validation))
  expect_true(nrow(sampleTrainData$folds$validation %>% dplyr::filter(.data$rowId%in%trainData$folds$validation$rowId))==nrow(sampleTrainData$folds$validation))

  expect_true(
    sampleTrainData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull() >= trainData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull()
  )
  
  # check if all rowIds match..
  expect_true(
    nrow(sampleTrainData$covariateData$covariates %>% dplyr::collect() %>% dplyr::filter(.data$rowId %in% sampleTrainData$labels$rowId)) == nrow(sampleTrainData$covariateData$covariates)
  )
  
  # test setting numberOutcomestoNonOutcomes = 0: the sampled data should be same as input
  
  sampleSettings <- list(
    sampleSeed = 1,
    numberOutcomestoNonOutcomes = 0
  )
  sampleTrainData <- overSampleData(trainData, sampleSettings)
    
  expect_equal(
    nrow(sampleTrainData$labels), 
    nrow(trainData$labels)
  )
  
  expect_equal(
    nrow(sampleTrainData$folds$train), 
    nrow(trainData$folds$train)
  )
  
  expect_equal(
    sampleTrainData$covariateData$covariates %>% dplyr::tally() %>% dplyr::pull(), 
    trainData$covariateData$covariates  %>% dplyr::tally() %>% dplyr::pull()
  )
  
  # perhaps add manual data test
  
  
})
