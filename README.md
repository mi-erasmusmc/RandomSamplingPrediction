RandomSamplingPrediction
========================

Edited copy of PatientLevelPrediction used to investigate the impact of random oversampling and random undersampling on the performance of prediction models developed using observational health data.

Documentation of PatientLevelPrediction can be found on the [package website](https://ohdsi.github.io/PatientLevelPrediction).


Features
========

- Random oversampling and random undersampling applied to training folds within cross-validation only: [Sampling.R](https://github.com/mi-erasmusmc/RandomSamplingPrediction/blob/master/R/Sampling.R).
- Recalibration based on target imbalance ratio used for random oversampling or random undersampling: [RecalibrateValidatePlp.R](https://github.com/mi-erasmusmc/RandomSamplingPrediction/blob/master/R/RecalibrateValidatePlp.R).


Example code
============

Code used to investigate the impact of random oversampling and random undersampling on the performance of prediction models developed using observational health data: [ExampleCode.R](https://github.com/mi-erasmusmc/RandomSamplingPrediction/blob/master/docs/DepressionOutcomesPrediction/ExampleCode.R).

Cohort definitions
==================

Prediction models for various outcomes of interest within a target population of people with pharmaceutically treated depression:
- Study settings: [settings](https://github.com/mi-erasmusmc/RandomSamplingPrediction/tree/master/docs/DepressionOutcomesPrediction/settings).
- Cohort definitions: [cohorts](https://github.com/mi-erasmusmc/RandomSamplingPrediction/tree/master/docs/DepressionOutcomesPrediction/cohorts).
- Sql queries for cohorts: [sql](https://github.com/mi-erasmusmc/RandomSamplingPrediction/tree/master/docs/DepressionOutcomesPrediction/sql).
- Code used to extract data: [CodeToRun.R](https://github.com/mi-erasmusmc/RandomSamplingPrediction/blob/master/docs/DepressionOutcomesPrediction/CodeToRun.R).
