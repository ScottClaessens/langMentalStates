options(tidyverse.quiet = TRUE)
library(tarchetypes)
library(targets)
library(tidyverse)
source("R/functions_study1.R")
source("R/functions_study2.R")

# packages for pipeline
tar_option_set(
  packages = c("brms", "tidybayes", "tidyverse")
)

# targets for model fitting
targetsModels <-
  tar_map(
    # loop over outcome variables
    values = tibble(outcome = c("mental_state","BK","DW","IN","PE","EM","AR","OT")),
    # fit models
    tar_target(m1, fitModel1(dWide, outcome)),
    tar_target(m2, fitModel2(dWide, outcome)),
    # extract language differences
    tar_target(hyp1, hypothesis(m1, "b_languageeng - b_languageton = 0", class = NULL)),
    tar_target(hyp2, hypothesis(m2, "b_languageeng - b_languageton = 0", class = NULL))
  )

# full pipeline
list(
  
  ### Study 1 - Dictionaries
  
  # files
  tar_target(fileData, "data/dictionaries.rds", format = "file"),
  # load dictionary data
  tar_target(d, readRDS(fileData)),
  # plot proportions of definitions
  tar_target(plotProp, plotProportions(d)),
  # get data in wide format for modelling
  tar_target(dWide, pivotDataWider(d)),
  # fit models and extract language differences
  targetsModels,
  tar_combine(hyp1, targetsModels[["hyp1"]], command = list(!!!.x)),
  tar_combine(hyp2, targetsModels[["hyp2"]], command = list(!!!.x)),
  # plot model results
  tar_target(plotModels, plotModelResults(hyp1, hyp2)),
  
  ### Study 2 - Common Crawl
  
  #...
  
  # session info
  tar_target(sessionInfo, writeLines(capture.output(sessionInfo()), "sessionInfo.txt"))
)