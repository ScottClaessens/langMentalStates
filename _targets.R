options(tidyverse.quiet = TRUE)
library(tarchetypes)
library(targets)
library(tidyverse)
source("R/functions.R")

# packages for pipeline
tar_option_set(packages = c("brms", "readxl", "tidybayes", "tidyverse"))

# targets for model fitting
targetsModels <-
  tar_map(
    # loop over outcome variables
    values = expand_grid(
      dataset = rlang::syms(c("dWide1", "dWide2")),
      outcome = c("mental_state","BK","DW","IN","PE","EM","AR","OT"),
      modelFunction = rlang::syms(c("fitModel1", "fitModel2"))
      ),
    # fit models
    tar_target(m, modelFunction(dataset, outcome)),
    # extract language differences
    tar_target(hyp, hypothesis(m, "b_languageeng - b_languageton = 0", class = NULL))
  )

# full pipeline
list(
  
  ### Study 1 - Dictionaries
  
  # files
  tar_target(fileData1, "data/study1/dictionaries.rds", format = "file"),
  # load dictionary data
  tar_target(d1, readRDS(fileData1)),
  # plot proportions of definitions
  tar_target(plotProp1, plotProportions(d1, title = "Dictionary study",
                                        file = "plots/study1/proportions.pdf")),
  # get data in wide format for modelling
  tar_target(dWide1, pivotDataWider(d1)),
  # fit models and extract language differences
  #targetsModels,
  #tar_combine(hyp1, targetsModels$hyp[1:16], command = list(!!!.x)),
  ## plot model results
  #tar_target(plotModels1, plotModelResults(hyp1)),
  
  
  ### Study 2 - Common Crawl
  
  # files
  tar_target(fileData2, "data/study2/commonCrawl.rds", format = "file"),
  # load common crawl data
  tar_target(d2, readRDS(fileData2)),
  # plot proportions of definitions
  tar_target(plotProp2, plotProportions(d2, title = "Common crawl study",
                                        file = "plots/study2/proportions.pdf")),
  # get data in wide format for modelling
  tar_target(dWide2, pivotDataWider(d2)),
  # fit models and extract language differences
  #tar_combine(hyp2, targetsModels$hyp[17:32], command = list(!!!.x)),
  # plot model results
  #tar_target(plotModels2, plotModelResults(hyp2)),
  
  ### Session info
  
  tar_target(sessionInfo, writeLines(capture.output(sessionInfo()), "sessionInfo.txt"))
  
)