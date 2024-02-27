options(tidyverse.quiet = TRUE)
library(tarchetypes)
library(targets)
library(tidyverse)
source("R/functions.R")

# options for running in parallel with clustermq
options(
  clustermq.scheduler = "slurm", 
  clustermq.template = "slurm_clustermq.tmpl"
)

# packages for pipeline
tar_option_set(packages = c("brms","readxl","scales","tidybayes","tidyverse"))

# targets for study 1 model fitting
targetsModels1 <-
  tar_map(
    # loop over outcome variables
    values = expand_grid(
      dataset = rlang::syms("d1"),
      outcome = c("mental_state","BK","DW","IN","PE","EM","AR","OT"),
      modelFunction = rlang::syms(c("fitModel1", "fitModel2"))
      ),
    # fit models
    tar_target(m, modelFunction(dataset, outcome)),
    # extract language differences
    tar_target(hyp, hypothesis(m, "b_languageeng - b_languageton = 0", 
                               class = NULL))
  )

# targets for study 2 model fitting
targetsModels2 <-
  tar_map(
    # loop over outcome variables
    values = expand_grid(
      dataset = rlang::syms("d2"),
      mentalStateVar = c("mental_state","BK","DW","IN","PE","EM","AR","OT"),
      modelFunction = rlang::syms(c("fitUsageModel1", "fitUsageModel2"))
    ),
    # fit models
    tar_target(m, modelFunction(dataset, mentalStateVar))
  )

# full pipeline
list(
  
  ### Study 1 - Dictionaries
  
  # files
  tar_target(fileData1, "data/study1/dictionaries.rds", format = "file"),
  # load dictionary data
  tar_target(d1, readRDS(fileData1)),
  # plot proportions of definitions
  tar_target(plotProp1, plotProportions1(d1)),
  # fit models and extract language differences
  targetsModels1,
  tar_combine(hyp1, targetsModels1$hyp, command = list(!!!.x)),
  # plot model results
  tar_target(plotModels1, plotModelResults1(hyp1)),
  
  
  ### Study 2 - Common Crawl
  
  # files
  tar_target(fileData2, "data/study2/combinedDomainData.rds", format = "file"),
  # load common crawl data
  tar_target(d2, loadData2(fileData2)),
  # plot proportions
  tar_target(plotProp2, plotProportions2(d2)),
  # fit usage models and extract language differences
  targetsModels2,
  
  
  ### Session info
  
  tar_target(sessionInfo, writeLines(capture.output(sessionInfo()), 
                                     "sessionInfo.txt"))
  
)
