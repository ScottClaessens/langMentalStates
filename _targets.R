options(tidyverse.quiet = TRUE)
library(tarchetypes)
library(targets)
library(tidyverse)
source("R/functions.R")

# packages for pipeline
tar_option_set(
  packages = c("brms", "tidybayes", "tidyverse")
)

# targets for model fitting
targetsModels <-
  tar_map(
    values = tibble(outcome = c("mental_state", "BK", "DW", "IN", 
                                "PE", "EM", "AR", "OT")),
    tar_target(m1, fitModel1(dWide, outcome)),
    tar_target(hyp1, hypothesis(m1, "b_languageeng - b_languageton = 0", class = NULL))
  )

# full pipeline
list(
  # files
  tar_target(fileData, "data/dictionaries.rds", format = "file"),
  # load dictionary data
  tar_target(d, readRDS(fileData)),
  # plot proportions of definitions
  tar_target(plotProp, plotProportions(d)),
  # get data in wide format for modelling
  tar_target(dWide, pivotDataWider(d)),
  # fit models
  targetsModels,
  tar_combine(hyp1, targetsModels[["hyp1"]], command = list(!!!.x)),
  # plot model results
  tar_target(plotM1, plotModel(hyp1, filename = "plots/model1.pdf")),
  # session info
  tar_target(sessionInfo, writeLines(capture.output(sessionInfo()), "sessionInfo.txt"))
)