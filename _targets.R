options(tidyverse.quiet = TRUE)
library(tarchetypes)
library(targets)
library(tidyverse)
source("R/functions.R")

# packages for pipeline
tar_option_set(
  packages = c("brms", "tidyverse")
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
  tar_map(
    values = tibble(outcome = c("mental_state", "BK", "DW", "IN", 
                                "PE", "EM", "AR", "OT")),
    tar_target(m1, fitModel1(dWide, outcome))
  ),
  # session info
  tar_target(sessionInfo, writeLines(capture.output(sessionInfo()), "sessionInfo.txt"))
)