options(tidyverse.quiet = TRUE)
library(tarchetypes)
library(targets)
library(tidyverse)
source("R/functions.R")

# full pipeline
list(
  # files
  tar_target(fileData, "data/dictionaries.rds", format = "file"),
  # load dictionary data
  tar_target(d, readRDS(fileData)),
  # plot proportions of definitions
  tar_target(plotProp, plotProportions(d)),
  # session info
  tar_target(sessionInfo, writeLines(capture.output(sessionInfo()), "sessionInfo.txt"))
)