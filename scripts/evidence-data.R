#reading in and cleaning the evidence-experiment data
library(tidyverse)
library(haven)
library(here)

GOOGLE_DRIVE <- Sys.getenv("GOOGLE_DRIVE")
evidence <- here(GOOGLE_DRIVE, "Shared drives", "evidence-experiment", "economist-20260117.sav") |> 
  read_sav() |> 
  mutate(
    ICE_Video = as.integer(ICE_Video),  # keep as 0/1 for math
    collapsed_supp_abolishice = case_match( #collapse into 3 categories
      supp_abolishice,
      c(1, 2) ~ 1,
      c(3, 4) ~ 2,
      5 ~ 3))

