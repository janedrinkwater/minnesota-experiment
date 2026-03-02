#reading in and cleaning the evidence-experiment data
library(tidyverse)
library(haven)
library(here)

GOOGLE_DRIVE <- Sys.getenv("GOOGLE_DRIVE")

vars <- c(
  "good_threaten",
  "good_obey",
  "good_car_contact",
  "good_attempt_runover",
  "good_ice_injured",
  "good_ice_professional",
  "justified_ice_shooting",
  "ice_mn_killing_invest",
  "ice_mn_good_widow",
  "collapsed_abolishice")

evidence <- here(GOOGLE_DRIVE, "Shared drives", "evidence-experiment", "economist-20260117.sav") |> 
  read_sav() |> 
  mutate(
    ICE_Video = if_else(ICE_Video == 0, 1, 0), #making 0 control & 1 treatment
    collapsed_abolishice = case_match( #collapsing into 3 categories
      supp_abolishice,
      c(1, 2) ~ 1,
      c(3, 4) ~ 2,
      5 ~ 3)) |>
  filter(!is.na(ICE_Video)) |> #getting rid of those who didn't answer the treatment/control question
  pivot_longer(cols = all_of(vars), #pivoting longer for easier analysis
               names_to = "question",
               values_to = "response") |> 
  mutate(question = fct_relevel(question, rev(vars))) #making sure the questions are in the right order

write_rds(evidence, here("data", "evidence-cleaned.rds"))
