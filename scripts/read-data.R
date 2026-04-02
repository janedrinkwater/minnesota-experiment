#reading in and cleaning the evidence-experiment data
library(tidyverse)
library(haven)
library(here)
library(labelled)

# read spss file
GOOGLE_DRIVE <- Sys.getenv("GOOGLE_DRIVE")
sav <- here(GOOGLE_DRIVE, 
            "Shared drives", 
            "evidence-experiment", 
            "economist-20260117.sav") |> 
  read_sav() 

# save labels for later use
lbls <- tibble(
  var_name = c(
    "good_threaten",
    "good_obey",
    "good_car_contact",
    "good_attempt_runover",
    "good_ice_injured",
    "good_ice_professional",
    "justified_ice_shooting",
    "ice_mn_killing_invest",
    "ice_mn_good_widow",
    "supp_abolishice"),
  label = map_chr(var_name, \(v) var_label(sav[[v]]))
)

# convert to factors, create long form dataset
sav <- sav |> 
  filter(complete.cases(ICE_Video)) |> # include only those randomized to treatment
  mutate(
    across(where(is.labelled), as_factor),
    across(where(is.factor), fct_drop),
    treatment = ICE_Video == "Shown video",
    supp_abolishice = fct_collapse(supp_abolishice,
                                  "Yes" = c("Strongly support", "Somewhat support"),
                                  "No" = c("Somewhat oppose", "Strongly oppose"),
                                  "Not sure" = "Not sure"),
    justified_ice_shooting = fct_recode(justified_ice_shooting,
                                  "Yes" = "Was justified",
                                  "No" = "Was not justified",
                                  "Not sure" = "Not sure"),
    pid3lean = fct_collapse(
      pid7_baseline, 
        "Dem" = c("Strong Democrat", "Not very strong Democrat", "Lean Democrat"),
        "Rep" = c("Lean Republican", "Not very strong Republican", "Strong Republican"),
        "Ind" = "Independent")
  )

# long format for easier analysis (should be in analysis script)
long <- sav |> 
  select(treatment, pid3lean, all_of(lbls$var_name)) |>
  pivot_longer(cols = all_of(lbls$var_name),
               names_to = "question",
               values_to = "response") |> 
  mutate(
    proice = case_when(
      question %in% c("good_obey", "ice_mn_killing_invest", "supp_abolishice") ~ 
        replace_values(response, "Yes" ~ "No", "No" ~ "Yes"),
      TRUE ~ response)
  )

# Percentage pro-ice by treatment and party
long |> 
  filter(complete.cases(pid3lean, proice)) |> 
  count(question, treatment, pid3lean, proice) |> 
  group_by(question, treatment, pid3lean) |> 
  mutate(p = proportions(n)) |>
  filter(proice == "Yes", pid3lean %in% c("Dem", "Rep")) |> 
  ungroup() |> 
  select(question, treatment, pid3lean, p) |> 
  pivot_wider(names_from = treatment, values_from = p) |> 
  mutate(effect = `TRUE` - `FALSE`) |> 
  select(question, pid3lean, effect) |> 
  pivot_wider(names_from = pid3lean, values_from = effect)
  
  
  
  
    

  pivot_longer(cols = all_of(vars), #pivoting longer for easier analysis
               names_to = "question",
               values_to = "response") |> 
  mutate(question = fct_relevel(question, rev(vars))) 

write_rds(evidence, here("data", "evidence-cleaned.rds"))

#randomization/control check
check <- evidence |> 
  mutate(rep = case_when(pid7_baseline %in% c(1, 2, 3) ~ "Dem",
                         pid7_baseline%in% c(5, 6, 7) ~ "Rep"),
         saw= as.numeric(saw_ice_video == 1))

#how well did we randomize for saw_ice_video? Significant diff. in exposure to the video (Treatment 79%, control 73%)?
t.test(check$saw ~ check$ICE_Video)
#are saw_ice_video and party correlated? Significant diff. in exposure to video (Dem 81%, Rep 74%)
t.test(check$saw ~ check$rep)
#how well did we randomize for party? Sliiiightly significant difference (Dem 47% treated, Rep 49% treated). Would not worry about this one.
t.test(check$ICE_Video ~ check$rep)