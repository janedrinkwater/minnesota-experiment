library(tidyverse)
library(here)
library(ordinal)
library(tidymodels)

evidence <- here("data", "evidence-cleaned.rds") |> 
  read_rds() |> 
  mutate(dem = if_else(pid7 %in% c(1, 2, 3), 1, 0),
         rep = if_else(pid7 %in% c(5, 6, 7), 1, 0),
         ice_dem = ICE_Video * dem,
         ice_rep = ICE_Video * rep,
         response = haven::as_factor(response)) |> 
  filter(pid7%in%c(1, 2, 3, 5, 6, 7)) |> 
  group_by(question)

questions = group_keys(evidence)$question

results <- group_split(evidence) |> 
  set_names(questions) |> 
  map(\(df) {
    clm(response ~ rep + ice_dem + ice_rep, data = df) |> 
      tidy()
}) |> 
  bind_rows(.id = "question") |> 
  ungroup()

results |> 
  filter(term %in% c("ice_dem", "ice_rep")) |> 
  dplyr::select(question, term, estimate, std.error) |> 
  pivot_wider(id_cols = question, names_from = term, values_from = c(estimate, std.error)) |> 
  dplyr::select(question, estimate_ice_dem, std.error_ice_dem, estimate_ice_rep, std.error_ice_rep)

evidence <- evidence |>  
  set_names(group_keys(evidence)$question) |> 
  map(\(df) {
    clm(response ~ ICE_Video:dem + ICE_Video:rep, data = df) |> 
      tidy()
}) |> 
  bind_rows(.id = "question")

#ordinal for all questions, by party
results_ordinal <- evidence |> 
  filter(pid7 %in% c(1, 2, 3, 5, 6, 7)) |> 
  mutate(
    rep = if_else(pid7 %in% c(5, 6, 7), 1, 0),
    # Create ordered factor: Disagree -> Not Sure -> Agree
    response_ordered = factor(response, 
                              levels = c(2, 3, 1),
                              labels = c("Disgree", "Not Sure", "Agree"),
                              ordered = TRUE)) 
