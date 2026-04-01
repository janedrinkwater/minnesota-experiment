library(tidyverse)
library(here)
library(haven)
library(ordinal)
library(broom)
library(gt)

evidence <- here("data", "evidence-cleaned.rds") |> 
  read_rds()

#ordinal partisanship interaction for full-spectrum (5 pt) abolish ICE attitude
#significant
abolish_pid <- evidence |> 
  filter(pid7 %in% c(1, 2, 3, 5, 6, 7)) |> 
  mutate(rep = if_else(pid7 %in% c(5, 6, 7), 1, 0),
         ICE_Video = as_factor(ICE_Video),
         abolish_ordered = factor(supp_abolishice, 
                                  levels = c(1, 2, 5, 3, 4), 
                                  labels = c("Strongly Support", 
                                             "Somewhat Support",
                                             "Not sure",
                                             "Somewhat Oppose", 
                                             "Strongly Oppose"),
                                  ordered = TRUE))

model_abolish_pid <- clm(abolish_ordered ~ ICE_Video * rep, 
                          data = abolish_pid)

tidy(model_abolish_pid)

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

clm(response_ordered ~ ICE_Video + rep + I(rep * ICE_Video),
    data = filter(results_ordinal, question == "good_attempt_runover")) |> 
  summary()

clm(response_ordered ~ ICE_Video:rep,
    data = filter(results_ordinal, question == "good_attempt_runover")) |> 
  summary()

clm(response_ordered ~ rep + I(ICE_Video * rep) + I(ICE_Video * (1 - rep)),
    data = filter(results_ordinal, question == "good_attempt_runover")) |> 
  summary()
  
  group_by(question) |>
#within each question, run the same model (use summarize)
  summarise(model = clm(response_ordered ~ ICE_Video * rep, 
                 data = cur_data())) |>
  group_modify(~{
    model <- clm(response_ordered ~ ICE_Video * rep, 
                 data = .x)
    tidy(model)
  })

#table of results
question_labels <- c(
  "good_car_contact" = "Renee Good's car contacted ICE agent",
  "good_ice_injured" = "ICE agent(s) were injured",
  "good_threaten" = "R.G. threatened ICE agents",
  "good_obey" = "R.G. obeyed ICE orders",
  "good_attempt_runover" = "R.G. tried to run over ICE agent",
  "good_ice_professional" = "ICE agents behaved professionally",
  "justified_ice_shooting" = "ICE shooting was justified",
  "ice_mn_killing_invest" = "ICE agent should be investigated",
  "ice_mn_good_widow" = "R.G.'s widow should be investigated",
  "collapsed_abolishice" = "Support abolishing ICE")

table <- results_ordinal |> 
  filter(term == "ICE_Video1:rep") |>
  ungroup() |> 
  mutate(
    question = factor(question, levels = names(question_labels)),
    question_label = question_labels[as.character(question)]) |>
  arrange(question) |> 
  dplyr::select(question_label, estimate, std.error, statistic, p.value) |> 
  gt() |>
  tab_header(
    title = "Differential Treatment Effects by Party",
    subtitle = "Ordinal Logit; baseline is Control × Democrat") |>
  fmt_number(
    columns = c(estimate, std.error, statistic),
    decimals = 3) |>
  fmt_number(
    columns = p.value,
    decimals = 4) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = p.value,
      rows = p.value < 0.05)) |>
  cols_label(
    question_label = "Outcome",
    estimate = "Coefficient",
    std.error = "SE",
    statistic = "z-value",
    p.value = "p-value")

gtsave(table, "pid_ordinal_table.png")

#graph of results
results_ordinal <- evidence |> 
  filter(pid7 %in% c(1, 2, 3, 5, 6, 7)) |> 
  mutate(
    rep = if_else(pid7 %in% c(5, 6, 7), 1, 0),
    ICE_Video = as_factor(ICE_Video),
    # Create ordered factor: Agree -> Not Sure -> Disagree
    response_ordered = factor(response, 
                              levels = c(2, 3, 1),
                              labels = c("Disgree", "Not Sure", "Agree"),
                              ordered = TRUE)) |>
  group_by(question) |>
  group_modify(~{
    model <- clm(response_ordered ~ ICE_Video * rep, 
                 data = .x)
    tidy(model, conf.int = TRUE, conf.level = 0.95)
  }) |> 
  filter(term == "ICE_Video1:rep")

ggplot(results_ordinal, aes(x = estimate, y = reorder(question, estimate))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_discrete(labels = c(
    good_threaten = "R.G. threatened\nICE agents",
    good_obey = "R.G. obeyed ICE orders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "Renee Good tried to\nrun over ICE agent",
    good_ice_injured = "ICE agent(s)\nwere injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was\njustified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  labs(
    title = "Directional treatment effect by party",
    subtitle = "Difference in video effect on Republicans, compared to Democrats",
    caption = "Ordinal logistic regression with 95% confidence intervals",
    x = NULL,
    y = NULL) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    plot.caption = element_text(hjust = 0, face = "italic", size = 8))