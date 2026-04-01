library(tidyverse)
library(here)
library(haven)
library(ordinal)
library(broom)
library(gt)

evidence <- here("data", "evidence-cleaned.rds") |> 
  read_rds()

#pid
#the problem here, like the problem with everything else: % agree goes up for every group in the treatment
evidence |> 
  filter(pid7 %in% c(1, 2, 6, 7),
         complete.cases(ICE_Video)) |>
  mutate(pid7 = as_factor(pid7),
         agree = if_else(response == 1, 1, 0),
         ICE_Video = as_factor(ICE_Video)) |> 
  group_by(question, pid7, ICE_Video) |> 
  summarise(agree = mean(agree, na.rm = TRUE)) |>
  ggplot(aes(x = pid7, y = agree, fill = ICE_Video)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~question)

#prior immigration attitudes
evidence |> 
  mutate(ICE_Video = as_factor(ICE_Video),
         fav_fedtroops_mn = as_factor(fav_fedtroops_mn),
         agree = if_else(response == 1, 1, 0)) |>
  filter(complete.cases(fav_fedtroops_mn, ICE_Video),
         response %in% c(1, 2)) |> 
  group_by(question, fav_fedtroops_mn, ICE_Video) |> 
  summarise(agree = mean(agree, na.rm = TRUE)) |>
  ggplot(aes(x = fav_fedtroops_mn, y = agree, fill = ICE_Video)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~question)

#regression of party strength
#strong partisans - nothing significant
results <- evidence |> 
  mutate(agree = ifelse(response == 1, 1, 0)) |> 
  filter(pid7 %in% c(1, 7)) |> 
  mutate(strongrep = if_else(pid7 == 7, 1, 0),
         ICE_Video = as_factor(ICE_Video)) |>
  group_by(question) |>
  group_modify(~{
    model <- glm(agree ~ ICE_Video * strongrep, 
                 data = .x, 
                 family = binomial)
    tidy(model)
  })

#weak partisans - nothing significant
results_weak <- evidence |> 
  mutate(agree = ifelse(response == 1, 1, 0)) |> 
  filter(pid7 %in% c(2, 6)) |> 
  mutate(weakrep = if_else(pid7 == 6, 1, 0),
         ICE_Video = as_factor(ICE_Video)) |>
  group_by(question) |>
  group_modify(~{
    model <- glm(agree ~ ICE_Video * weakrep, 
                 data = .x, 
                 family = binomial)
    tidy(model)
  })

#ordinal logistic regression for support of abolishing ICE
#significant for ICE_Video1:strongrep
abolish <- evidence |> 
  filter(pid7 %in% c(1, 7)) |> 
  mutate(strongrep = if_else(pid7 == 7, 1, 0),
         ICE_Video = as_factor(ICE_Video),
         abolish_ordered = factor(supp_abolishice, 
                                  levels = c(1, 2, 5, 3, 4),
                                  labels = c("Strongly Support", 
                                             "Somewhat Support",
                                             "Not sure",
                                             "Somewhat Oppose", 
                                             "Strongly Oppose"),
                                  ordered = TRUE))

model_abolish <- clm(abolish_ordered ~ ICE_Video * strongrep, 
                     data = abolish)

tidy(model_abolish)

#same thing but for weak partisans
#significant for ICE_Video1:weakrep
abolish_weak <- evidence |> 
  filter(pid7 %in% c(2, 6)) |> 
  mutate(weakrep = if_else(pid7 == 6, 1, 0),
         ICE_Video = as_factor(ICE_Video),
         abolish_ordered = factor(supp_abolishice, 
                                  levels = c(1, 2, 5, 3, 4), 
                                  labels = c("Strongly Support", 
                                             "Somewhat Support",
                                             "Not sure",
                                             "Somewhat Oppose", 
                                             "Strongly Oppose"),
                                  ordered = TRUE))

model_abolish_weak <- clm(abolish_ordered ~ ICE_Video * weakrep, 
                     data = abolish_weak)

tidy(model_abolish_weak)

#combined model - marginally significant, but looks like weak partisans get more polarized than strong ones?
evidence_combined <- evidence |> 
  filter(pid7 %in% c(1, 2, 6, 7)) |>  # strong and weak partisans
  mutate(
    republican = if_else(pid7 %in% c(6, 7), 1, 0),  # Rep vs Dem
    strong = if_else(pid7 %in% c(1, 7), 1, 0),      # Strong vs Weak
    ICE_Video = as_factor(ICE_Video),
    abolish_ordered = factor(supp_abolishice, 
                             levels = c(1, 2, 5, 3, 4),
                             labels = c("Strongly Support", 
                                        "Somewhat Support",
                                        "Not sure",
                                        "Somewhat Oppose", 
                                        "Strongly Oppose"),
                             ordered = TRUE))

model_threeway <- clm(abolish_ordered ~ ICE_Video * republican * strong, 
                      data = evidence_combined) |> 
  tidy()

#table of results
strength_table <- model_threeway |> 
  filter(term %in% c("ICE_Video1", "republican", "strong", "ICE_Video1:republican", "ICE_Video1:strong", "republican:strong", "ICE_Video1:republican:strong")) |>
  dplyr::select(term, estimate, std.error, statistic, p.value) |> 
  gt() |>
  tab_header(
    title = "Differential Treatment Effects by Party & Strength of Partisanship",
    subtitle = "Ordinal Logit on support for abolishing ICE (5-pt scale);
baseline is Control x Weak x Democrat") |>
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
    term = "Term",
    estimate = "Coefficient",
    std.error = "SE",
    statistic = "z-value",
    p.value = "p-value")

gtsave(strength_table, "pid_strength_table.png")

#partisanship model (not differentiating by strength) - significant
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

#prior support for federal troops in mn
evidence |> 
  mutate(ICE_Video = as_factor(ICE_Video),
         fav_fedtroops_mn = as_factor(fav_fedtroops_mn),
         agree = if_else(response == 1, 1, 0)) |>
  filter(complete.cases(fav_fedtroops_mn, ICE_Video),
         response %in% c(1, 2)) |> 
  group_by(question, fav_fedtroops_mn, ICE_Video) |> 
  summarise(agree = mean(agree, na.rm = TRUE)) |>
  ggplot(aes(x = fav_fedtroops_mn, y = agree, fill = ICE_Video)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~question)
