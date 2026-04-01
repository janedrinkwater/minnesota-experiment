library(tidyverse)
library(here)
library(haven)
library(broom)

evidence <- here("data", "evidence-cleaned.rds") |> 
  read_rds()

#odds ratio with no weights, no controls
dir_evidence <- evidence |> 
  filter(response != 3) |> #3 is "not sure"; subset to only those with a direction
  mutate(agree = as.numeric(response == 1)) |>  #turning yes (1) to 1 and no (2) to 0)
  group_by(question) |> 
  nest() |> 
  mutate(model = map(data, ~glm(agree ~ ICE_Video,
                                data = ., 
                                family = binomial)),
         tidy_model = map(model, tidy, conf.int = TRUE, exponentiate = TRUE)) |> 
  unnest(tidy_model) |> 
  filter(term == "ICE_Video") |>
  select(question, estimate, conf.low, conf.high, p.value) |> 
  mutate(question_type = if_else(question %in% c("good_threaten", 
                                                 "good_obey", 
                                                 "good_car_contact", 
                                                 "good_attempt_runover", 
                                                 "good_ice_injured", 
                                                 "good_ice_professional"), 
                                 "Factual", "Policy"))

ggplot(dir_evidence, aes(x = estimate, y = reorder(question, -estimate), color = question_type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 width = 0.2,
                 height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_discrete(labels = c(
    good_threaten = "R.G. threatened ICE\nagents",
    good_obey = "Renee Good obeyed\nICE orders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "R.G. tried to run over\nICE agent",
    good_ice_injured = "ICE agent(s)\nwere injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was\njustified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  scale_x_continuous(breaks = c(0.4, 0.6, 0.8, 1, 1.2, 1.4)) +
  scale_color_manual(values = c("Factual" = "#added0", "Policy" = "#c4a958")) +
  labs(
    title = "Video effect on direction of ICE attitudes",
    subtitle = "Treatment effect on odds of agreeing with statements (conditional on having opinion)",
    y = NULL,
    x = "Odds Ratio",
    color = "Question type") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top")

#odds ratio without weights, controlling for saw_ice_video
dir_evidence <- evidence |> 
  filter(response != 3) |> #3 is "not sure"; subset to only those with a direction
  mutate(agree = as.numeric(response == 1), #turning yes (1) to 1 and no (2) to 0
         saw_ice_video = as.numeric(saw_ice_video == 1)) |> 
  group_by(question) |> 
  nest() |> 
  mutate(model = map(data, ~glm(agree ~ ICE_Video + saw_ice_video, 
                                data = ., 
                                family = binomial)),
         tidy_model = map(model, tidy, conf.int = TRUE, exponentiate = TRUE)) |> 
  unnest(tidy_model) |> 
  filter(term == "ICE_Video") |>
  select(question, estimate, conf.low, conf.high, p.value) |> 
  mutate(question_type = if_else(question %in% c("good_threaten", 
                                                 "good_obey", 
                                                 "good_car_contact", 
                                                 "good_attempt_runover", 
                                                 "good_ice_injured", 
                                                 "good_ice_professional"), 
                                 "Factual", "Policy"))

ggplot(dir_evidence, aes(x = estimate, y = reorder(question, -estimate), color = question_type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 width = 0.2,
                 height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_discrete(labels = c(
    good_threaten = "R.G. threatened ICE\nagents",
    good_obey = "Renee Good obeyed\nICE orders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "R.G. tried to run over\nICE agent",
    good_ice_injured = "ICE agent(s)\nwere injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was\njustified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  scale_x_continuous(breaks = c(0.4, 0.6, 0.8, 1, 1.2, 1.4)) +
  scale_color_manual(values = c("Factual" = "#added0", "Policy" = "#c4a958")) +
  labs(
    title = "Video effect on direction of ICE attitudes",
    subtitle = "Treatment effect on odds of agreeing with statements (conditional on having opinion)",
    y = NULL,
    x = "Odds Ratio",
    color = "Question type") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top")

#weighted odds ratio
dir_evidence <- evidence |> 
  filter(response != 3) |> #3 is "not sure"; subset to only those with a direction
  mutate(agree = as.numeric(response == 1), #turning yes (1) to 1 and no (2) to 0
         saw_ice_video = as.numeric(saw_ice_video == 1)) |> 
  group_by(question) |> 
  nest() |> 
  mutate(model = map(data, ~glm(agree ~ ICE_Video + saw_ice_video, 
                                data = ., 
                                family = binomial,
                                weights = weight)),
         tidy_model = map(model, tidy, conf.int = TRUE, exponentiate = TRUE)) |> 
  unnest(tidy_model) |> 
  filter(term == "ICE_Video") |>
  select(question, estimate, conf.low, conf.high, p.value) |> 
  mutate(question_type = if_else(question %in% c("good_threaten", 
                                                 "good_obey", 
                                                 "good_car_contact", 
                                                 "good_attempt_runover", 
                                                 "good_ice_injured", 
                                                 "good_ice_professional"), 
                                 "Factual", "Policy"))

ggplot(dir_evidence, aes(x = estimate, y = reorder(question, -estimate), color = question_type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 width = 0.2,
                 height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_discrete(labels = c(
    good_threaten = "R.G. threatened ICE\nagents",
    good_obey = "Renee Good obeyed\nICE orders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "R.G. tried to run over\nICE agent",
    good_ice_injured = "ICE agent(s)\nwere injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was\njustified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  scale_x_continuous(breaks = c(0.4, 0.6, 0.8, 1, 1.2, 1.4)) +
  scale_color_manual(values = c("Factual" = "#added0", "Policy" = "#c4a958")) +
  labs(
    title = "Video effect on direction of ICE attitudes",
    subtitle = "Odds ratios from weighted logistic regression,\namong respondents with opinions",
    caption = "Model controls for prior video exposure",
    y = NULL,
    x = "Odds Ratio",
    color = "Question type") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, face = "italic", size = 8))