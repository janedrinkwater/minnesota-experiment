library(tidyverse)
library(here)
library(broom)

evidence <- here("data", "evidence-cleaned.rds") |> 
  read_rds()

#difference in means (t test)
certainty <- evidence |>
  mutate(not_sure = as.numeric(response == 3)) |> #binary sure/not sure
  group_by(question) |>
  summarize( #creating a table of t-tests and means
    test = list(t.test(not_sure ~ ICE_Video)),
    control_mean = mean(not_sure[ICE_Video == 0], na.rm = TRUE),
    treatment_mean = mean(not_sure[ICE_Video == 1], na.rm = TRUE)) |>
  mutate(tidy_test = map(test, tidy)) |>
  unnest(tidy_test) |>
  mutate( #calculating difference in means
    difference = (estimate1 - estimate2)) |> # control - treatment
  select(question, control_mean, treatment_mean, difference, 
         conf.low, conf.high, p.value) |> 
  mutate(question_type = if_else(question %in% c("good_threaten", 
                                                 "good_obey", 
                                                 "good_car_contact", 
                                                 "good_attempt_runover", 
                                                 "good_ice_injured", 
                                                 "good_ice_professional"), "Factual", "Policy"))

ggplot(certainty, aes(x = difference, y = reorder(question, difference), color = question_type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 width = 0.2, 
                 height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_y_discrete(labels = c(
    good_threaten = "Renee Good threatened\nICE agents",
    good_obey = "R.G. obeyed ICE orders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "R.G. tried to run over\nICE agent",
    good_ice_injured = "ICE agent(s)\nwere injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was\njustified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  scale_color_manual(values = c("Factual" = "#added0", "Policy" = "#c4a958")) +
  labs(
    title = "Video effect on attitude certainty",
    subtitle = "Decrease in % responding 'Not Sure' (Control - Treatment)",
    x = NULL,
    y = NULL,
    color = "Question type") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top") 

#LM without weights; not controlling for saw_ice_video
certainty <- evidence |>
  mutate(not_sure = as.numeric(response == 3)) |> 
  group_by(question) |>
  summarize(
    model = list(lm(not_sure ~ ICE_Video))) |>
  mutate(tidy_model = map(model, ~tidy(., conf.int = TRUE))) |>
  unnest(tidy_model) |>
  filter(term == "ICE_Video") |> # Keep only the treatment effect
  select(question, difference = estimate, conf.low, conf.high, p.value) |>
  mutate(
    # Flip sign so positive = control higher (video reduced uncertainty)
    difference = -difference,
    temp_low = -conf.high,
    temp_high = -conf.low,
    conf.low = temp_low,
    conf.high = temp_high
  ) |>
  select(-temp_low, -temp_high) |>
  mutate(question_type = if_else(question %in% c("good_threaten", 
                                                 "good_obey", 
                                                 "good_car_contact", 
                                                 "good_attempt_runover", 
                                                 "good_ice_injured", 
                                                 "good_ice_professional"), 
                                 "Factual", "Policy"))

ggplot(certainty, aes(x = difference, y = reorder(question, difference), color = question_type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 width = 0.2, 
                 height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_y_discrete(labels = c(
    good_threaten = "Renee Good threatened\nICE agents",
    good_obey = "R.G. obeyed ICE orders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "R.G. tried to run over\nICE agent",
    good_ice_injured = "ICE agent(s)\nwere injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was\njustified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  scale_color_manual(values = c("Factual" = "#added0", "Policy" = "#c4a958")) +
  labs(
    title = "Video effect on attitude certainty",
    subtitle = "Decrease in % responding 'Not Sure'",
    caption = "Treatment effect estimated from OLS",
    x = NULL,
    y = NULL,
    color = "Question type") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, face = "italic", size = 8))

#LM without weights; controlling for saw_ice_video
#I don't think I will use this one, since saw_ice_video INTERACTS w treatment
certainty <- evidence |>
  mutate(not_sure = as.numeric(response == 3), #binary sure/not sure
         saw_ice_video = as.numeric(saw_ice_video == 1)) |> #binary saw/did not see
  group_by(question) |>
  summarize(
    model = list(lm(not_sure ~ ICE_Video + saw_ice_video))) |>
  mutate(tidy_model = map(model, ~tidy(., conf.int = TRUE))) |>
  unnest(tidy_model) |>
  filter(term == "ICE_Video") |> # Keep only the treatment effect
  select(question, difference = estimate, conf.low, conf.high, p.value) |>
  mutate(
    # Flip sign so positive = control higher (video reduced uncertainty)
    difference = -difference,
    temp_low = -conf.high,
    temp_high = -conf.low,
    conf.low = temp_low,
    conf.high = temp_high
  ) |>
  select(-temp_low, -temp_high) |>
  mutate(question_type = if_else(question %in% c("good_threaten", 
                                                 "good_obey", 
                                                 "good_car_contact", 
                                                 "good_attempt_runover", 
                                                 "good_ice_injured", 
                                                 "good_ice_professional"), 
                                 "Factual", "Policy"))

ggplot(certainty, aes(x = difference, y = reorder(question, difference), color = question_type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 width = 0.2, 
                 height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_y_discrete(labels = c(
    good_threaten = "Renee Good threatened\nICE agents",
    good_obey = "R.G. obeyed ICE orders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "R.G. tried to run over\nICE agent",
    good_ice_injured = "ICE agent(s)\nwere injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was\njustified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  scale_color_manual(values = c("Factual" = "#added0", "Policy" = "#c4a958")) +
  labs(
    title = "Video effect on attitude certainty",
    subtitle = "Decrease in % responding 'Not Sure'",
    caption = "OLS controlling for prior video exposure",
    x = NULL,
    y = NULL,
    color = "Question type") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, face = "italic", size = 8))

# LM with weights and controlling for saw_ice_video
certainty <- evidence |>
  mutate(not_sure = as.numeric(response == 3), #binary sure/not sure
         saw_ice_video = as.numeric(saw_ice_video == 1)) |> #binary saw/did not see
  group_by(question) |>
  summarize(
    # Weighted linear regression
    model = list(lm(not_sure ~ ICE_Video + saw_ice_video, weights = weight)),
    # Weighted means for reporting
    control_mean = weighted.mean(not_sure[ICE_Video == 0], 
                                 w = weight[ICE_Video == 0], 
                                 na.rm = TRUE),
    treatment_mean = weighted.mean(not_sure[ICE_Video == 1], 
                                   w = weight[ICE_Video == 1], 
                                   na.rm = TRUE)
  ) |>
  mutate(tidy_model = map(model, ~tidy(., conf.int = TRUE))) |>
  unnest(tidy_model) |>
  filter(term == "ICE_Video") |> # Keep only the treatment effect
  select(question, control_mean, treatment_mean, 
         difference = estimate,  # This is your treatment effect
         conf.low, conf.high, p.value) |>
  mutate(
    # Flip sign so positive = control higher (video reduced uncertainty)
    difference = -difference,
    temp_low = -conf.high,
    temp_high = -conf.low,
    conf.low = temp_low,
    conf.high = temp_high
  ) |>
  select(-temp_low, -temp_high) |>
  mutate(question_type = if_else(question %in% c("good_threaten", 
                                                 "good_obey", 
                                                 "good_car_contact", 
                                                 "good_attempt_runover", 
                                                 "good_ice_injured", 
                                                 "good_ice_professional"), 
                                 "Factual", "Policy"))

ggplot(certainty, aes(x = difference, y = reorder(question, difference), color = question_type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 width = 0.2, 
                 height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_y_discrete(labels = c(
    good_threaten = "Renee Good threatened\nICE agents",
    good_obey = "R.G. obeyed ICE orders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "R.G. tried to run over\nICE agent",
    good_ice_injured = "ICE agent(s)\nwere injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was\njustified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  scale_color_manual(values = c("Factual" = "#added0", "Policy" = "#c4a958")) +
  labs(
    title = "Video effect on attitude certainty",
    subtitle = "Decrease in % responding 'Not Sure'",
    caption = "OLS controlling for prior video exposure & using survey weights",
    x = NULL,
    y = NULL,
    color = "Question type") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, face = "italic", size = 8))