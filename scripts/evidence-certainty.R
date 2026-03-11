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
    title = "Watching Renee Good video decreased uncertainty of ICE attitudes",
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
