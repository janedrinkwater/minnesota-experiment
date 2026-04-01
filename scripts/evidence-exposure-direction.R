library(tidyverse)
library(here)
library(haven)
library(broom)
library(marginaleffects)

evidence <- here("data", "evidence-cleaned.rds") |> 
  read_rds()

#checking for PID correlation with exposure to video
evidence |> 
  mutate(saw_ice_video = case_match(
    saw_ice_video, 1 ~ "Yes",
    c(2,3) ~ "No"),
    saw_ice_video = factor(saw_ice_video, levels = c("No", "Yes")),
    pid = case_when(pid7_baseline %in% c(1, 2, 3) ~ "Dem",
                    pid7_baseline == 4 ~ "Ind",
                    pid7_baseline %in% c(5, 6, 7) ~ "Rep")) |> 
  glm(saw_ice_video ~ pid, family = "binomial", data = _) |> 
  summary()

#odds ratio - unweighted, no controls
exposure_direction <- evidence |> 
  filter(response != 3) |> #3 is "not sure"; subset to only those with a direction
  mutate(agree = ifelse(response == 1, 1, 0),
         saw_ice_video = case_match(
           saw_ice_video, 1 ~ "Yes",
           c(2,3) ~ "No"),
         saw_ice_video = factor(saw_ice_video, levels = c("No", "Yes"))) |> 
  filter(!is.na(saw_ice_video)) |> 
  group_by(question) |> 
  nest() |> 
  mutate(
    model = map(data, ~glm(agree ~ ICE_Video * saw_ice_video, 
                           data = ., 
                           family = binomial)),
    margins = map(model, ~comparisons(
      ., 
      variables = "ICE_Video",
      by = "saw_ice_video",
      newdata = datagrid(saw_ice_video = c("No", "Yes")),
      transform = "exp") |> 
        tidy())) |>
  unnest(margins) |>
  select(question, video = saw_ice_video, estimate, conf.low, conf.high, p.value)

ggplot(exposure_direction, aes(x = estimate, y = question, color = video)) +
  geom_point(size = 3,
             position = position_dodge(width = 0.6),) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 position = position_dodge(width = 0.6),) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_discrete(labels = c(
    good_threaten = "Renee Good\nthreatened ICE agents",
    good_obey = "R.G. obeyed\nICE orders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "R.G. tried to\nrun over ICE agent",
    good_ice_injured = "ICE agent(s)\nwere injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was\njustified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  scale_color_manual(values = c("Yes" = "#205072", "No" = "#added0"),
                     labels = c("Yes" = "Already saw video", "No" = "Hadn't seen video")) +
  labs(
    title = "Video effect on attitudes by prior video exposure",
    subtitle = "Treatment effect on respondents' odds of agreeing with statement",
    caption = "From Logit interacted by previous exposure to ICE video",
    x = "Odds Ratio",
    y = NULL,
    color = NULL) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, face = "italic", size = 8)) 

#odds ratio (controlling for party ID - why tho)
exposure_direction <- evidence |> 
  filter(response != 3) |> #3 is "not sure"; subset to only those with a direction
  mutate(agree = ifelse(response == 1, 1, 0),
         saw_ice_video = case_match(
           saw_ice_video, 1 ~ "Yes",
           c(2,3) ~ "No"),
         saw_ice_video = factor(saw_ice_video, levels = c("No", "Yes")),
         pid = case_when(pid7_baseline %in% c(1, 2, 3) ~ "Dem",
                         pid7_baseline == 4 ~ "Ind",
                         pid7_baseline %in% c(5, 6, 7) ~ "Rep")) |> 
  filter(!is.na(saw_ice_video)) |> 
  group_by(question) |> 
  nest() |> 
  mutate(
    model = map(data, ~glm(agree ~ ICE_Video * saw_ice_video + pid, 
                           data = ., 
                           family = binomial)),
    margins = map(model, ~comparisons(
      ., 
      variables = "ICE_Video",
      by = "saw_ice_video",
      newdata = datagrid(saw_ice_video = c("No", "Yes")),
      transform = "exp") |> 
        tidy())) |>
  unnest(margins) |>
  select(question, video = saw_ice_video, estimate, conf.low, conf.high, p.value)

ggplot(exposure_direction, aes(x = estimate, y = question, color = video)) +
  geom_point(size = 3,
             position = position_dodge(width = 0.6),) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 position = position_dodge(width = 0.6),) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_discrete(labels = c(good_threaten = "Renee Good\nthreatened ICE agents",
                              good_obey = "R.G. obeyed\nICE orders",
                              good_car_contact = "R.G.'s car contacted\nICE agent",
                              good_attempt_runover = "R.G. tried to\nrun over ICE agent",
                              good_ice_injured = "ICE agent(s)\nwere injured",
                              good_ice_professional = "ICE agents behaved\nprofessionally",
                              justified_ice_shooting = "ICE shooting was\njustified",
                              ice_mn_killing_invest = "ICE agent should\nbe investigated",
                              ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
                              collapsed_abolishice = "Support abolishing ICE")) +
  scale_color_manual(values = c("Yes" = "#205072", "No" = "#added0"),
                     labels = c("Yes" = "Already saw video", "No" = "Hadn't seen video")) +
  labs(
    title = "Video effect on attitudes by prior video exposure",
    subtitle = "Treatment effect on respondents' odds of agreeing with statement",
    caption = "From Logit interacted by previous exposure to ICE video, controlling for partisanship",
    x = "Odds Ratio",
    y = NULL,
    color = NULL) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, face = "italic", size = 8)) 

#WEIGHTED odds ratio (controlling for party ID)
exposure_direction <- evidence |> 
  filter(response != 3) |> #3 is "not sure"; subset to only those with a direction
  mutate(agree = ifelse(response == 1, 1, 0),
         saw_ice_video = case_match(
           saw_ice_video, 1 ~ "Yes",
           c(2,3) ~ "No"),
         saw_ice_video = factor(saw_ice_video, levels = c("No", "Yes"))) |> 
  filter(!is.na(saw_ice_video)) |> 
  group_by(question) |> 
  nest() |> 
  mutate(
    model = map(data, ~glm(agree ~ ICE_Video * saw_ice_video + pid3_baseline, 
                           data = ., 
                           family = binomial,
                           weights = weight)),
    margins = map(model, ~comparisons(
      ., 
      variables = "ICE_Video",
      by = "saw_ice_video",
      newdata = datagrid(saw_ice_video = c("No", "Yes")),
      transform = "exp") |> 
        tidy())) |>
  unnest(margins) |>
  select(question, video = saw_ice_video, estimate, conf.low, conf.high, p.value)

ggplot(exposure_direction, aes(x = estimate, y = question, color = video)) +
  geom_point(size = 3,
             position = position_dodge(width = 0.6),) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 position = position_dodge(width = 0.6),) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_discrete(labels = c(
    good_threaten = "R.G. threatened 
ICE agents",
    good_obey = "R.G. obeyed 
ICE orders",
    good_car_contact = "R.G.'s car contacted 
ICE agent",
    good_attempt_runover = "Renee Good tried to 
run over ICE agent",
    good_ice_injured = "ICE agent(s) 
were injured",
    good_ice_professional = "ICE agents behaved 
professionally",
    justified_ice_shooting = "ICE shooting was 
justified",
    ice_mn_killing_invest = "ICE agent should
be investigated",
    ice_mn_good_widow = "R.G.'s widow should 
be investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  scale_color_manual(values = c("Yes" = "#205072", "No" = "#added0"),
                     labels = c("Yes" = "Already saw video", "No" = "Hadn't seen video")) +
  labs(
    title = "Effect on attitudes, by prior exposure",
    subtitle = "Weighted logit interacted by exposure, among\nrespondents with opinions",
    x = "Odds Ratio",
    y = NULL,
    color = NULL) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top")
