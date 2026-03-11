library(tidyverse)
library(here)
library(haven)
library(broom)
library(marginaleffects)

evidence <- here("data", "evidence-cleaned.rds") |> 
  read_rds()

pid_direction <- evidence |> 
  filter(response != 3) |> #3 is "not sure"; subset to only those with a direction
  mutate(agree = ifelse(response == 1, 1, 0),
         pid7_baseline = case_match(pid7_baseline, 
                                     c(1, 2, 3) ~ "Dem",
                                     c(5, 6, 7) ~ "Rep",
                                     4 ~ "Ind",
                                     8 ~ NA),
         pid = factor(pid7_baseline, levels = c("Dem", "Rep", "Ind"))) |> 
  filter(!is.na(pid)) |> 
  group_by(question) |> 
  nest() |> 
  mutate(
    model = map(data, ~glm(agree ~ ICE_Video * pid + saw_ice_video, 
                           data = ., 
                           family = binomial)),
    margins = map(model, ~comparisons(
      ., 
      variables = "ICE_Video",
      by = "pid",
      newdata = datagrid(pid = c("Dem", "Rep", "Ind")),
      transform = "exp") |> 
        tidy())) |>
  unnest(margins) |>
  select(question, pid, estimate, conf.low, conf.high, p.value)

ggplot(pid_direction, aes(x = estimate, y = question, color = pid)) +
  geom_point(size = 3,
             position = position_dodge(width = 0.6),) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 position = position_dodge(width = 0.6),) +
  geom_vline(xintercept = 1, linetype = "dashed") +
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
  scale_color_manual(values = c("Dem" = "#619CFF", "Rep" = "#F8766D", "Ind" = "#825187")) +
  labs(
    title = "Effect of ICE video on attitudes varies slightly by party",
    subtitle = "Treatment effect on respondents' odds of agreeing (conditional on having an opinion),
by partisanship",
    x = "Odds Ratio",
    y = NULL,
    color = NULL) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top")
