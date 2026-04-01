library(tidyverse)
library(here)
library(haven)
library(broom)
library(marginaleffects)

evidence <- here("data", "evidence-cleaned.rds") |> 
  read_rds()

#UNWEIGHTED marginal effects (LLM w interaction)
exposure_certainty <- evidence |> 
  mutate(not_sure = as.numeric(response == 3),
         saw_ice_video = case_match( #renaming
           saw_ice_video, 1 ~ "Yes",
           c(2,3) ~ "No"),
         saw_ice_video = factor(saw_ice_video, levels = c("No", "Yes"))) |> 
  filter(complete.cases(saw_ice_video, not_sure)) |> 
  group_by(question) |>
  nest() |>
  mutate(
    model = map(data, ~lm(not_sure ~ ICE_Video * saw_ice_video, 
                          data = .)),
    margins = map(model, ~comparisons(
      ., 
      variables = "ICE_Video",
      by = "saw_ice_video",
      newdata = datagrid(saw_ice_video = c("No", "Yes"))) |> #smaller dataset
        tidy())) |>
  unnest(margins) |>
  select(question, saw_ice_video, estimate, std.error, 
         conf.low, conf.high, p.value) |> 
  mutate(decrease = -estimate,
         conf.low = -conf.low,
         conf.high = -conf.high)

ggplot(exposure_certainty, aes(x = decrease, y = reorder(question, decrease), color = saw_ice_video)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6),
    height = 0.2,
    linewidth = 0.6) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 2.5) +
  scale_y_discrete(labels = c(
    good_threaten = "R.G. threatened ICE agent",
    good_obey = "R.G. obeyed ICE orders",
    good_car_contact = "R.G.'s car contacted
ICE agent",
    good_attempt_runover = "Renee Good tried to
run over ICE agent",
    good_ice_injured = "ICE agent was injured",
    good_ice_professional = "ICE agents behaved
professionally",
    justified_ice_shooting = "ICE shooting was justified",
    ice_mn_killing_invest = "ICE agent should
be investigated",
    ice_mn_good_widow = "R.G.'s widow should
be investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("Yes" = "#205072", "No" = "#added0"),
                     labels = c("Yes" = "Already saw video", "No" = "Hadn't seen video")) +
  labs(
    title = "Video effect on certainty, by prior exposure",
    subtitle = "% by which treatment reduces uncertainty, by whether person already saw video",
    caption = "Marginal effects from OLS interacted by previous exposure to ICE video",
    x = "",
    y = NULL,
    color = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, face = "italic", size = 8))

#weighted marginal effects (LLM w interaction)
exposure_certainty <- evidence |> 
  mutate(not_sure = as.numeric(response == 3),
    saw_ice_video = case_match( #renaming
      saw_ice_video, 1 ~ "Yes",
      c(2,3) ~ "No"),
    saw_ice_video = factor(saw_ice_video, levels = c("No", "Yes"))) |> 
  filter(complete.cases(saw_ice_video, not_sure)) |> 
  group_by(question) |>
  nest() |>
  mutate(
    model = map(data, ~lm(not_sure ~ ICE_Video * saw_ice_video + pid3_baseline, 
                          data = .,
                          weights = weight)),
    margins = map(model, ~comparisons(
      ., 
      variables = "ICE_Video",
      by = "saw_ice_video",
      newdata = datagrid(saw_ice_video = c("No", "Yes"))) |> #smaller dataset
  tidy())) |>
  unnest(margins) |>
  select(question, saw_ice_video, estimate, std.error, 
         conf.low, conf.high, p.value) |> 
  mutate(decrease = -estimate,
         conf.low = -conf.low,
         conf.high = -conf.high)

ggplot(exposure_certainty, aes(x = decrease, y = reorder(question, decrease), color = saw_ice_video)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6),
    height = 0.2,
    linewidth = 0.6) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 2.5) +
  scale_y_discrete(labels = c(
    good_threaten = "R.G. threatened ICE agent",
    good_obey = "R.G. obeyed ICE orders",
    good_car_contact = "R.G.'s car contacted
ICE agent",
    good_attempt_runover = "Renee Good tried to
run over ICE agent",
    good_ice_injured = "ICE agent was injured",
    good_ice_professional = "ICE agents behaved
professionally",
    justified_ice_shooting = "ICE shooting was justified",
    ice_mn_killing_invest = "ICE agent should
be investigated",
    ice_mn_good_widow = "R.G.'s widow should
be investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("Yes" = "#205072", "No" = "#added0"),
                     labels = c("Yes" = "Already saw video", "No" = "Hadn't seen video")) +
  labs(
    title = "Effect on certainty, by prior exposure",
    subtitle = "% by which treatment reduces uncertainty, by \nwhether participant already saw ICE video",
    caption = "Weighted OLS interacted by previous exposure to video; controls for party",
    x = "",
    y = NULL,
    color = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, face = "italic", size = 8))





#Update, 3/30: what is this? come back and see
#relative odds
exposure_certainty_or <- evidence |> 
  mutate(not_sure = as.numeric(response == 3),
         saw_ice_video = case_match(
           saw_ice_video, 1 ~ "Yes",
           c(2,3) ~ "No"),
         saw_ice_video = factor(saw_ice_video, levels = c("No", "Yes"))) |> 
  filter(!is.na(saw_ice_video), !is.na(not_sure)) |> 
  group_by(question) |>
  nest() |>
  mutate(
    model = map(data, ~glm(not_sure ~ ICE_Video * saw_ice_video, 
                           data = ., 
                           family = binomial(link = "logit"))),
    margins = map(model, ~comparisons(
      ., 
      variables = "ICE_Video",
      by = "saw_ice_video",
      newdata = datagrid(saw_ice_video = c("No", "Yes")),
      transform = "exp") |>  # get odds ratios
        tidy())) |>
  unnest(margins) |>
  select(question, video = saw_ice_video, estimate,
         conf.low, conf.high, p.value)

ggplot(exposure_certainty_or, aes(x = estimate, y = reorder(question, -estimate), color = video)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6),
    height = 0.2,
    linewidth = 0.6) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 2.5) +
  scale_y_discrete(labels = c(
    good_threaten = "R.G. threatened ICE agent",
    good_obey = "R.G. obeyed ICE orders",
    good_car_contact = "R.G.'s car contacted
ICE agent",
    good_attempt_runover = "Renee Good tried to
run over ICE agent",
    good_ice_injured = "ICE agent was injured",
    good_ice_professional = "ICE agents behaved
professionally",
    justified_ice_shooting = "ICE shooting was justified",
    ice_mn_killing_invest = "ICE agent should
be investigated",
    ice_mn_good_widow = "R.G.'s widow should
be investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  scale_color_manual(values = c("Yes" = "#205072", "No" = "#added0"),
                     labels = c("Yes" = "Saw video", "No" = "Didn't see video")) +
  labs(
    title = "Prior exposure to ICE video changes video effect on attitude certainty",
    subtitle = "Treatment effects on whether respondents said 'Not sure', interacted by
previous exposure to ICE video",
    x = "Odds Ratio",
    y = NULL,
    color = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top")
