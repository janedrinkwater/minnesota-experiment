library(tidyverse)
library(here)
library(haven)
library(broom)
library(marginaleffects)

evidence <- here("data", "evidence-cleaned.rds") |> 
  read_rds()

#UNWEIGHTED marginal effects (OLS w interaction), no controls
pid_certainty <- evidence |> 
  mutate(not_sure = as.numeric(response == 3),
         pid7_baseline = case_match(pid7_baseline, 
                                    c(1, 2, 3) ~ "Dem",
                                    c(5, 6, 7) ~ "Rep",
                                    4 ~ "Ind",
                                    8 ~ NA),
         pid = factor(pid7_baseline, levels = c("Dem", "Rep", "Ind"))) |> 
  filter(complete.cases(pid, not_sure)) |> 
  group_by(question) |>
  nest() |>
  mutate(
    model = map(data, ~lm(not_sure ~ ICE_Video * pid, 
                          data = .)),
    margins = map(model, ~comparisons(
      ., 
      variables = "ICE_Video",
      by = "pid",
      newdata = datagrid(pid = c("Dem", "Rep", "Ind"))) |> #smaller dataset
        tidy())) |>
  unnest(margins) |>
  select(question, pid, estimate, std.error, 
         conf.low, conf.high, p.value) |> 
  mutate(decrease = -estimate,
         conf.low = -conf.low,
         conf.high = -conf.high)

ggplot(pid_certainty, aes(x = decrease, y = reorder(question, decrease), color = pid)) +
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
    good_threaten = "Renee Good threatened\nICE agents",
    good_obey = "R.G. obeyed ICE\norders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "R.G. tried to run over\nICE agent",
    good_ice_injured = "ICE agent(s)\nwere injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was\njustified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  scale_color_manual(values = c("Dem" = "#619CFF", "Rep" = "#F8766D", "Ind" = "#825187")) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(
    title = "Video effect on certainty, by party",
    subtitle = "% by which treatment reduces uncertainty, by participant's party ID",
    caption = "Marginal effects from an OLS interacted by party",
    x = "",
    y = NULL,
    color = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, face = "italic", size = 8))

#UNWEIGHTED marginal effects (OLS w interaction), control for prior exposure
pid_certainty <- evidence |> 
  mutate(not_sure = as.numeric(response == 3),
         pid7_baseline = case_match(pid7_baseline, 
                                     c(1, 2, 3) ~ "Dem",
                                     c(5, 6, 7) ~ "Rep",
                                     4 ~ "Ind",
                                     8 ~ NA),
         pid = factor(pid7_baseline, levels = c("Dem", "Rep", "Ind")),
         saw_ice_video = as.numeric(saw_ice_video == 1)) |> 
  filter(complete.cases(pid, not_sure)) |> 
  group_by(question) |>
  nest() |>
  mutate(
    model = map(data, ~lm(not_sure ~ ICE_Video * pid + saw_ice_video, 
                          data = .)),
    margins = map(model, ~comparisons(
      ., 
      variables = "ICE_Video",
      by = "pid",
      newdata = datagrid(pid = c("Dem", "Rep", "Ind"))) |> #smaller dataset
        tidy())) |>
  unnest(margins) |>
  select(question, pid, estimate, std.error, 
         conf.low, conf.high, p.value) |> 
  mutate(decrease = -estimate,
         conf.low = -conf.low,
         conf.high = -conf.high)

ggplot(pid_certainty, aes(x = decrease, y = reorder(question, decrease), color = pid)) +
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
    good_threaten = "Renee Good threatened\nICE agents",
    good_obey = "R.G. obeyed ICE\norders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "R.G. tried to run over\nICE agent",
    good_ice_injured = "ICE agent(s)\nwere injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was\njustified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  scale_color_manual(values = c("Dem" = "#619CFF", "Rep" = "#F8766D", "Ind" = "#825187")) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(
    title = "Video effect on certainty, by party",
    subtitle = "% by which treatment reduces uncertainty, by participant's party ID",
    caption = "Marginal effects from an OLS interacted by party, controlling for prior exposure",
    x = "",
    y = NULL,
    color = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, face = "italic", size = 8))

#WEIGHTED: marginal effects (OLS w interaction)
pid_certainty <- evidence |> 
  mutate(not_sure = as.numeric(response == 3),
         pid7_baseline = case_match(pid7_baseline, 
                                    c(1, 2, 3) ~ "Dem",
                                    c(5, 6, 7) ~ "Rep",
                                    4 ~ "Ind",
                                    8 ~ NA),
         pid = factor(pid7_baseline, levels = c("Dem", "Rep", "Ind")),
         saw_ice_video = as.numeric(saw_ice_video == 1)) |> 
  filter(complete.cases(pid, not_sure)) |> 
  group_by(question) |>
  nest() |>
  mutate(
    model = map(data, ~lm(not_sure ~ ICE_Video * pid + saw_ice_video, 
                          data = .,
                          weights = weight)),
    margins = map(model, ~comparisons(
      ., 
      variables = "ICE_Video",
      by = "pid",
      newdata = datagrid(pid = c("Dem", "Rep", "Ind"))) |> #smaller dataset
        tidy())) |>
  unnest(margins) |>
  select(question, pid, estimate, std.error, 
         conf.low, conf.high, p.value) |> 
  mutate(decrease = -estimate,
         conf.low = -conf.low,
         conf.high = -conf.high)

ggplot(pid_certainty, aes(x = decrease, y = reorder(question, decrease), color = pid)) +
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
    good_threaten = "Renee Good threatened\nICE agents",
    good_obey = "R.G. obeyed ICE\norders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "R.G. tried to run over\nICE agent",
    good_ice_injured = "ICE agent(s)\nwere injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was\njustified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  scale_color_manual(values = c("Dem" = "#619CFF", "Rep" = "#F8766D", "Ind" = "#825187")) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(
    title = "Video effect on certainty, by party",
    subtitle = "% by which treatment reduces uncertainty, by \nparticipant's party ID",
    caption = "Weighted OLS interacted by party, controlling for prior exposure",
    x = "",
    y = NULL,
    color = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    plot.caption = element_text(hjust = 0, face = "italic", size = 8))

#relative odds
pid_certainty_or <- evidence |> 
  mutate(not_sure = as.numeric(response == 3),
         pid7_baseline = case_match(pid7_baseline, 
                                    c(1, 2, 3) ~ "Dem",
                                    c(5, 6, 7) ~ "Rep",
                                    4 ~ "Ind",
                                    8 ~ NA),
         pid = factor(pid7_baseline, levels = c("Dem", "Rep", "Ind"))) |> 
  filter(complete.cases(pid, not_sure)) |> 
  group_by(question) |>
  nest() |>
  mutate(
    model = map(data, ~glm(not_sure ~ ICE_Video * pid, 
                           data = ., 
                           family = binomial(link = "logit"))),
    margins = map(model, ~comparisons(
      ., 
      variables = "ICE_Video",
      by = "pid",
      newdata = datagrid(pid = c("Dem", "Rep", "Ind")),
      transform = "exp") |>  # get odds ratios
        tidy())) |>
  unnest(margins) |>
  select(question, pid, estimate,
         conf.low, conf.high, p.value)

ggplot(pid_certainty_or, aes(x = estimate, y = reorder(question, -estimate), color = pid)) +
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
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "Renee Good tried to\nrun over ICE agent",
    good_ice_injured = "ICE agent was injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was justified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  scale_color_manual(values = c("Dem" = "lightblue3", "Rep" = "firebrick", "Ind" = "#9987A5")) +
  labs(
    title = "Treatment effect on certainty ('Not Sure'), by Party ID",
    subtitle = "Logistic regression with interaction, presented as odds ratio",
    x = "",
    y = NULL,
    color = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top")
