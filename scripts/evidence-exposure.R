library(tidyverse)
library(here)
library(haven)
library(broom)
library(marginaleffects)

evidence <- here("data", "evidence-cleaned.rds") |> 
  read_rds()

#checking n & separations for relative odds - low n for people with no prior exposure, (3-32 ppl depending on question & group) 
#more no-exposure people in treatment group than control group

separation_check <- evidence |> 
  mutate(not_sure = as.numeric(response == 3),
         saw_ice_video = case_match(
           saw_ice_video, 1 ~ "Yes",
           2 ~ "Heard",
           3 ~ "No"),
         saw_ice_video = factor(saw_ice_video, levels = c("No", "Heard", "Yes"))) |> 
  filter(!is.na(saw_ice_video), !is.na(not_sure)) |> 
  group_by(question, ICE_Video, saw_ice_video) |>
  summarise(
    n = n(),
    n_notsure = sum(not_sure),
    prop_notsure = mean(not_sure),
    .groups = "drop")

View(separation_check)
#39 no-exposure in control group; 38 in treatment group
#on factual questions, 82-92% of no-exposure in control group were not sure. on policy, 51-77% were not sure
#on factual questions, 

#marginal effects (LLM w interaction)
exposure_certainty <- evidence |> 
  mutate(not_sure = as.numeric(response == 3),
    saw_ice_video = case_match( #renaming
      saw_ice_video, 1 ~ "Yes",
      2 ~ "Heard",
      3 ~ "No"),
    saw_ice_video = factor(saw_ice_video, levels = c("No", "Heard", "Yes"))) |> 
  filter(!is.na(saw_ice_video), !is.na(not_sure)) |> 
  group_by(question) |>
  nest() |>
  mutate(
    model = map(data, ~lm(not_sure ~ ICE_Video * saw_ice_video, data = .)),
    margins = map(model, ~comparisons(
      ., 
      variables = "ICE_Video",
      by = "saw_ice_video",
      newdata = datagrid(saw_ice_video = c("No","Heard", "Yes"))) |> #smaller dataset
  tidy())) |>
  unnest(margins) |>
  select(question, saw_ice_video, estimate, std.error, 
         conf.low, conf.high, p.value)

ggplot(exposure_certainty, aes(x = estimate, y = question, color = video)) +
  annotate("rect", 
           xmin = -0.8, xmax = 0.1,
           ymin = 0, ymax = 4.5,
           fill = "gray90",
           alpha = .3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    position = position_dodge(width = 0.6),
    height = 0.2,
    linewidth = 0.6) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 2.5) +
  annotate("text",
           x = -.76,
           y = 4.5,
           hjust = 0,
           label = "Factual\nPolicy",
           fontface = "bold",
           size = 3) +
  annotate("segment",
           x = -.78, xend = -.78,
           y = 4.6, yend = 4.9,
           arrow = arrow(length = unit(0.12, "cm"))) +
  annotate("segment",
           x = -.78, xend = -.78,
           y = 4.4, yend = 4.1,
           arrow = arrow(length = unit(0.12, "cm"))) +
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
  scale_color_manual(values = c("Yes" = "#205072","Heard" = "#329D9C","No" = "#added0"),
                     labels = c("Yes" = "Saw video", "Heard" = "Didn't see video, but heard about it", "No" = "Didn't see or hear about video")) +
  labs(
    title = "Prior exposure to ICE video changes video effect on attitude certainty",
    subtitle = "Treatment effects on whether respondents said 'Not sure', interacted by
previous exposure to ICE video",
    x = "",
    y = NULL,
    color = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top")

#relative odds
exposure_certainty_or <- evidence |> 
  mutate(not_sure = as.numeric(response == 3),
         saw_ice_video = case_match(
           saw_ice_video, 1 ~ "Yes",
           2 ~ "Heard",
           3 ~ "No"),
         saw_ice_video = factor(saw_ice_video, levels = c("No", "Heard", "Yes"))) |> 
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
      newdata = datagrid(saw_ice_video = c("No","Heard", "Yes")),
      transform = "exp") |>  # get odds ratios
        tidy())) |>
  unnest(margins) |>
  select(question, video = saw_ice_video, estimate,
         conf.low, conf.high, p.value)
