library(tidyverse)
library(here)
library(haven)
library(broom)
library(marginaleffects)

evidence <- here("data", "evidence-cleaned.rds") |> 
  read_rds()

#checking n - it's low for people with no prior exposure, (3-32, depending on question & group) 
#more no-exposure people in treatment group than control group, because we're filtering out DK here

diagnostic_table <- evidence |> 
  filter(response != 3,
         !is.na(saw_ice_video)) |>
  mutate(agree = ifelse(response == 1, 1, 0)) |> 
  group_by(question, ICE_Video, saw_ice_video) |> 
  summarise(
    n = n(),
    n_yes = sum(agree == 1),
    n_no = sum(agree == 0),
    pct_yes = mean(agree) * 100,
    .groups = "drop") |> 
  arrange(question, ICE_Video, saw_ice_video)

#odds ratio (controlling for party ID)
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
    model = map(data, ~glm(agree ~ ICE_Video * saw_ice_video + pid3, 
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
    title = "Prior exposure to ICE video doesn't change video effect on attitudes",
    subtitle = "Treatment effect on respondents' odds of agreeing with each statement, from Logit interacted 
by previous exposure to ICE video",
    x = "Odds Ratio",
    y = NULL,
    color = NULL) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top")
