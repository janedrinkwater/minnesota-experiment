library(tidyverse)
library(here)
library(haven)
library(broom)

evidence <- here("data", "evidence-cleaned.rds") |> 
  read_rds()

dir_evidence <- evidence |> 
  filter(response != 3) |> #3 is "not sure"; subset to only those with a direction
  mutate(agree = ifelse(response == 1, 1, 0)) |> #turning yes (1) to 1 and no (2) to 0
  group_by(question) |> 
  nest() |> 
  mutate(model = map(data, ~glm(agree ~ ICE_Video, 
                                data = ., 
                                family = binomial)),
         tidy_model = map(model, tidy, conf.int = TRUE, exponentiate = TRUE)) |> 
  unnest(tidy_model) |> 
  filter(term == "ICE_Video") |>
  select(question, estimate, conf.low, conf.high, p.value)

ggplot(dir_evidence, aes(x = estimate, y = question)) +
  annotate("rect", 
           xmin = 0.2, xmax = 1.5,
           ymin = 0, ymax = 4.5,
           fill = "gray90",
           alpha = .3) +
  geom_point(size = 3, color = "#5571ed") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 width = 0.2,
                 height = 0.2,
                 color = "#5571ed") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  annotate("text",
           x = 0.26,
           y = 4.5,
           hjust = 0,
           label = "Factual\nPolicy",
           fontface = "bold",
           size = 3) +
  annotate("segment",
           x = 0.23, xend = 0.23,
           y = 4.6, yend = 4.9,
           arrow = arrow(length = unit(0.12, "cm"))) +
  annotate("segment",
           x = 0.23, xend = 0.23,
           y = 4.4, yend = 4.1,
           arrow = arrow(length = unit(0.12, "cm"))) +
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
  labs(
    title = "Watching ICE Video has limited directional effect",
    subtitle = "Treatment effect on agreement with ICE statements (Conditional on Opinion Certainty)",
    y = NULL,
    x = "Odds Ratio") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank())