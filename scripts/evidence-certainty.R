library(tidyverse)
library(here)
library(broom)

evidence <- here("data", "evidence-cleaned.rds") |> 
  read_rds()

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
    difference = (estimate2 - estimate1),  # treatment - control
    ci_low = -conf.high, #flipping CI because t.test does control - treatment
    ci_high = -conf.low) |>
  select(question, control_mean, treatment_mean, difference, 
         ci_low, ci_high, p.value)

ggplot(certainty, aes(x = difference, y = question)) +
  annotate("rect", 
           xmin = -.175, xmax = 0,
           ymin = 0, ymax = 4.5,
           fill = "gray90",
           alpha = .3) +
  geom_point(size = 3, color = "#5571ed") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), 
                 width = 0.2, 
                 height = 0.2,
                 color = "#5571ed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  annotate("text",
           x = -0.01,
           y = 4.5,
           hjust = 1,
           label = "Factual\nPolicy",
           fontface = "bold",
           size = 3) +
  annotate("segment",
           x = -0.005, xend = -0.005,
           y = 4.6, yend = 4.9,
           arrow = arrow(length = unit(0.12, "cm"))) +
  annotate("segment",
           x = -0.005, xend = -0.005,
           y = 4.4, yend = 4.1,
           arrow = arrow(length = unit(0.12, "cm"))) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
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
    title = "Watching Renee Good video decreased uncertainty of ICE attitudes",
    subtitle = "Difference in % responding 'Not Sure' (Treatment - Control)",
    x = NULL,
    y = NULL) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank())