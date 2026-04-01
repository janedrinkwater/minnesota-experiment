library(tidyverse)
library(here)
library(haven)
library(broom)
library(nestedLogit)

test_data <- evidence |> filter(question == unique(evidence$question)[1])

# Try fitting with weights
test_model <- nestedLogit(
  response ~ ICE_Video,
  dichotomies = comparisons,
  data = test_data,
  weights = test_data$weight)

evidence <- here("data", "evidence-cleaned.rds") |> 
  read_rds() |> 
  mutate(response = as_factor(response),
         response = factor(response, 
                           levels = c("No", "Yes", "Not sure"))) |>
  filter(complete.cases(response, ICE_Video, weight))

comparisons <- logits(certainty = dichotomy("Not sure", 
                                            Sure=c("No", "Yes")),
                      direction = dichotomy("No", "Yes"))

models <- evidence |>
  group_by(question) |>
  nest() |>
  mutate(
    model = map(data, function(df) {
      nestedLogit(
        response ~ ICE_Video,
        dichotomies = comparisons,
        data = df,
        weights = df$weight)
    }))

# Tidy coefficients with standard errors
tidy_coefs <- models |>
  mutate(tidy = map(model, tidy)) |>
  select(question, tidy) |>
  unnest(tidy)

# Calculate treatment effects for each dichotomy
treatment_effects <- tidy_coefs |>
  filter(term == "ICE_Video") |>
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error,
    question_type = if_else(question %in% c("good_threaten", 
                                                 "good_obey", 
                                                 "good_car_contact", 
                                                 "good_attempt_runover", 
                                                 "good_ice_injured", 
                                                 "good_ice_professional"), "Factual", "Policy"))

ggplot(treatment_effects, aes(x = estimate, y = reorder(question, estimate * (response == "certainty")), , color = response, shape = question_type)) +
  geom_point(size = 2,
             position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(xmin = lower, xmax = upper), 
                width = 0.2,
                position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "ICE video changes attitude certainty more than direction",
       subtitle = "Treatment effect on predicted probability of attitude certainty and agreement,
calculated from nested Logit",
       y = NULL,
       x = "Effect on predicted probability",
       color = NULL,
       shape = NULL) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_y_discrete(labels = c(
    good_threaten = "R.G. threatened ICE\nagents",
    good_obey = "Renee Good obeyed\nICE orders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "R.G. tried to run over\nICE agent",
    good_ice_injured = "ICE agent(s)\nwere injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was\njustified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  scale_color_manual(values = c("certainty" = "#added0", "direction" = "#c4a958"),
                     labels = c("Certainty", "Agreement")) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "top") 
