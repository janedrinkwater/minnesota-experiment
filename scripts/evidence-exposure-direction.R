library(tidyverse)
library(here)
library(haven)
library(broom)

evidence <- here("data", "evidence-cleaned.rds") |> 
  read_rds()

# Filter to only people who gave yes/no (drop "not sure")
exposure_direction <- evidence |> 
  filter(response != 3,
         !is.na(saw_ice_video)) |>
  mutate(agree = ifelse(response == 1, 1, 0))

#checking n - it's low for people with no prior exposure, (3-32, depending on question & group) 
#more no-exposure people in treatment group than control group, because we're filtering out DK here

diagnostic_table <- exposure_direction |> 
  group_by(question, ICE_Video, saw_ice_video) |> 
  summarise(
    n = n(),
    n_yes = sum(agree == 1),
    n_no = sum(agree == 0),
    pct_yes = mean(agree) * 100,
    .groups = "drop") |> 
  arrange(question, ICE_Video, saw_ice_video)

# Look for cells with 0% or 100% or very small n
print(diagnostic_table)

, #turning yes (1) to 1 and no (2) to 0
    saw_ice_video = case_match( #renaming
      saw_ice_video, 1 ~ "Yes",
      2 ~ "Heard",
      3 ~ "No"),
    saw_ice_video = factor(saw_ice_video, 
                           levels = c("No", #no is reference category
                                      "Heard", 
                                      "Yes"))) |> 
  group_by(question) |> 
  nest() |> 
  mutate(
    # Run model for each question
    model = map(data, ~glm(agree ~ ICE_Video * saw_ice_video,
                           data = .x,
                           family = binomial(link = "logit"))),
    
    # Extract tidy results with odds ratios
    results = map(model, ~tidy(.x, exponentiate = TRUE, conf.int = TRUE)))



# Get odds ratios by prior exposure group
exposure_direction <- evidence |> 
  filter(response != 3,
         !is.na(saw_ice_video)) |>
  mutate(
    agree = ifelse(response == 1, 1, 0),
    saw_ice_video = case_match(
      saw_ice_video, 
      1 ~ "Yes",
      2 ~ "Heard",
      3 ~ "No")) |> 
  group_by(question) |> 
  nest() |> 
  mutate(
    # Model with interaction
    model = map(data, ~glm(agree ~ ICE_Video * saw_ice_video, 
                           data = ., 
                           family = binomial)),
    
    # Extract treatment effects for each exposure group
    effects = map(model, function(fit) {
      est <- coef(fit)
      V <- vcov(fit)
      
      # Print coefficient names to debug (remove after checking)
      # print(names(est))
      
      # Get the actual interaction term names
      coef_names <- names(est)
      no_term <- coef_names[grepl("No", coef_names)]
      yes_term <- coef_names[grepl("Yes", coef_names)]
      
      tibble(
        exposure = c("Heard", "No", "Yes"),
        # Log odds ratios
        log_or = c(
          est["ICE_Video"],
          est["ICE_Video"] + est[no_term],
          est["ICE_Video"] + est[yes_term]
        ),
        # Standard errors
        se = c(
          sqrt(V["ICE_Video", "ICE_Video"]),
          sqrt(V["ICE_Video", "ICE_Video"] + 
                 V[no_term, no_term] + 
                 2 * V["ICE_Video", no_term]),
          sqrt(V["ICE_Video", "ICE_Video"] + 
                 V[yes_term, yes_term] + 
                 2 * V["ICE_Video", yes_term])
        )
      ) |>
        mutate(
          estimate = exp(log_or),
          conf.low = exp(log_or - 1.96 * se),
          conf.high = exp(log_or + 1.96 * se),
          z = log_or / se,
          p.value = 2 * (1 - pnorm(abs(z)))
        )
    })
  ) |>
  unnest(effects) |>
  select(question, exposure, estimate, conf.low, conf.high, p.value)

# Plot with facets by exposure
ggplot(dir_evidence_by_exposure, aes(x = estimate, y = question, color = exposure)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_color_manual(values = c("Heard" = "#7B9EFF", 
                                "No" = "#4A5FD1", 
                                "Yes" = "#2A3B8F"),
                     name = "Prior Exposure") +
  scale_y_discrete(labels = c(
    good_threaten = "R.G. threatened\nICE agents",
    good_obey = "R.G. obeyed\nICE orders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "Renee Good tried to\nrun over ICE agent",
    good_ice_injured = "ICE agent(s)\nwere injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was\njustified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  labs(
    title = "Treatment Effect by Prior Exposure to Video",
    subtitle = "Odds ratios for agreement (Conditional on Opinion Certainty)",
    y = NULL,
    x = "Odds Ratio") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom")