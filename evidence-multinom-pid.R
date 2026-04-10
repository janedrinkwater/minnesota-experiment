library(tidyverse)
library(haven)
library(here)
library(labelled)
library(broom)

sav <- here("data", "evidence-sav.rds") |> 
  read_rds()

# long format for easier analysis (should be in analysis script)
long <- sav |> 
  dplyr::select(treatment, pid3lean, all_of(lbls$var_name)) |>
  pivot_longer(cols = all_of(lbls$var_name),
               names_to = "question",
               values_to = "response") |> 
  mutate(
    proice = case_when(
      question %in% c("good_obey", "ice_mn_killing_invest", "supp_abolishice") ~ 
        case_match(response, "Yes" ~ "No", "No" ~ "Yes", "Not sure" ~ "Not sure"),
      TRUE ~ response),
    proice = factor(proice))

diff_results <- long |> 
  filter(complete.cases(pid3lean, proice),
         pid3lean %in% c("Rep", "Dem")) |>
  group_by(question, pid3lean, treatment) |> 
  count(proice) |> 
  mutate(p = proportions(n)) |> 
  dplyr::select(- n) |> 
  pivot_wider(names_from = proice,
              values_from = p) 

#significance
significance_tests <- long |> 
  filter(complete.cases(pid3lean, proice),
         pid3lean %in% c("Rep", "Dem")) |>
  group_by(question, pid3lean) |> 
  summarise(
    # Get counts for each group
    n_yes_treatment = sum(proice == "Yes" & treatment == TRUE),
    n_yes_control = sum(proice == "Yes" & treatment == FALSE),
    n_no_treatment = sum(proice == "No" & treatment == TRUE),
    n_no_control = sum(proice == "No" & treatment == FALSE),
    n_treatment = sum(treatment == TRUE),
    n_control = sum(treatment == FALSE),
    
    # Proportions
    p_yes_treatment = n_yes_treatment / n_treatment,
    p_yes_control = n_yes_control / n_control,
    p_no_treatment = n_no_treatment / n_treatment,
    p_no_control = n_no_control / n_control,
    
    # Your difference effect
    diff_effect = (p_yes_treatment - p_no_treatment) - (p_yes_control - p_no_control),
    
    # Standard errors for each proportion
    se_yes_treatment = sqrt(p_yes_treatment * (1 - p_yes_treatment) / n_treatment),
    se_yes_control = sqrt(p_yes_control * (1 - p_yes_control) / n_control),
    se_no_treatment = sqrt(p_no_treatment * (1 - p_no_treatment) / n_treatment),
    se_no_control = sqrt(p_no_control * (1 - p_no_control) / n_control),
    
    # SE of difference-in-differences
    # SE[(A-B)-(C-D)] = sqrt(SE_A^2 + SE_B^2 + SE_C^2 + SE_D^2)
    se_diff_effect = sqrt(se_yes_treatment^2 + se_yes_control^2 + 
                            se_no_treatment^2 + se_no_control^2),
    
    # Z-test (large sample approximation)
    z_stat = diff_effect / se_diff_effect,
    
    # Two-tailed p-value
    p.value = 2 * pnorm(abs(z_stat), lower.tail = FALSE),
    
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ "†",
      TRUE ~ ""
    ),
    .groups = "drop"
  ) |>
  dplyr::select(question, pid3lean, diff_effect, se_diff_effect, z_stat, p.value, significance)

#effect on not sure by party
diff_results |> 
  dplyr::select(- c(`Yes`, `No`)) |> 
  rename(not_sure = `Not sure`) |> 
  group_by(question, pid3lean) |> 
  pivot_wider(names_from = treatment,
              values_from = not_sure) |> 
  rename(control = `FALSE`,
         treatment = `TRUE`) |>
  mutate(effect_notsure = treatment - control,
         y_position = case_match(question,
                                 "good_obey" ~ 1,
                                 "good_car_contact" ~ 2,
                                 "good_ice_injured" ~ 3,
                                 "good_threaten" ~ 4,
                                 "good_ice_professional" ~ 5,
                                 "good_attempt_runover" ~ 6,
                                 "justified_ice_shooting" ~ 7,
                                 "ice_mn_killing_invest" ~ 8,
                                 "ice_mn_good_widow" ~ 9,
                                 "supp_abolishice" ~ 10) + 
           ifelse(pid3lean == "Rep", 0.1, -0.1)) |> 
  ggplot(aes(x = effect_notsure, y = y_position, color = pid3lean)) +
  geom_segment(aes(x = control, xend = treatment, y = y_position, yend = y_position),
               arrow = arrow(type = "closed", length = unit(0.1, "inches")),
               linewidth = 1) +
  geom_text(aes(x = (control + treatment) / 2,  # Center between start and end
                label = 100 * round(effect_notsure, 2),
                color = pid3lean),
            vjust = -.5,
            fontface = "bold",
            show.legend = FALSE) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("Rep"= "tomato3", "Dem" = "steelblue")) +
  scale_y_continuous(breaks = 1:n_distinct(diff_results$question),
                     labels = c(
                       good_threaten = "R.G. threatened\nICE agents",
                       good_obey = "R.G. obeyed ICE orders",
                       good_car_contact = "R.G.'s car contacted\nICE agent",
                       good_attempt_runover = "Renee Good tried to\nrun over ICE agent",
                       good_ice_injured = "ICE agent(s)\nwere injured",
                       good_ice_professional = "ICE agents behaved\nprofessionally",
                       justified_ice_shooting = "ICE shooting was\njustified",
                       ice_mn_killing_invest = "ICE agent should\nbe investigated",
                       ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
                       supp_abolishice = "Support abolishing ICE")) +
  guides(color = guide_legend(),
         shape = "none",
         size = "none") +
  labs(
    title = "Treatment effect on uncertainty by party",
    subtitle = "Difference in % uncertain (treatment - control) by partisanship",
    x = NULL,
    y = NULL,
    shape = NULL,
    size = NULL,
    color = "Party") +
  theme_minimal() +
  theme(legend.position = "top",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank())

#same thing but with difference
diff_results |> 
  rename(not_sure = `Not sure`,
         yes = Yes,
         no = No) |> 
  group_by(question, pid3lean) |> 
  mutate(difference_treatment = (yes[treatment == TRUE] - no[treatment == TRUE]),
         difference_control = (yes[treatment == FALSE] - no[treatment == FALSE]),
         difference_effect = (difference_treatment - difference_control)) |>
  ggplot(aes(x = difference_effect, y = factor(question))) +
  geom_segment(aes(x = difference_control, xend = difference_treatment, 
                   y = question, yend = question, 
                   color = pid3lean),
               arrow = arrow(type = "closed", length = unit(0.1, "inches")),
               linewidth = 1) +
  geom_text(data = . %>% filter(treatment == TRUE),
            aes(x = (difference_control + difference_treatment) / 2,  # Center between start and end
                label = 100 * round(difference_effect, 2),
                color = pid3lean),
            vjust = -1,
            fontface = "bold",
            show.legend = FALSE) +
  geom_vline(xintercept = 0) +
  labs(
    title = "Treatment effect on ICE support by party",
    subtitle = "Treatment effect (treatment - control) on % support - % oppose",
    x = "#",
    caption = "Arrow starts at control and ends at treatment. Direction: effect direction. Length: effect size",
    y = NULL,
    shape = NULL,
    size = NULL,
    color = "Party") +
  scale_x_continuous(labels = scales::label_percent()) +
  scale_y_discrete(labels = c(good_threaten = "R.G. threatened\nICE agents",
                              good_obey = "R.G. obeyed ICE orders",
                              good_car_contact = "R.G.'s car contacted\nICE agent",
                              good_attempt_runover = "Renee Good tried to\nrun over ICE agent",
                              good_ice_injured = "ICE agent(s)\nwere injured",
                              good_ice_professional = "ICE agents behaved\nprofessionally",
                              justified_ice_shooting = "ICE shooting was\njustified",
                              ice_mn_killing_invest = "ICE agent should\nbe investigated",
                              ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
                              supp_abolishice = "Abolishing ICE")) +
  scale_color_manual(values = c("Rep" = "tomato3", "Dem" = "steelblue")) +
  theme_minimal() +
  theme(legend.position = "top",
        plot.caption = element_text(hjust = 0, face = "italic", size = 8))

#both effects
diff_results |> 
  rename(not_sure = `Not sure`,
         yes = Yes,
         no = No) |> 
  group_by(question, pid3lean) |> 
  summarise(
    # Effect on "not sure"
    effect_notsure = not_sure[treatment == TRUE] - not_sure[treatment == FALSE],
    # Effect on difference (yes - no)
    difference_treatment = (yes[treatment == TRUE] - no[treatment == TRUE]),
    difference_control = (yes[treatment == FALSE] - no[treatment == FALSE]),
    difference_effect = difference_treatment - difference_control,
    .groups = "drop") |> 
  pivot_longer(cols = c(effect_notsure, difference_effect),
               names_to = "effect_type",
               values_to = "effect_value") |> 
  mutate(effect_type = factor(effect_type, 
                              levels = c("effect_notsure", "difference_effect"),
                              labels = c("Uncertainty", "Direction"))) |> 
  ggplot(aes(x = effect_value, y = question, 
             color = pid3lean)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  facet_wrap(~ effect_type, scales = "free_x") +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("Rep" = "tomato3", "Dem" = "steelblue")) +
  scale_y_discrete(labels = c(
    good_threaten = "R.G. threatened\nICE agents",
    good_obey = "R.G. obeyed ICE orders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "Renee Good tried to\nrun over ICE agent",
    good_ice_injured = "ICE agent(s)\nwere injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was\njustified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    supp_abolishice = "Support abolishing ICE")) +
  labs(
    title = "Treatment effects on uncertainty and direction by party",
    subtitle = "Percentage point change (treatment - control)",
    x = "Effect size",
    y = NULL,
    color = "Party"
  ) +
  theme_bw() +
  theme(legend.position = "top",
        strip.text = element_text(face = "bold", size = 11))
