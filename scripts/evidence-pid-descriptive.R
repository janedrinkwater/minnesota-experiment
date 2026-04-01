library(tidyverse)
library(here)
library(haven)
library(broom)

evidence <- here("data", "evidence-cleaned.rds") |> 
  read_rds()

#just for people who saw the video, what are partisan differences?
evidence |> 
  mutate(pid7_baseline = case_match(pid7_baseline, 
                                    c(1, 2, 3) ~ "Dem",
                                    c(5, 6, 7) ~ "Rep",
                                    c(4, 8) ~ NA),
         pid = factor(pid7_baseline, levels = c("Dem", "Rep")),
         response = as_factor(response),
         response = factor(response, levels = c("No", "Not sure", "Yes")),
         question = factor(question, levels = c("good_obey", "good_threaten", "good_car_contact",
                                                "good_attempt_runover", "good_ice_injured",
                                                "good_ice_professional", "justified_ice_shooting",
                                                "ice_mn_killing_invest", "ice_mn_good_widow",
                                                "collapsed_abolishice"))) |>
  filter(complete.cases(pid, response),
         ICE_Video == 1,
         pid %in% c("Dem", "Rep")) |> 
  ggplot(aes(y = pid, x = after_stat(count), fill = response, color = pid, weight = weight)) +
  geom_bar(position = "fill",
           linewidth = 1) +
  facet_grid(question ~ ., switch = "y",
             labeller = labeller(question = c(
               good_obey = "Renee Good obeyed\nICE orders",
               good_threaten = "R.G. threatened ICE\nagent",
               good_car_contact = "R.G.'s car contacted\nICE agent",
               good_attempt_runover = "R.G. tried to run over\nICE agent",
               good_ice_injured = "ICE agent was injured",
               good_ice_professional = "ICE agents behaved\nprofessionally",
               justified_ice_shooting = "ICE shooting was justified",
               ice_mn_killing_invest = "ICE agent should be\ninvestigated",
               ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
               collapsed_abolishice = "Support abolishing ICE"
             ))) +
  labs(title = "Partisans interpret video differently",
       subtitle = "Distribution of responses by party, among participants\nwho watched the video",
       y = NULL, 
       x = NULL,
       fill = NULL) +
  scale_fill_manual(values = c("Yes" = "#4A5A6A", "Not sure" = "#e8e3d5", "No" = "#f5f5f5")) +
  scale_color_manual(values = c("Dem" = "#619CFF", "Rep" = "#F8766D"), guide = "none") +
  scale_y_discrete(position = "right") +
  scale_x_continuous(labels = scales::label_percent()) +
  theme_minimal() +
  theme(strip.text.y.left = element_text(angle = 0, hjust = 1),
        axis.text.y.right = element_text(angle = 0),
        legend.position = "top",
        plot.title = element_text(face = "bold"))

#dot plot of agreement by PID (facetted by treatment)
evidence |> 
  filter(pid7_baseline != 8,
         complete.cases(response, ICE_Video)) |> 
  mutate(agree = if_else(response == 1, 1, 0),
         ICE_Video = if_else(ICE_Video == 1, "Treatment", "Control")) |> 
  group_by(pid7_baseline, question, ICE_Video) |> 
  summarise(mean = mean(agree)) |> 
  group_by(question, ICE_Video) |> 
  mutate(diff = abs(mean[pid7_baseline == 7] - mean[pid7_baseline == 1])) |>
  ggplot(aes(y = reorder(factor(question), diff), x = mean, color = factor(pid7_baseline))) +
  geom_point(size = 5, alpha = .5) +
  labs(title = "ICE attitudes vary by partisanship",
       subtitle = "Share agreeing with each statement, by party ID",
       x = NULL,
       y = NULL,
       color = "Party ID") +
  facet_wrap(~ICE_Video) +
  scale_color_manual(values = c("navyblue", "blue","#619CFF","purple", "#F8766D","tomato3","firebrick"),
                     labels = c("Strong Dem", "Weak Dem", "Lean Dem", "Ind", "Lean Rep", "Weak Rep", "Strong Rep")) +
  scale_x_continuous(labels = scales::label_percent()) +
  scale_y_discrete(labels = c(
    good_threaten = "Renee Good threatened\nICE agent",
    good_obey = "R.G. obeyed ICE orders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "R.G. tried to run over\nICE agent",
    good_ice_injured = "ICE agent was injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was justified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(),
        plot.title = element_text(face = "bold"))

#variant for March 25 powerpoint
evidence |> 
  filter(pid7_baseline != 8,
         ICE_Video == 1,
         complete.cases(response, ICE_Video)) |> 
  mutate(agree = if_else(response == 1, 1, 0)) |> 
  group_by(pid7_baseline, question, ICE_Video) |> 
  summarise(mean = mean(agree)) |> 
  group_by(question, ICE_Video) |> 
  mutate(diff = abs(mean[pid7_baseline == 7] - mean[pid7_baseline == 1])) |>
  ggplot(aes(y = reorder(factor(question), diff), x = mean, color = factor(pid7_baseline))) +
  geom_point(size = 5, alpha = .5) +
  labs(title = "ICE attitudes vary by partisanship",
       subtitle = "Share agreeing with each statement by party, among participants\nwho watched the video",
       x = NULL,
       y = NULL,
       color = "Party ID") +
  scale_color_manual(values = c("navyblue", "blue","#619CFF","purple", "#F8766D","tomato3","firebrick"),
                     labels = c("Strong Dem", "Weak Dem", "Lean Dem", "Ind", "Lean Rep", "Weak Rep", "Strong Rep")) +
  scale_x_continuous(labels = scales::label_percent()) +
  scale_y_discrete(labels = c(
    good_threaten = "Renee Good threatened\nICE agent",
    good_obey = "R.G. obeyed ICE orders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "R.G. tried to run over\nICE agent",
    good_ice_injured = "ICE agent was injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was justified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(),
        plot.title = element_text(face = "bold"))

#difference in agreement (R - D)
evidence |> 
  mutate(rep = case_when(pid7_baseline %in% c(1, 2, 3) ~ "Dem",
                         pid7_baseline %in% c(5, 6, 7) ~ "Rep"),
         agree = if_else(response == 1, 1, 0)) |>
  filter(complete.cases(rep, response, ICE_Video)) |> 
  group_by(rep, question, ICE_Video) |> 
  summarise(mean = mean(agree), .groups = "drop") |> 
  pivot_wider(names_from = rep, values_from = mean) |> 
  mutate(diff = Rep - Dem) |> 
  group_by(question) |> 
  mutate(diff_in_diff = diff[ICE_Video == 1] - diff[ICE_Video == 0])

##Regression/difference-in-difference. Rep - Dem, Treatment - Control.
results <- evidence |> 
  mutate(rep = case_when(pid7_baseline %in% c(1, 2, 3) ~ "Dem",
                         pid7_baseline %in% c(5, 6, 7) ~ "Rep"),
         agree = if_else(response == 1, 1, 0),
         rep_binary = if_else(rep == "Rep", 1, 0)) |>
  filter(complete.cases(rep, response, ICE_Video)) |> 
  group_by(question) |> 
  summarise(
    model = list(lm(agree ~ rep_binary * ICE_Video)),
    .groups = "drop"
  ) |> 
  mutate(tidy_model = map(model, tidy, conf.int = TRUE)) |> 
  unnest(tidy_model) |> 
  filter(term == "rep_binary:ICE_Video")  # This IS the diff-in-diff!

# Plot
results |> 
  ggplot(aes(x = question, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(y = "Difference-in-Differences (with 95% CI)", x = "Question")

#Which questions have the biggest gap in Rep - Dem opinions? Needs error bars
evidence |> 
  mutate(rep = case_when(pid7_baseline %in% c(1, 2, 3) ~ "Dem",
                         pid7_baseline%in% c(5, 6, 7) ~ "Rep"),
         agree = if_else(response == 1, 1, 0)) |>
  filter(complete.cases(rep, response),
         ICE_Video == 1) |> 
  group_by(rep, question) |> 
  summarise(mean = mean(agree), .groups = "drop") |> 
  pivot_wider(names_from = rep, values_from = mean) |> 
  mutate(diff = abs(Rep - Dem),
         question_type = if_else(question %in% c("good_threaten", 
                                                 "good_obey", 
                                                 "good_car_contact", 
                                                 "good_attempt_runover", 
                                                 "good_ice_injured", 
                                                 "good_ice_professional"), "Factual", "Policy")) |>
  ggplot(aes(y = reorder(factor(question), diff), x = diff, color = question_type)) +
  geom_point(size = 5) +
  labs(title = "Post-video difference in Rep and Dem attitudes",
       subtitle = "Difference in % of Reps and Dems agreeing with each statement, among\nparticipants who watched the video",
       x = NULL,
       y = NULL,
       color = "Question type") +
  scale_color_manual(values = c("Factual" = "#added0", "Policy" = "#c4a958")) +
  scale_x_continuous(labels = scales::label_percent()) +
  scale_y_discrete(labels = c(
    good_threaten = "Renee Good threatened\nICE agent",
    good_obey = "R.G. obeyed ICE orders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "R.G. tried to run over\nICE agent",
    good_ice_injured = "ICE agent was injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was justified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(),
        plot.title = element_text(face = "bold"),
        legend.position = "top")


#Effect of party ID on agreement, averaging across treatment and control
#controlling for saw_ice_video because it's not evenly distributed by party
model <- evidence |> 
  filter(pid7_baseline != 8) |> 
  mutate(agree = if_else(response == 1, 1, 0),
         saw_ice_video = as.numeric(saw_ice_video == 1),
         pid = case_when(pid7_baseline %in% c(1:3) ~ 1,
                         pid7_baseline == 4 ~ 4,
                         pid7_baseline %in% c(5:7) ~ 3)) |> 
  group_by(question) |> 
  nest() |> 
  mutate(
    # Fit logistic regression
    model = map(data, ~glm(agree ~ pid + ICE_Video + saw_ice_video, 
                           data = ., 
                           family = binomial(link = "logit"),
                           weights = weight)),
    # Extract coefficients
    tidy_model = map(model, tidy, conf.int = TRUE),
    # Get marginal effects (probability scale)
    margins = map(model, ~{
      # Average marginal effect
      marginaleffects::avg_slopes(., variables = "pid")
    })
  )

# Log results
logit_results <- model |> 
  select(question, tidy_model, margins) |> 
  unnest(tidy_model) |> 
  filter(term == "pid")

# Probability scale (marginal effects)
prob_results <- logit_results |> 
  select(question, margins) |> 
  unnest(margins) |> 
  mutate(question_type = if_else(question %in% c("good_threaten", 
                                                 "good_obey", 
                                                 "good_car_contact", 
                                                 "good_attempt_runover", 
                                                 "good_ice_injured", 
                                                 "good_ice_professional"), "Factual", "Policy"))

ggplot(prob_results, aes(x = estimate, y = reorder(question, estimate), color = question_type)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0) +
  labs(title = "Effect of partisanship on ICE attitudes",
       subtitle = "Effect of becoming more Republican on chance of agreeing\n(1-point shift on a 3-point scale; leaners included in party)",
       caption = "Weighted logit controlling for treatment and prior exposure to video",
       color = "Question type",
       x = "",
       y = NULL) +
  scale_x_continuous(labels = scales::label_percent()) +
  scale_y_discrete(labels = c(
    good_threaten = "Renee Good threatened\nICE agent",
    good_obey = "R.G. obeyed ICE orders",
    good_car_contact = "R.G.'s car contacted\nICE agent",
    good_attempt_runover = "R.G. tried to run over\nICE agent",
    good_ice_injured = "ICE agent was injured",
    good_ice_professional = "ICE agents behaved\nprofessionally",
    justified_ice_shooting = "ICE shooting was justified",
    ice_mn_killing_invest = "ICE agent should\nbe investigated",
    ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
    collapsed_abolishice = "Support abolishing ICE")) +
  scale_color_manual(values = c("Factual" = "#added0", "Policy" = "#c4a958")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(size = 8, face = "italic", hjust = 0),
        legend.position = "top")
