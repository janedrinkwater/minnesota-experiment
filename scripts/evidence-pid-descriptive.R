library(tidyverse)
library(here)
library(haven)

evidence <- here("data", "evidence-cleaned.rds") |> 
  read_rds()

#looking at descriptive statistics
descriptive <- evidence |> 
  mutate(pid7_baseline = case_match(pid7_baseline, 
                                    c(1, 2, 3) ~ "Dem",
                                    c(5, 6, 7) ~ "Rep",
                                    4 ~ "Ind",
                                    8 ~ NA),
         pid = factor(pid7_baseline, levels = c("Dem", "Rep", "Ind")),
         response = as_factor(response),
         ICE_Video = factor(ICE_Video, labels = c("No Video", "Video"))) |>
  filter(complete.cases(pid, response)) |> 
  group_by(pid, question, ICE_Video, response) |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(pid, question, ICE_Video) |>
  mutate(proportion = n / sum(n))

descriptive |> 
  ggplot(aes(y = interaction(question, ICE_Video), x = proportion, fill = response, color = ICE_Video)) +
  geom_col(position = "stack") +
  facet_wrap(~pid) +
  theme_minimal() 

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
  ggplot(aes(y = pid, x = after_stat(count), fill = response, color = pid)) +
  geom_bar(position = "fill",
           linewidth = 1) +
  facet_grid(question ~ ., switch = "y",
             labeller = labeller(question = c(
               good_obey = "Renee Good obeyed\nICE orders",
               good_threaten = "R.G. threatened ICE agent",
               good_car_contact = "R.G.'s car contacted\nICE agent",
               good_attempt_runover = "R.G. tried to run over\nICE agent",
               good_ice_injured = "ICE agent was injured",
               good_ice_professional = "ICE agents behaved\nprofessionally",
               justified_ice_shooting = "ICE shooting was justified",
               ice_mn_killing_invest = "ICE agent should be\ninvestigated",
               ice_mn_good_widow = "R.G.'s widow should\nbe investigated",
               collapsed_abolishice = "Support abolishing ICE"
             ))) +
  labs(title = "After watching the same ICE video, partisans have polarized attitudes",
       subtitle = "Share of responses by party ID among those who watched the video",
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
