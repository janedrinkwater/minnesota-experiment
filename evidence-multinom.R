library(tidyverse)
library(haven)
library(here)
library(nnet)
library(emmeans)

evidence <- here("data", "evidence-cleaned.rds") |> 
  read_rds() |> 
  mutate(response = as_factor(response))

# Fit models
results <- evidence |>
  group_by(question) |>
  nest() |>
  mutate(
    data = map(data, ~mutate(.x, response = factor(response))),
    model = map(data, ~multinom(response ~ ICE_Video, data = .x, trace = FALSE)),
    # Get contrasts with emmeans
    results = map(model, ~{
      emm <- emmeans(.x, ~ ICE_Video, mode = "prob")
      contrast(emm, method = "revpairwise", infer = TRUE) |>  # Use contrast instead
        as_tibble() |>
        rename(response = 1, prob_diff = estimate, se = SE, 
               ci_lower = lower.CL, ci_upper = upper.CL) |>
        mutate(response = str_remove(response, " prob"))  # Clean up labels
    })
  ) |>
  select(question, results) |>
  unnest(results)

# Function to get predictions and SEs using delta method
get_diff <- function(model, dat) {
  # Get predictions
  pred_0 <- predict(model, newdata = data.frame(ICE_Video = 0), type = "probs")
  pred_1 <- predict(model, newdata = data.frame(ICE_Video = 1), type = "probs")
  diff <- pred_1 - pred_0
  
  # Calculate empirical SE from the data
  # Split by ICE_Video
  dat_0 <- dat[dat$ICE_Video == 0, ]
  dat_1 <- dat[dat$ICE_Video == 1, ]
  
  # Get weighted proportions for each response
  props_0 <- sapply(levels(dat$response), function(r) {
    weighted.mean(dat_0$response == r, dat_0$weight, na.rm = TRUE)
  })
  props_1 <- sapply(levels(dat$response), function(r) {
    weighted.mean(dat_1$response == r, dat_1$weight, na.rm = TRUE)
  })
  
  # Calculate SE using standard formula for difference in proportions
  n_0 <- sum(dat$ICE_Video == 0)
  n_1 <- sum(dat$ICE_Video == 1)
  
  se <- sqrt(props_0 * (1 - props_0) / n_0 + props_1 * (1 - props_1) / n_1)
  
  tibble(
    response = names(diff),
    prob_diff = as.numeric(diff),
    se = se,
    ci_lower = prob_diff - 1.96 * se,
    ci_upper = prob_diff + 1.96 * se
  )
}

# Run models
results <- evidence |>
  group_by(question) |>
  nest() |>
  mutate(
    # Make sure response is a factor
    data = map(data, ~mutate(.x, response = factor(response))),
    # Fit model with error handling
    model = map(data, ~tryCatch(
      multinom(response ~ ICE_Video, data = .x, weights = weight, trace = FALSE),
      error = function(e) {
        message("Error fitting model: ", e$message)
        return(NULL)
      }
    )),
    # Get predictions with CIs
    results = map2(model, data, ~{
      if (is.null(.x)) return(NULL)
      tryCatch(
        get_diff(.x, .y),
        error = function(e) {
          message("Error getting predictions: ", e$message)
          return(NULL)
        }
      )
    })
  ) |>
  filter(!map_lgl(results, is.null)) |>
  select(question, results) |>
  unnest(results)

# Plot
ggplot(results, aes(x = prob_diff, y = question, color = response)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = ci_lower, xmax = ci_upper),
                  position = position_dodge(width = 0.5), size = 0.5) +
  labs(x = "Difference in Predicted Probability (Treatment - Control)",
       y = NULL, 
       color = "Response",
       title = "Video Effect on Response Probabilities") +
  theme_minimal() +
  theme(legend.position = "bottom")
