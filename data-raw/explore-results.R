library(winterRunDSM)
library(tidyverse)
r2r_seeds <- winterRunDSM::winter_run_model(scenario = NULL, mode = "seed",
                                            seeds = NULL, ..params = winterRunDSM::r_2_r_baseline_params)

r2r_model_results <- winterRunDSM::winter_run_model(mode = "simulate", ..params = winterRunDSM::r_2_r_baseline_params,
                                                    seeds = r2r_seeds)
r2r_model_results$spawners
r2r_model_results$phos

spawn <- dplyr::as_tibble(r2r_model_results$spawners) |>
  dplyr::mutate(location = fallRunDSM::watershed_labels) |>
  pivot_longer(cols = c(`1`:`20`), values_to = 'spawners', names_to = "year") %>%
  group_by(year, location) |>
  summarize(total_spawners = sum(spawners)) |>
  filter(location != "Feather River") |>
  mutate(year = as.numeric(year)) %>%
  ggplot(aes(year, total_spawners, color = location)) +
  geom_line() +
  theme_minimal() +
  labs(y = "Spawners",
       x = "Year") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = 1:20) +
  theme(text = element_text(size = 20))

spawn
