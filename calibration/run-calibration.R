# remotes::install_github("cvpia-osc/DSMflow")
# remotes::install_github("cvpia-osc/DSMtemperature")
# remotes::install_github("cvpia-osc/DSMhabitat")
# remotes::install_github("cvpia-osc/DSMscenario")


library(winterRunDSM)
library(GA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

source("calibration/fitness.R")
source("calibration/update-params.R")

params <- DSMCalibrationData::set_synth_years(winterRunDSM::params)

# Perform calibration --------------------
res <- ga(type = "real-valued",
          fitness =
            function(x) -winter_run_fitness(
              known_adults = DSMCalibrationData::grandtab_observed$winter,
              seeds = DSMCalibrationData::grandtab_imputed$winter,
              params = params,
              x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
              x[11]
            ),
          lower = c(2.5, rep(-3.5, 11)),
          upper = rep(3.5, 12),
          popSize = 150,
          maxiter = 10000,
          run = 50,
          parallel = TRUE,
          pmutation = .4)

readr::write_rds(res, paste0("calibration/fits/result-", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".rds"))

# Evaluate Results ------------------------------------
keep <- c(1, 3)
r1_solution <- res@solution[1, ]

r1_params <- update_params(x = r1_solution, winterRunDSM::params)
r1_params <- DSMCalibrationData::set_synth_years(r1_params)
r1_sim <- winter_run_model(seeds = DSMCalibrationData::grandtab_imputed$winter, mode = "calibrate",
                           ..params = r1_params,
                           stochastic = FALSE)


r1_nat_spawners <- as_tibble(r1_sim[keep, ,drop = F]) %>%
  mutate(watershed = DSMscenario::watershed_labels[keep]) %>%
  gather(year, spawners, -watershed) %>%
  mutate(type = "simulated",
         year = readr::parse_number(year) + 5)


r1_observed <- as_tibble((1 - winterRunDSM::params$proportion_hatchery[keep]) * DSMCalibrationData::grandtab_observed$winter[keep, ]) %>%
  mutate(watershed = DSMscenario::watershed_labels[keep]) %>%
  gather(year, spawners, -watershed) %>%
  mutate(type = "observed", year = as.numeric(year) - 1997) %>%
  filter(!is.na(spawners),
         year > 5)



r1_eval_df <- bind_rows(r1_nat_spawners, r1_observed)


r1_eval_df %>% 
  ggplot(aes(year, spawners, color = type)) + geom_line() + facet_wrap(~watershed, scales = "free_y")

r1_eval_df %>%
  spread(type, spawners) %>%
  ggplot(aes(observed, simulated)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Observed vs Predicted updated",
       x = "Observed Natural Spawners",
       y = "Predicted Natural Spawners") +
  xlim(0, 20000) +
  ylim(0, 20000)

r1_eval_df %>%
  spread(type, spawners) %>%
  filter(!is.na(observed)) %>%
  group_by(watershed) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  ) %>% arrange(desc(abs(r)))

r1_eval_df %>%
  spread(type, spawners) %>%
  filter(!is.na(observed)) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )


