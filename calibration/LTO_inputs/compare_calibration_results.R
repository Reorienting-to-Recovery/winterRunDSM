

# compare results

r2r_param_mapping <- tibble("R2R_index" = 1:12) |> 
  mutate("R2R_name" = c("surv_adult_enroute_int",
                    "..surv_juv_rear_int",
                    "..surv_juv_rear_contact_points",
                    "..surv_juv_rear_prop_diversions",
                    "..surv_juv_rear_total_diversions",
                    "..surv_juv_bypass_int",
                    "..surv_juv_delta_int",
                    "..surv_juv_delta_contact_points",
                    "..surv_juv_delta_total_diverted",
                    "..surv_juv_outmigration_sj_int",
                    "..surv_egg_to_fry_mean_egg_temp_effect",
                    "..ocean_entry_success_int"))

LTO_param_mapping <- tibble("LTO_index" = 1:16,
                            "LTO_name" = c("juvenile in-channel survival intercept",
                                           "juvenile floodplains bypass survival intercept",
                                           "juvenile delta survival functions rearing intercept",
                                           "juvenile san joaquin outmigration survival intercept",
                                           "juvenile outmigration survival parameter intercept 1",
                                           "juvenile outmigration survival parameter intercept 2",
                                           "juvenile delta outmigration survival intercept 1",
                                           "juvenile delta outmigration survival intercept 2",
                                           "juvenile delta outmigration survival intercept 3",
                                           "ocean entry survival intercept",
                                           "adult en route survival intercept",
                                           "egg to fry survival temperature coefficient",
                                           "juvenile contact points survival",
                                           "juvenile proportion diverted survival parameter",
                                           "juvenile total diverted survival parameter",
                                           "juvenile rearing survival total diversions parameter"))

map_params <- tibble("LTO_index" = c(1:16, NA),
                     "R2R_index" = c(2, 6, 7, 10,
                                     NA, NA, NA, NA,
                                     NA, 12, 1, 11, 
                                     3, 4, 5, 9, 8)) |> 
  left_join(r2r_param_mapping, by = "R2R_index") |> 
  left_join(LTO_param_mapping, by = "LTO_index") |> 
  # pulled over from run-calibration.R
  mutate("LTO_mins" = c(rep(-3.5,10),0,-3.5,rep(0,4), 0), # set 0 for lower bound for en route survival [11]
         "LTO_maxes" = c(rep(3.5,9),-1,rep(3.5,6), 3.5),
         "LTO_suggested" = c(-0.6558315, -3.4999845,  1.4933417, -3.0188308,  2.0000003,  0.7999889, -3.5000000, -0.1999996,
                             -3.4999920, -2.9839253,  3.4999976,  0.6466230,  0.0194795,  0.1000000,  0.3000000,  0.4820249,
                             0.0194795))

LTO_res <- readRDS("calibration/LTO_inputs/LTO_calib output_popSize100_LooseOC_long_time1770336187.38852.rds")
R2R_res <- readRDS("calibration/res-2026-02-06-LTO_comparison-pop100.rds")
#R2R_og_inputs <- readRDS("calibration/res-2026-02-09.rds")

LTO_results <- tibble("LTO_index" = 1:16) |> 
  mutate(LTO_value = unname(LTO_res@solution[LTO_index]))

R2R_results <- tibble("R2R_index" = 1:12) |> 
  mutate(R2R_value = unname(R2R_res@solution[R2R_index])) |> 
  left_join(map_params)

# R2R_results_og <- tibble("R2R_index" = 1:12) |> 
#   mutate(R2R_og_value = unname(R2R_og_inputs@solution[R2R_index])) |> 
#   left_join(map_params)

LTO_results |> 
  left_join(R2R_results,
            by = "LTO_index") |> 
  View()
