library(tidyverse)

# run_year = normalize_with_context(1990, data$run_year), # the run year, this should be normalized
run_year <-  output$returning_adults$sim_year

# age = normalize_with_context(3, data$age), # hathcery fish 60% return at age = 3

# Total_N = normalize_with_context(200000, data$Total_N), # obtained from ..params$hatchery_release
# rel_month = normalize_with_context(1:12, data$rel_month), # released are done all at once in the mode
# flow.1011 = normalize_with_context(200, data$flow.1011), # median flow for Oct
# flow_discrep = normalize_with_context(-200, data$flow_discrep), # difference between flow.1011 and avg flow apr-may
# mean_PDO_retn = normalize_with_context(0, data$mean_PDO_retn) # use the dataset they use cached intot he model, vary by year


d |> filter(origin == "hatchery",
            watershed == "Battle Creek") |>
  transmute(run = sim_year,
            age = return_sim_year - sim_year,
            Total_N = sum(fallRunDSM::r_to_r_baseline_params$hatchery_release),
            rel_month = 1,
            flow_1011 = 1220,
            flow_discrep = 0,
            mean_PDO_retn = 12)

d |> filter(origin == "hatchery",
            watershed == "Battle Creek")





# each row of the dataframe will be an input




m <- fallRunDSM::hatchery_stray_betareg


d |> filter(wat)

all_data <- read_csv("data-raw/stray-eda/wetransfer_sturrock-et-al/input_data/alldata_formodel_031918.csv")

hatchery_origin_new_data = expand_grid(
  hatchery = tolower(names(fallRunDSM::hatchery_to_bay_distance)[1]),
  dist_hatch = normalize_with_context(fallRunDSM::hatchery_to_bay_distance[1], all_data$dist_hatch),
  run_year = normalize_with_context(1 + 1980, all_data$run_year),
  age = normalize_with_context(2:5, all_data$age),
  Total_N = normalize_with_context(1200000, all_data$Total_N),
  rel_month = normalize_with_context(3, all_data$rel_month),
  flow.1011 = normalize_with_context(276.5, all_data$flow.1011),
  flow_discrep = normalize_with_context(0, all_data$flow_discrep),
  mean_PDO_retn = normalize_with_context(-7, all_data$mean_PDO_retn)
)

m3_per_s_to_ft3_per_s <- function(m3_per_s) {
  return(m3_per_s * 35.314666212661)
}


new_data_for_predictions <- function(hatchery, type = c("river", "bay"), run_year,
                                     flow_oct_nov, flow_apr_may, released, mean_PDO_return) {
  run_year <- run_year + 1980
  flow_discrep <- flow_oct_nov - flow_apr_may
  return(
    expand_grid(
      hatchery = hatchery,
      dist_hatch = normalize_with_params(
        ifelse(type == "bay", fallRunDSM::hatchery_to_bay_distance[hatchery], rep(0, 5)),
        betareg_normalizing_context$dist_hatch$mean,
        betareg_normalizing_context$dist_hatch$sd),
      run_year = normalize_with_params(
        run_year, betareg_normalizing_context$run_year$mean, betareg_normalizing_context$run_year$sd
      ),
      age = normalize_with_params(2:5, betareg_normalizing_context$age$mean, betareg_normalizing_context$age$sd),
      Total_N = normalize_with_params(released, betareg_normalizing_context$Total_N$mean, betareg_normalizing_context$Total_N$sd),
      rel_month = normalize_with_params(1, betareg_normalizing_context$rel_month$mean, betareg_normalizing_context$rel_month$sd),
      flow.1011 = normalize_with_params(flow_oct_nov, betareg_normalizing_context$flow.1011$mean, betareg_normalizing_context$flow.1011$sd),
      flow_discrep = normalize_with_params(
        flow_discrep,
        betareg_normalizing_context$flow_discrep$mean,
        betareg_normalizing_context$flow_discrep$sd),
      mean_PDO_retn = normalize_with_params(mean_PDO_return, betareg_normalizing_context$mean_PDO_retn$mean, betareg_normalizing_context$mean_PDO_retn$sd)
    )
  )
}


flow_1011 <- m3_per_s_to_ft3_per_s(mean(DSMflow::mean_flow$biop_itp_2018_2019["American River", 10:11, ]))
flow_45 <- m3_per_s_to_ft3_per_s(mean(DSMflow::mean_flow$biop_itp_2018_2019["American River", 4:5, ]))

mean(monthly_mean_pdo[monthly_mean_pdo$month == 1, ]$PDO)
hatchery_origin_new_data <- new_data_for_predictions(
  "nimbus",
  type = "bay",
  run_year = 1,
  flow_oct_nov = flow_1011,
  flow_apr_may = flow_45,
  released = sum(fallRunDSM::r_to_r_baseline_params$hatchery_release[hatchery_to_watershed_lookup["nimbus"], ]),
  mean_PDO_return = mean(monthly_mean_pdo[monthly_mean_pdo$month == 1, ]$PDO))

hatchery <- "nimbus"
run_year <- 1983
released <- fallRunDSM::r_to_r_baseline_params$hatchery_release
flow_oct_nov <- fallRunDSM::r_to_r_baseline_params$flows_oct_nov["American River", "1983"]
flow_apr_may <- fallRunDSM::r_to_r_baseline_params$flows_apr_may["American River", "1983"]
mean_PDO_return <- fallRunDSM::monthly_mean_pdo |> filter(year == 1983, month == 1) |> pull(PDO)
flow_discrep <- flow_oct_nov - flow_apr_may

d <- expand_grid(
  hatchery = hatchery,
  dist_hatch = normalize_with_params(
    50,
    betareg_normalizing_context$dist_hatch$mean,
    betareg_normalizing_context$dist_hatch$sd),
  run_year = normalize_with_params(
    1983, betareg_normalizing_context$run_year$mean, betareg_normalizing_context$run_year$sd
  ),
  age = normalize_with_params(2:5, betareg_normalizing_context$age$mean, betareg_normalizing_context$age$sd),
  Total_N = normalize_with_params(sum(released["American River",]), betareg_normalizing_context$Total_N$mean, betareg_normalizing_context$Total_N$sd),
  rel_month = normalize_with_params(1, betareg_normalizing_context$rel_month$mean, betareg_normalizing_context$rel_month$sd),
  flow.1011 = normalize_with_params(flow_oct_nov, betareg_normalizing_context$flow.1011$mean, betareg_normalizing_context$flow.1011$sd),
  flow_discrep = normalize_with_params(
    flow_discrep,
    betareg_normalizing_context$flow_discrep$mean,
    betareg_normalizing_context$flow_discrep$sd),
  mean_PDO_retn = normalize_with_params(mean_PDO_return, betareg_normalizing_context$mean_PDO_retn$mean, betareg_normalizing_context$mean_PDO_retn$sd)
)


predict(winterRunDSM::hatchery_stray_betareg, newdata = d)

