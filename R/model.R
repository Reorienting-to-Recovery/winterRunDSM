#' @title Winter Run Chinook Model
#' @description Winter Run Chinook life cycle model used for Reorienting to Recovery's Structured
#' Decision Making Process
#' @param scenario Model inputs, can be modified to test management actions
#' @param mode The mode to run model in. Can be \code{"seed"}, \code{"simulate"}, \code{"calibrate"}
#' @param seeds The default value is NULL runs the model in seeding mode,
#' returning a 31 by 25 matrix with the first four years of seeded adults. This
#' returned value can be fed into the model again as the value for the seeds argument
#' @param ..params Parameters for model and submodels. Defaults to \code{fallRunDSM::\code{\link{params}}}.
#' @param stochastic \code{TRUE} \code{FALSE} value indicating if model should be run stochastically. Defaults to \code{FALSE}.
#' @source IP-117068
#' @examples
#' winter_run_seeds <- winterRunDSM::winter_run_model(mode = "seed")
#' winterRunDSM::winter_run_model(scenario = DSMscenario::scenarios$ONE,
#'                            mode = "simulate",
#'                            seeds = winter_run_seeds)
#' @export
winter_run_model <- function(scenario = NULL,
                             mode = c("seed", "simulate", "calibrate"),
                             seeds = NULL,
                             ..params = winterRunDSM::r_to_r_baseline_params,
                             stochastic = FALSE,
                             delta_surv_inflation = FALSE){
  
  mode <- match.arg(mode)
  
  if (mode == "simulate") {
    if (is.null(scenario)) {
      # the do nothing scenario to force habitat degradation
      scenario <- DSMscenario::scenarios$NO_ACTION
    }
    
    habitats <- list(
      spawning_habitat = ..params$spawning_habitat,
      inchannel_habitat_fry = ..params$inchannel_habitat_fry,
      inchannel_habitat_juvenile = ..params$inchannel_habitat_juvenile,
      floodplain_habitat = ..params$floodplain_habitat,
      weeks_flooded = ..params$weeks_flooded
    )
    
    # Apply spawn decay multiplier
    scenario_data <- DSMscenario::load_scenario(scenario,
                                                habitat_inputs = habitats,
                                                species = DSMscenario::species$SPRING_RUN,
                                                spawn_decay_rate = ..params$spawn_decay_rate,
                                                rear_decay_rate = ..params$rear_decay_rate,
                                                spawn_decay_multiplier = ..params$spawn_decay_multiplier,
                                                stochastic = stochastic)
    
    ..params$spawning_habitat <- scenario_data$spawning_habitat
    ..params$inchannel_habitat_fry <- scenario_data$inchannel_habitat_fry
    ..params$inchannel_habitat_juvenile <- scenario_data$inchannel_habitat_juvenile
    ..params$floodplain_habitat <- scenario_data$floodplain_habitat
    ..params$weeks_flooded <- scenario_data$weeks_flooded
    
    ..params$spawning_habitat <- scenario_data$spawning_habitat
    ..params$inchannel_habitat_fry <- scenario_data$inchannel_habitat_fry
    ..params$inchannel_habitat_juvenile <- scenario_data$inchannel_habitat_juvenile
    ..params$floodplain_habitat <- scenario_data$floodplain_habitat
    ..params$weeks_flooded <- scenario_data$weeks_flooded
  }
  
  if (mode == "calibrate") {
    scenario_data <- list(
      survival_adjustment = matrix(1, nrow = 31, ncol = 21,
                                   dimnames = list(DSMscenario::watershed_labels,
                                                   1980:2000)))
  }
  simulation_length <- switch(mode,
                              "seed" = 6,
                              "simulate" = 20,
                              "calibrate" = 19)
  output <- list(
    
    # SIT METRICS
    spawners = matrix(0, nrow = 31, ncol = 20, dimnames = list(winterRunDSM::watershed_labels, 1:20)),
    juvenile_biomass = matrix(0, nrow = 31, ncol = 20, dimnames = list(winterRunDSM::watershed_labels, 1:20)),
    # proportion_natural = matrix(NA_real_, nrow = 31, ncol = 20, dimnames = list(winterRunDSM::watershed_labels, 1:20))
    north_delta_fish = data.frame(),
    
    # R2R METRICS
    returning_adults = tibble::tibble(),
    adults_in_ocean = matrix(0, nrow = 31, ncol = 20, dimnames = list(winterRunDSM::watershed_labels, 1:20)),
    juveniles = data.frame(),
    juveniles_at_chipps = data.frame(),
    proportion_natural_at_spawning = matrix(0, nrow = 31, ncol = 20, dimnames = list(winterRunDSM::watershed_labels, 1:20)),
    proportion_natural_juves_in_tribs = matrix(0, nrow = 31, ncol = 20, dimnames = list(winterRunDSM::watershed_labels, 1:20)),
    phos = matrix(0, nrow = 31, ncol = 20, dimnames = list(winterRunDSM::watershed_labels, 1:20)),
    harvested_adults = data.frame()
  )
  
  if (mode == 'calibrate') {
    calculated_adults <- matrix(0, nrow = 31, ncol = 30)
  }
  
  adults <- switch(mode,
                   "seed" = winterRunDSM::adult_seeds,
                   "simulate" = seeds$adults,
                   "calibrate" = seeds,
  )
  
  # TODO do we want the yearling logic from springRunDSM?

  for (year in 1:simulation_length) {
    # TODO added this list from springRunDSM - keep?
    adults_in_ocean <- numeric(31)
    lower_mid_sac_fish <- matrix(0, nrow = 20, ncol = 4, dimnames = list(winterRunDSM::watershed_labels[1:20], winterRunDSM::size_class_labels))
    lower_sac_fish <- matrix(0, nrow = 27, ncol = 4, dimnames = list(winterRunDSM::watershed_labels[1:27], winterRunDSM::size_class_labels))
    upper_mid_sac_fish <- matrix(0, nrow = 15, ncol = 4, dimnames = list(winterRunDSM::watershed_labels[1:15], winterRunDSM::size_class_labels))
    sutter_fish <- matrix(0, nrow = 15, ncol = 4, dimnames = list(winterRunDSM::watershed_labels[1:15], winterRunDSM::size_class_labels))
    yolo_fish <- matrix(0, nrow = 20, ncol = 4, dimnames = list(winterRunDSM::watershed_labels[1:20], winterRunDSM::size_class_labels))
    san_joaquin_fish <- matrix(0, nrow = 4, ncol = 4, dimnames = list(winterRunDSM::watershed_labels[28:31], winterRunDSM::size_class_labels))
    north_delta_fish <- matrix(0, nrow = 23, ncol = 4, dimnames = list(winterRunDSM::watershed_labels[1:23], winterRunDSM::size_class_labels))
    south_delta_fish <- matrix(0, nrow = 31, ncol = 4, dimnames = list(winterRunDSM::watershed_labels, winterRunDSM::size_class_labels))
    juveniles_at_chipps <- matrix(0, nrow = 31, ncol = 4, dimnames = list(winterRunDSM::watershed_labels, winterRunDSM::size_class_labels))
    natural_adults <- round(adults * (1 - ..params$proportion_hatchery))
    
    avg_ocean_transition_month <- ocean_transition_month(stochastic = stochastic) # 2
    
    # R2R logic updates #
    # R2R logic to add fish size as an input -----------------------------------
    default_hatch_age_dist <- tibble::tibble(watershed = winterRunDSM::watershed_labels,
                                             prop_2 = rep(.3, 31),
                                             prop_3 = rep(.6, 31),
                                             prop_4 = rep(.1, 31),
                                             prop_5 = rep(0, 31))
    default_nat_age_dist <- tibble::tibble(watershed = winterRunDSM::watershed_labels,
                                           prop_2 = rep(.22, 31),
                                           prop_3 = rep(.47, 31),
                                           prop_4 = rep(.26, 31),
                                           prop_5 = rep(.05, 31))
    if (year %in% c(1:6)) {
      hatch_age_dist <- default_hatch_age_dist
      natural_age_dist <- default_nat_age_dist
    } else {
      hatch_age_dist <- output$returning_adults |>
        dplyr::filter(return_sim_year == year, origin == "hatchery") |>
        dplyr::mutate(age = return_sim_year - sim_year,
                      return_total = ifelse(is.nan(return_total), 0, return_total)) |>
        dplyr::group_by(watershed, age) |>
        dplyr::summarise(total = sum(return_total, na.rm = TRUE)) |>
        tidyr::pivot_wider(names_from = age, values_from = total) |>
        dplyr::mutate(total = `2` + `3` + `4`,
                      prop_2 = ifelse(total == 0, .3, `2`/total), #need to allow for straying fish even if total pop was initially 0
                      prop_3 = ifelse(total == 0, .6, `3`/total),
                      prop_4 = ifelse(total == 0, .1, `4`/total),
                      prop_5 = 0) |>
        dplyr::select(-c(`2`, `3`, `4`, total))
      
      # find natural age distribution
      natural_age_dist <- output$returning_adults |>
        dplyr::filter(return_sim_year == year, origin == "natural") |>
        dplyr::mutate(age = return_sim_year - sim_year,
                      return_total = ifelse(is.nan(return_total), 0, return_total)) |>
        dplyr::group_by(watershed, age) |>
        dplyr::summarise(total = sum(return_total, na.rm = TRUE)) |>
        tidyr::pivot_wider(names_from = age, values_from = total) |>
        dplyr::mutate(total = `2` + `3` + `4` + `5`,
                      prop_2 = ifelse(total == 0, .22, `2`/total), #need to allow for straying fish even if total pop was initially 0
                      prop_3 = ifelse(total == 0, .47, `3`/total),
                      prop_4 = ifelse(total == 0, .26, `4`/total),
                      prop_5 = ifelse(total == 0, .05, `5`/total)) |>
        dplyr::select(-c(`2`, `3`, `4`, `5`, total))
    }
    # Begin adult logic --------------------------------------------------------
    # In seed and calibrate just use adults
    # Do not need to apply harvest, or survival because starting with GrandTab values
    if (mode %in% c("seed", "calibrate")) {
      adult_index <- ifelse(mode == "seed", 1, year)
      annual_adults <- adults[, adult_index]
      annual_adults_hatch_removed <- if (stochastic) {
        rbinom(n = 31,
               size = adults_by_month,
               prob = 1 - natural_adult_removal_rate)
      } else {
        annual_adults * (1 - natural_adult_removal_rate)
      }
      spawners = list(init_adults = round(annual_adults_hatch_removed),
                      proportion_natural = 1 - winterRunDSM::params$proportion_hatchery)
    }
    
    if(mode == "simulate") {
      # TODO brought in harvest logic from springRunDSM - ok?
      # HARVEST ----------------------------------------------------------------
      # Incidental harvest percentage 
      hatch_adults <- adults[, year] * seeds$proportion_hatchery 
      adults_after_harvest <- hatch_adults * (1 - .1) # assume 10% hooking mortality 
      hatch_after_harvest_by_age <- round(unname(adults_after_harvest) * as.matrix(default_hatch_age_dist[2:5]))
      row.names(hatch_after_harvest_by_age) = winterRunDSM::watershed_labels
      colnames(hatch_after_harvest_by_age) = c(2, 3, 4, 5)
      harvested_hatchery_adults <- hatch_adults - adults_after_harvest
      
      # Incidental harvest percentage 
      nat_adults <- adults[, year] * (1 - seeds$proportion_hatchery)
      natural_adults_after_harvest <- nat_adults * (1 - .1) # assume 10% hooking mortality 
      natural_adults_by_age <- round(unname(natural_adults_after_harvest) * as.matrix(default_nat_age_dist[2:5]))
      harvested_natural_adults <- nat_adults - natural_adults_after_harvest
      row.names(natural_adults_by_age) = winterRunDSM::watershed_labels
      colnames(natural_adults_by_age) = c(2, 3, 4, 5)
      
      adults_after_harvest <- list(hatchery_adults = hatch_after_harvest_by_age,
                                   natural_adults = natural_adults_by_age,
                                   harvested_hatchery_adults = harvested_hatchery_adults,
                                   harvested_natural_adults = harvested_natural_adults)
      
      natural_adult_harvest <- sum(adults_after_harvest$harvested_natural_adults, na.rm = TRUE)
      hatchery_adult_harvest <- sum(adults_after_harvest$harvested_hatchery_adults, na.rm = TRUE)
      harvest <- tibble::tibble(year = year,
                                hatchery_harvest = hatchery_adult_harvest,
                                natural_harvest = natural_adult_harvest,
                                total_harvest = hatchery_harvest + natural_harvest)
      output$harvested_adults <- dplyr::bind_rows(output$harvested_adults, harvest)
      
      # STRAY --------------------------------------------------------------------
      adults_after_stray <- apply_straying(year, adults_after_harvest$natural_adults,
                                           adults_after_harvest$hatchery_adults,
                                           total_releases = ..params$hatchery_release,
                                           release_month = 1,
                                           flows_oct_nov = ..params$flows_oct_nov,
                                           flows_apr_may = ..params$flows_apr_may,
                                           winterRunDSM::monthly_mean_pdo)
      
      # APPLY EN ROUTE SURVIVAL ---------------------------------------------------
      spawners <- apply_enroute_survival(year,
                                         adults = adults_after_stray,
                                         month_return_proportions = ..params$month_return_proportions,
                                         gates_overtopped = ..params$gates_overtopped,
                                         tisdale_bypass_watershed = ..params$tisdale_bypass_watershed,
                                         yolo_bypass_watershed = ..params$yolo_bypass_watershed,
                                         migratory_temperature_proportion_over_20 = ..params$migratory_temperature_proportion_over_20,
                                         ..surv_adult_enroute_int = ..params$..surv_adult_enroute_int,
                                         .adult_en_route_migratory_temp = ..params$.adult_en_route_migratory_temp,
                                         .adult_en_route_bypass_overtopped = ..params$.adult_en_route_bypass_overtopped,
                                         stochastic = stochastic)
    }
    
    init_adults <- spawners$init_adults
    
    output$spawners[ , year] <- init_adults
    # # For use in the r2r metrics ---------------------------------------------
    # TODO fix handling for PHOS on non spawn and 0 fish watersheds
    phos <- ifelse(is.na(1 - spawners$proportion_natural), 0, 1 - spawners$proportion_natural)
    if (mode == "simulate" & year > 5 & (sum(..params$hatchery_release) + sum(..params$hatchery_releases_at_chipps)) == 0) {
      natural_proportion_with_renat <- rep(1, 31)
      names(natural_proportion_with_renat) <- fallRunDSM::watershed_labels
    } else if (year > 3){
      phos_diff_two_years <- ifelse(is.na(phos - output$phos[, (year - 2)]), 0, phos - output$phos[, (year - 2)])
      phos_diff_last_year <- ifelse(is.na(phos - output$phos[, (year - 1)]), 0, phos - output$phos[, (year - 1)])
      if (any(phos_diff_two_years < 0 & phos_diff_last_year < 0)) {
        perc_diff <- (phos - output$phos[, (year - 2)]) /  output$phos[, (year - 2)]
        renaturing_tribs <- which(phos_diff_two_years < 0 & phos_diff_last_year < 0)
        proportion_renaturing <- ifelse(names(phos_diff_two_years) %in% names(renaturing_tribs), abs(phos_diff_two_years), 0)
        total_renaturing_in_year <- spawners$init_adults * (1 - spawners$proportion_natural) * proportion_renaturing
        total_natural_with_renaturing <- total_renaturing_in_year + spawners$init_adults * spawners$proportion_natural
        natural_proportion_with_renat <-  total_natural_with_renaturing / spawners$init_adults
        natural_proportion_with_renat <- ifelse(is.nan(natural_proportion_with_renat), 0, natural_proportion_with_renat)
      } else {
        natural_proportion_with_renat <-  spawners$proportion_natural
      }
    } else {
      natural_proportion_with_renat <-  spawners$proportion_natural
    }
    
    output$proportion_natural_at_spawning[ , year] <- natural_proportion_with_renat
    output$phos[ , year] <- 1 - natural_proportion_with_renat
    # end R2R metric logic -----------------------------------------------------
    
    egg_to_fry_surv <- surv_egg_to_fry(
      proportion_natural = natural_proportion_with_renat, # update to new prop nat (renaturing logic applied),
      scour = ..params$prob_nest_scoured,
      temperature_effect = ..params$mean_egg_temp_effect,
      .proportion_natural = ..params$.surv_egg_to_fry_proportion_natural,
      .scour = ..params$.surv_egg_to_fry_scour,
      ..surv_egg_to_fry_int = ..params$..surv_egg_to_fry_int
    )
    
    min_spawn_habitat <- apply(..params$spawning_habitat[ , 3:6, year], 1, min)
    
    # Migatory accumulated degree days and associated prespawn survival 
    accumulated_degree_days <- cbind(march = rowSums(..params$degree_days[ , 3:6, year]),
                                     april = rowSums(..params$degree_days[ , 4:6, year]),
                                     may = rowSums(..params$degree_days[ , 5:6, year]),
                                     june = ..params$degree_days[ , 6, year])
    
    average_degree_days <- apply(accumulated_degree_days, 1, weighted.mean, ..params$month_return_proportions)
    
    # R2R: above and below degree days
    average_degree_days_abv_dam <- apply(cbind(march = rowSums(..params$degree_days_abv_dam[ , 3:6, year]),
                                               april = rowSums(..params$degree_days_abv_dam[ , 4:6, year]),
                                               may = rowSums(..params$degree_days_abv_dam[ , 5:6, year]),
                                               june = ..params$degree_days_abv_dam[ , 6, year]), 1, weighted.mean, ..params$month_return_proportions)
    
    prespawn_survival <- surv_adult_prespawn(average_degree_days,
                                             ..surv_adult_prespawn_int = ..params$..surv_adult_prespawn_int,
                                             .deg_day = ..params$.adult_prespawn_deg_day)
    # R2R: above and below prespawn
    prespawn_survival_abv_dam <- .95 #TODO confirm with tech team, use max fall run surv
    
    # Apply SR pools logic, this reflects prespawn survival during holding period 
    # R2R: init adults split into above and below dam and apply prespawn separately and 
    # then add them back together 
    init_adults <- if (stochastic) {
      blw_dam <- rbinom(31, round(init_adults), (prespawn_survival * (1 - ..params$above_dam_spawn_proportion))) 
      abv_dam <- rbinom(31, round(init_adults), (prespawn_survival * ..params$above_dam_spawn_proportion))
      blw_dam + abv_dam
    } else {
      blw_dam <- round((1 - ..params$above_dam_spawn_proportion) * init_adults * prespawn_survival, 0) 
      abv_dam <- round(..params$above_dam_spawn_proportion * init_adults * prespawn_survival_abv_dam, 0)
      blw_dam + abv_dam
    }
    
    # R2R: abv dam degree days
    average_degree_days_abv_dam <- rowSums(..params$degree_days_abv_dam[ , 7:10, year]) * (1 - holding_split) + 
      rowSums(..params$degree_days_abv_dam[ , 7:9, year]) * holding_split
    
    prespawn_survival <- surv_adult_prespawn(average_degree_days,
                                             ..surv_adult_prespawn_int = ..params$..surv_adult_prespawn_int,
                                             .deg_day = ..params$.adult_prespawn_deg_day)
    
    # R2R: above and below prespawn
    prespawn_survival_abv_dam <- .95 #TODO confirm with tech team, use max fall run surv
    
    # calculate juveniles 
    juveniles <- spawn_success(escapement = init_adults,
                               proportion_natural = natural_proportion_with_renat, # R2R ADDS NEW PARAM
                               hatchery_age_distribution = hatch_age_dist, # R2R ADDS NEW PARAM
                               natural_age_distribution = natural_age_dist, # R2R ADDS NEW PARAM
                               fecundity_lookup = ..params$fecundity_lookup, # R2R ADDS NEW PARAM
                               adult_prespawn_survival = prespawn_survival, 
                               adult_prespawn_survival_abv_dam = prespawn_survival_abv_dam, # R2R: ADDS NEW PARAM for abv dam
                               abv_dam_spawn_proportion = ..params$above_dam_spawn_proportion, # R2R: ADDS NEW PARAM for abv dam
                               egg_to_fry_survival = egg_to_fry_surv,
                               prob_scour = ..params$prob_nest_scoured,
                               spawn_habitat = min_spawn_habitat,
                               sex_ratio = ..params$spawn_success_sex_ratio,
                               redd_size = ..params$spawn_success_redd_size,
                               fecundity = ..params$spawn_success_fecundity,
                               stochastic = stochastic)
  
    # R2R hatchery logic -------------------------------------------------------
    # Currently adds only on major hatchery rivers (American, Battle, Feather, Merced, Moke)
    # Add all as large fish
    total_juves_pre_hatchery <- rowSums(juveniles)
    natural_juveniles <- total_juves_pre_hatchery  * natural_proportion_with_renat
    total_juves_pre_hatchery <- rowSums(juveniles)
    # TODO add ability to vary release per year
    juveniles <- juveniles + ..params$hatchery_release
    
    # Create new prop natural including hatch releases that we can use to apply to adult returns
    proportion_natural_juves_in_tribs <- natural_juveniles / rowSums(juveniles)
    output$proportion_natural_juves_in_tribs[ , year] <- proportion_natural_juves_in_tribs
    
    # # For use in the r2r metrics ---------------------------------------------
    d <- data.frame(juveniles)
    colnames(d) <- c("s", "m", "l", "vl")
    d$watershed <- winterRunDSM::watershed_labels
    d <- d |> tidyr::pivot_longer(names_to = "size", values_to = "juveniles", -watershed)
    d$year <- year
    output$juveniles <- dplyr::bind_rows(output$juveniles, d)
    
    # end R2R metric -----------------------------------------------------------
    
    fish_list <- lapply(1:8, function(i) list(juveniles = juveniles,
                                              lower_mid_sac_fish = lower_mid_sac_fish,
                                              lower_sac_fish = lower_sac_fish,
                                              upper_mid_sac_fish = upper_mid_sac_fish,
                                              sutter_fish = sutter_fish,
                                              yolo_fish = yolo_fish,
                                              san_joaquin_fish = san_joaquin_fish,
                                              north_delta_fish = north_delta_fish,
                                              south_delta_fish = south_delta_fish,
                                              juveniles_at_chipps = juveniles_at_chipps,
                                              adults_in_ocean = adults_in_ocean))
    
    names(fish_list) <- c(paste0("route_", 1:8, "_fish"))
    
    stopifnot(nrow(juveniles) == 31)
    
    # Create new prop natural including hatch releases that we can use to apply to adult returns
    proportion_natural_juves_in_tribs <- natural_juveniles / rowSums(juveniles)
    output$proportion_natural_juves_in_tribs[ , year] <- proportion_natural_juves_in_tribs
    
    # # For use in the r2r metrics ---------------------------------------------
    d <- data.frame(juveniles)
    colnames(d) <- c("s", "m", "l", "vl")
    d$watershed <- fallRunDSM::watershed_labels
    d <- d |> tidyr::pivot_longer(names_to = "size", values_to = "juveniles", -watershed)
    d$year <- year
    output$juveniles <- dplyr::bind_rows(output$juveniles, d)
    # end R2R metric -----------------------------------------------------------
    growth_temps <- ..params$avg_temp
    growth_temps[which(growth_temps > 28)] <- 28
    
    for (month in c(9:12, 1:5)) {
      if (month %in% 1:5) juv_dynamics_year <- year + 1 else juv_dynamics_year <- year
      
      growth_rates_ic <- get_growth_rates(growth_temps[,month, juv_dynamics_year],
                                          prey_density = ..params$prey_density)
      
      growth_rates_fp <- get_growth_rates(growth_temps[,month, juv_dynamics_year],
                                          prey_density = ..params$prey_density,
                                          floodplain = TRUE)
      
      growth_rates_delta <- get_growth_rates(..params$avg_temp_delta[month, juv_dynamics_year,],
                                             prey_density = ..params$prey_density_delta)
      
      habitat <- get_habitat(juv_dynamics_year, month,
                             inchannel_habitat_fry = ..params$inchannel_habitat_fry,
                             inchannel_habitat_juvenile = ..params$inchannel_habitat_juvenile,
                             floodplain_habitat = ..params$floodplain_habitat,
                             sutter_habitat = ..params$sutter_habitat,
                             yolo_habitat = ..params$yolo_habitat,
                             delta_habitat = ..params$delta_habitat)
      
      rearing_survival <- get_rearing_survival(juv_dynamics_year, month,
                                               survival_adjustment = scenario_data$survival_adjustment,
                                               mode = mode,
                                               avg_temp = ..params$avg_temp,
                                               avg_temp_delta = ..params$avg_temp_delta,
                                               prob_strand_early = ..params$prob_strand_early,
                                               prob_strand_late = ..params$prob_strand_late,
                                               proportion_diverted = ..params$proportion_diverted,
                                               total_diverted = ..params$total_diverted,
                                               delta_proportion_diverted = ..params$delta_proportion_diverted,
                                               delta_total_diverted = ..params$delta_total_diverted,
                                               weeks_flooded = ..params$weeks_flooded,
                                               prop_high_predation = ..params$prop_high_predation,
                                               contact_points = ..params$contact_points,
                                               delta_contact_points = ..params$delta_contact_points,
                                               delta_prop_high_predation = ..params$delta_prop_high_predation,
                                               ..surv_juv_rear_int= ..params$..surv_juv_rear_int,
                                               .surv_juv_rear_contact_points = ..params$.surv_juv_rear_contact_points,
                                               ..surv_juv_rear_contact_points = ..params$..surv_juv_rear_contact_points,
                                               .surv_juv_rear_prop_diversions = ..params$.surv_juv_rear_prop_diversions,
                                               ..surv_juv_rear_prop_diversions = ..params$..surv_juv_rear_prop_diversions,
                                               .surv_juv_rear_total_diversions = ..params$.surv_juv_rear_total_diversions,
                                               ..surv_juv_rear_total_diversions = ..params$..surv_juv_rear_total_diversions,
                                               ..surv_juv_bypass_int = ..params$..surv_juv_bypass_int,
                                               ..surv_juv_delta_int = ..params$..surv_juv_delta_int,
                                               .surv_juv_delta_contact_points = ..params$.surv_juv_delta_contact_points,
                                               ..surv_juv_delta_contact_points = ..params$..surv_juv_delta_contact_points,
                                               .surv_juv_delta_total_diverted = ..params$.surv_juv_delta_total_diverted,
                                               ..surv_juv_delta_total_diverted = ..params$..surv_juv_delta_total_diverted,
                                               .surv_juv_rear_avg_temp_thresh = ..params$.surv_juv_rear_avg_temp_thresh,
                                               .surv_juv_rear_high_predation = ..params$.surv_juv_rear_high_predation,
                                               .surv_juv_rear_stranded = ..params$.surv_juv_rear_stranded,
                                               .surv_juv_rear_medium = ..params$.surv_juv_rear_medium,
                                               .surv_juv_rear_large = ..params$.surv_juv_rear_large,
                                               .surv_juv_rear_floodplain = ..params$.surv_juv_rear_floodplain,
                                               .surv_juv_bypass_avg_temp_thresh = ..params$.surv_juv_bypass_avg_temp_thresh,
                                               .surv_juv_bypass_high_predation = ..params$.surv_juv_bypass_high_predation,
                                               .surv_juv_bypass_medium = ..params$.surv_juv_bypass_medium,
                                               .surv_juv_bypass_large = ..params$.surv_juv_bypass_large,
                                               .surv_juv_bypass_floodplain = ..params$.surv_juv_bypass_floodplain,
                                               .surv_juv_delta_avg_temp_thresh = ..params$.surv_juv_delta_avg_temp_thresh,
                                               .surv_juv_delta_high_predation = ..params$.surv_juv_delta_high_predation,
                                               .surv_juv_delta_prop_diverted = ..params$.surv_juv_delta_prop_diverted,
                                               .surv_juv_delta_medium = ..params$.surv_juv_delta_medium,
                                               .surv_juv_delta_large = ..params$.surv_juv_delta_large,
                                               min_survival_rate = ..params$min_survival_rate,
                                               stochastic = stochastic)
      
      migratory_survival <- get_migratory_survival(juv_dynamics_year, month,
                                                   cc_gates_prop_days_closed = ..params$cc_gates_prop_days_closed,
                                                   freeport_flows = ..params$freeport_flows,
                                                   vernalis_flows = ..params$vernalis_flows,
                                                   stockton_flows = ..params$stockton_flows,
                                                   vernalis_temps = ..params$vernalis_temps,
                                                   prisoners_point_temps = ..params$prisoners_point_temps,
                                                   CVP_exports = ..params$CVP_exports,
                                                   SWP_exports = ..params$SWP_exports,
                                                   upper_sacramento_flows = ..params$upper_sacramento_flows,
                                                   delta_inflow = ..params$delta_inflow,
                                                   avg_temp_delta = ..params$avg_temp_delta,
                                                   avg_temp = ..params$avg_temp,
                                                   delta_proportion_diverted = ..params$delta_proportion_diverted,
                                                   ..surv_juv_outmigration_sj_int = ..params$..surv_juv_outmigration_sj_int,
                                                   .surv_juv_outmigration_san_joaquin_medium = ..params$.surv_juv_outmigration_san_joaquin_medium,
                                                   .surv_juv_outmigration_san_joaquin_large = ..params$.surv_juv_outmigration_san_joaquin_large,
                                                   min_survival_rate = ..params$min_survival_rate,
                                                   stochastic = stochastic)
      
      if (delta_surv_inflation == TRUE){
        migratory_survival$bay_delta <- min(1, migratory_survival$bay_delt * 2)
        migratory_survival$sutter <-  min(1, migratory_survival$sutter * 2)
        migratory_survival$yolo <- pmin(1, migratory_survival$yolo * 2)
        migratory_survival$delta[which(migratory_survival$delta * 2 > 1)] <- 1
      }
      
      # TODO keep movement hypothesis weights?
      # hypothesis are layed out as follows:
      # 1. base filling + base
      # 2. base filling + snow
      # 3. base filling + genetics
      # 4. base filling + temperature
      # 5. dens filling + base
      # 6. dens filling + snow
      # 7. dens filling + genetics
      # 8. dens filling + temperature
      
      if (..params$movement_hypo_weights[1] != 0){
        fish_list$route_1_fish <- juvenile_month_dynamic(
          fish_list$route_1_fish,
          year = year, month = month,
          rearing_survival = rearing_survival,
          migratory_survival = migratory_survival,
          habitat = habitat, ..params = ..params,
          avg_ocean_transition_month = avg_ocean_transition_month,
          stochastic = stochastic,
          ic_growth = growth_rates_ic,
          fp_growth = growth_rates_fp,
          delta_growth = growth_rates_delta
        )
      }
      
      if (..params$movement_hypo_weights[2] != 0) {
        fish_list$route_2_fish <- juvenile_month_dynamic(
          fish_list$route_2_fish,
          year = year, month = month,
          rearing_survival = rearing_survival,
          migratory_survival = migratory_survival,
          habitat = habitat, ..params = ..params,
          avg_ocean_transition_month = avg_ocean_transition_month,
          stochastic = stochastic,
          ic_growth = growth_rates_ic,
          fp_growth = growth_rates_fp,
          delta_growth = growth_rates_delta,
          movement_fn = fallRunDSM::snow_globe_movement,
          movement_months = 1:2,
          movement_args = list(freeport_flow = ..params$freeport_flows[month, year],
                               vernalis_flow = ..params$vernalis_flows[month, year],
                               threshold = 1000, p_leave = 0.3, stochastic = stochastic)
        )
      }
      if (..params$movement_hypo_weights[3] != 0) {
        fish_list$route_3_fish <- juvenile_month_dynamic(
          fish_list$route_3_fish,
          year = year, month = month,
          rearing_survival = rearing_survival,
          migratory_survival = migratory_survival,
          habitat = habitat, ..params = ..params,
          avg_ocean_transition_month = avg_ocean_transition_month,
          stochastic = stochastic,
          ic_growth = growth_rates_ic,
          fp_growth = growth_rates_fp,
          delta_growth = growth_rates_delta,
          movement_fn = fallRunDSM::genetic_movement,
          movement_months = 1:2,
          movement_args = list(p_leave = 0.25, stochastic = stochastic)
        )
      }
      if (..params$movement_hypo_weights[4] != 0) {
        fish_list$route_4_fish <- juvenile_month_dynamic(
          fish_list$route_4_fish,
          year = year, month = month,
          rearing_survival = rearing_survival,
          migratory_survival = migratory_survival,
          habitat = habitat, ..params = ..params,
          avg_ocean_transition_month = avg_ocean_transition_month,
          stochastic = stochastic,
          ic_growth = growth_rates_ic,
          fp_growth = growth_rates_fp,
          delta_growth = growth_rates_delta,
          movement_fn = fallRunDSM::temperature_movement,
          movement_months = 1:2,
          movement_args = list(movement_month = 3, movement_temp = 15, stochastic = stochastic)
        )
      }
      if (..params$movement_hypo_weights[5] != 0) {
        fish_list$route_5_fish <- juvenile_month_dynamic(
          fish_list$route_5_fish,
          year = year, month = month,
          rearing_survival = rearing_survival,
          migratory_survival = migratory_survival,
          habitat = habitat, ..params = ..params,
          avg_ocean_transition_month = avg_ocean_transition_month,
          stochastic = stochastic,
          ic_growth = growth_rates_ic,
          fp_growth = growth_rates_fp,
          delta_growth = growth_rates_delta,
          filling_fn = fallRunDSM::fill_natal_dens_depend, # filling using density dependence
          filling_args = list(up_to_size_class = 2,
                              ..floodplain_capacity = ..params$..floodplain_capacity,
                              ..habitat_capacity = ..params$..habitat_capacity),
          filling_regional_fn = fallRunDSM::fill_regional_dens_depend,
          filling_regional_args = list(up_to_size_class = 3, ..floodplain_capacity = ..params$..floodplain_capacity,
                                       ..habitat_capacity = ..params$..habitat_capacity)
        )
      }
      if (..params$movement_hypo_weights[6] != 0) {
        fish_list$route_6_fish <- juvenile_month_dynamic(
          fish_list$route_6_fish,
          year = year, month = month,
          rearing_survival = rearing_survival,
          migratory_survival = migratory_survival,
          habitat = habitat, ..params = ..params,
          avg_ocean_transition_month = avg_ocean_transition_month,
          stochastic = stochastic,
          ic_growth = growth_rates_ic,
          fp_growth = growth_rates_fp,
          delta_growth = growth_rates_delta,
          filling_fn = fallRunDSM::fill_natal_dens_depend, # filling using density dependence
          filling_args = list(up_to_size_class = 2, ..floodplain_capacity = ..params$..floodplain_capacity,
                              ..habitat_capacity = ..params$..habitat_capacity),
          filling_regional_fn = fallRunDSM::fill_regional_dens_depend,
          filling_regional_args = list(up_to_size_class = 3, ..floodplain_capacity = ..params$..floodplain_capacity,
                                       ..habitat_capacity = ..params$..habitat_capacity),
          movement_fn = fallRunDSM::snow_globe_movement,
          movement_months = 1:2,
          movement_args = list(freeport_flow = ..params$freeport_flows[month, year],
                               vernalis_flow = ..params$vernalis_flows[month, year],
                               threshold = 1000, p_leave = 0.3, stochastic = stochastic)
        )
      }
      if (..params$movement_hypo_weights[7] != 0) {
        fish_list$route_7_fish <- juvenile_month_dynamic(
          fish_list$route_7_fish,
          year = year, month = month,
          rearing_survival = rearing_survival,
          migratory_survival = migratory_survival,
          habitat = habitat, ..params = ..params,
          avg_ocean_transition_month = avg_ocean_transition_month,
          stochastic = stochastic,
          ic_growth = growth_rates_ic,
          fp_growth = growth_rates_fp,
          delta_growth = growth_rates_delta,
          filling_fn = fallRunDSM::fill_natal_dens_depend, # filling using density dependence
          filling_args = list(up_to_size_class = 2, ..floodplain_capacity = ..params$..floodplain_capacity,
                              ..habitat_capacity = ..params$..habitat_capacity),
          filling_regional_fn = fallRunDSM::fill_regional_dens_depend,
          filling_regional_args = list(up_to_size_class = 3, ..floodplain_capacity = ..params$..floodplain_capacity,
                                       ..habitat_capacity = ..params$..habitat_capacity),
          movement_fn = fallRunDSM::genetic_movement,
          movement_months = 1:2,
          movement_args = list(p_leave = 0.25, stochastic = stochastic)
        )
      }
      if (..params$movement_hypo_weights[8] != 0) {
        fish_list$route_8_fish <- juvenile_month_dynamic(
          fish_list$route_8_fish,
          year = year, month = month,
          rearing_survival = rearing_survival,
          migratory_survival = migratory_survival,
          habitat = habitat, ..params = ..params,
          avg_ocean_transition_month = avg_ocean_transition_month,
          stochastic = stochastic,
          ic_growth = growth_rates_ic,
          fp_growth = growth_rates_fp,
          delta_growth = growth_rates_delta,
          filling_fn = fallRunDSM::fill_natal_dens_depend, # filling using density dependence
          filling_args = list(up_to_size_class = 2, ..floodplain_capacity = ..params$..floodplain_capacity,
                              ..habitat_capacity = ..params$..habitat_capacity),
          filling_regional_fn = fallRunDSM::fill_regional_dens_depend,
          filling_regional_args = list(up_to_size_class = 3, ..floodplain_capacity = ..params$..floodplain_capacity,
                                       ..habitat_capacity = ..params$..habitat_capacity),
          movement_fn = fallRunDSM::temperature_movement,
          movement_months = 1:2,
          movement_args = list(movement_month = 3, movement_temp = 15, stochastic = stochastic)
        )
      }
      if (FALSE) {
        fish_1_df <- create_fish_df(fish_df = fish_list$route_1_fish, month = month, year = year)
        fish_2_df <- create_fish_df(fish_df = fish_list$route_2_fish, month = month, year = year)
        fish_3_df <- create_fish_df(fish_df = fish_list$route_3_fish, month = month, year = year)
        fish_4_df <- create_fish_df(fish_df = fish_list$route_4_fish, month = month, year = year)
        fish_5_df <- create_fish_df(fish_df = fish_list$route_5_fish, month = month, year = year)
        fish_6_df <- create_fish_df(fish_df = fish_list$route_6_fish, month = month, year = year)
        fish_7_df <- create_fish_df(fish_df = fish_list$route_7_fish, month = month, year = year)
        fish_8_df <- create_fish_df(fish_df = fish_list$route_8_fish, month = month, year = year)
        
        output$north_delta_fish <- dplyr::bind_rows(
          output$north_delta_fish,
          fish_1_df,
          fish_2_df,
          fish_3_df,
          fish_4_df,
          fish_5_df,
          fish_6_df,
          fish_7_df,
          fish_8_df
        )
      }
      # # For use in the r2r metrics ---------------------------------------------
      juveniles_at_chipps <-
        ..params$movement_hypo_weights[1] * fish_list$route_1_fish$juveniles_at_chipps +
        ..params$movement_hypo_weights[2] * fish_list$route_2_fish$juveniles_at_chipps +
        ..params$movement_hypo_weights[3] * fish_list$route_3_fish$juveniles_at_chipps +
        ..params$movement_hypo_weights[4] * fish_list$route_4_fish$juveniles_at_chipps +
        ..params$movement_hypo_weights[5] * fish_list$route_5_fish$juveniles_at_chipps +
        ..params$movement_hypo_weights[6] * fish_list$route_6_fish$juveniles_at_chipps +
        ..params$movement_hypo_weights[7] * fish_list$route_7_fish$juveniles_at_chipps +
        ..params$movement_hypo_weights[8] * fish_list$route_8_fish$juveniles_at_chipps
      
      d <- data.frame(juveniles_at_chipps)
      colnames(d) <- c("s", "m", "l", "vl")
      d$watershed <- fallRunDSM::watershed_labels
      d <- d |> tidyr::pivot_longer(names_to = "size",
                                    values_to = "juveniles_at_chipps", -watershed)
      d$year <- year
      d$month <- month
      output$juveniles_at_chipps <- dplyr::bind_rows(output$juveniles_at_chipps, d)
      # end R2R metric -----------------------------------------------------------
      adults_in_ocean <-
        ..params$movement_hypo_weights[1] * fish_list$route_1_fish$adults_in_ocean +
        ..params$movement_hypo_weights[2] * fish_list$route_2_fish$adults_in_ocean +
        ..params$movement_hypo_weights[3] * fish_list$route_3_fish$adults_in_ocean +
        ..params$movement_hypo_weights[4] * fish_list$route_4_fish$adults_in_ocean +
        ..params$movement_hypo_weights[5] * fish_list$route_5_fish$adults_in_ocean +
        ..params$movement_hypo_weights[6] * fish_list$route_6_fish$adults_in_ocean +
        ..params$movement_hypo_weights[7] * fish_list$route_7_fish$adults_in_ocean +
        ..params$movement_hypo_weights[8] * fish_list$route_8_fish$adults_in_ocean
      
    } # end month loop
    
    output$juvenile_biomass[ , year] <- juveniles_at_chipps %*% winterRunDSM::params$mass_by_size_class
    
    
    # Updated logic here for R2R so that natural adults and hatchery adults return separately
    natural_adults_returning <- t(sapply(1:31, function(i) {
      if (stochastic) {
        rmultinom(1, (adults_in_ocean[i]), prob = c(.22, .47, .26, .05))
      } else {
        round((adults_in_ocean[i])* c(.22, .47, .26, .05))
      }
    })) * output$proportion_natural_juves_in_tribs[ , year]
    
    natural_adults_returning[is.na(natural_adults_returning)] = NaN
    
    # R2R release at chipps logic -----------------------------------------------
    bay_releases <- sweep(..params$hatchery_release, MARGIN=2, ..params$hatchery_release_proportion_bay, "*")
    hatchery_releases_at_chipps <- ocean_entry_success(migrants = bay_releases,
                                                       month = 7, # set to final month
                                                       avg_ocean_transition_month = avg_ocean_transition_month,
                                                       .ocean_entry_success_length = ..params$.ocean_entry_success_length,
                                                       ..ocean_entry_success_int = ..params$..ocean_entry_success_int,
                                                       .ocean_entry_success_months = ..params$.ocean_entry_success_months,
                                                       stochastic = stochastic)
    
    hatchery_adults_returning <- t(sapply(1:31, function(i) {
      if (stochastic) {
        rmultinom(1, (adults_in_ocean[i]), prob = c(.30, .60, .10)) * (1 - output$proportion_natural_juves_in_tribs[ , year][i]) +
          rmultinom(1, (hatchery_releases_at_chipps[i]), prob = c(.30, .60, .10))
      } else {
        round((adults_in_ocean[i] * c(.30, .60, .10)) * (1 - output$proportion_natural_juves_in_tribs[, year][i])) +
          round((hatchery_releases_at_chipps[i]) * c(.30, .60, .10))}
    }))
    
    hatchery_adults_returning[is.na(hatchery_adults_returning)] = NaN
    
    # # For use in the r2r metrics ---------------------------------------------
    # Create adult returning dataframes
    colnames(natural_adults_returning) <- c("V1", "V2", "V3", "V4")
    colnames(hatchery_adults_returning) <- c("V1", "V2", "V3")
    
    output$returning_adults <- dplyr::bind_rows(
      output$returning_adults,
      natural_adults_returning |>
        dplyr::as_tibble(.name_repair = "universal") |>
        dplyr::mutate(watershed = watershed_labels,
                      sim_year = year,
                      origin = "natural") |>
        tidyr::pivot_longer(V1:V4, names_to = "return_year", values_to = "return_total") |>
        dplyr::mutate(return_sim_year = readr::parse_number(return_year) + 1 + as.numeric(sim_year)),
      hatchery_adults_returning |>
        dplyr::as_tibble(.name_repair = "universal") |>
        dplyr::mutate(watershed = watershed_labels,
                      sim_year = year,
                      origin = "hatchery") |>
        tidyr::pivot_longer(V1:V3, names_to = "return_year", values_to = "return_total") |>
        dplyr::mutate(return_sim_year = readr::parse_number(return_year) + 1 + as.numeric(sim_year))
    )
    # End R2R metric logic -----------------------------------------------------
    
    # distribute returning adults for future spawning
    if (mode == "calibrate") {
      calculated_adults[1:31, (year + 2):(year + 5)] <- calculated_adults[1:31, (year + 2):(year + 5)] + natural_adults_returning
      calculated_adults[1:31, (year + 2):(year + 4)] <- calculated_adults[1:31, (year + 2):(year + 4)] + hatchery_adults_returning
      calculated_adults[is.na(calculated_adults)] = 0
    } else {
      adults[1:31, (year + 2):(year + 5)] <- adults[1:31, (year + 2):(year + 5)] + natural_adults_returning
      adults[1:31, (year + 2):(year + 4)] <- adults[1:31, (year + 2):(year + 4)] + hatchery_adults_returning
      adults[is.na(adults)] = 0
    }
    
  } # end year for loop
  
  if (mode == "seed") {
    return(list(adults = adults[ , 6:30],
                proportion_hatchery = 1 - proportion_natural_juves_in_tribs))
  } else if (mode == "calibrate") {
    return(calculated_adults[, 6:20]) #TODO QUESTION FOR EMANUEL - IS 6 - 20 enough, do we need more years
  }
  # Removed spawn change / viability info NOT USED FOR R2R Logic

  return(output)
  
}

