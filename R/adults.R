#' @title Adult Straying
#' @description Calculate the proportion of adults straying to non-natal streams to spawn
#' @details See \code{\link{params}} for details on parameter sources
#' @param wild Variable indicator of wild fish returning
#' @param natal_flow Variable describing proportion flows at tributary junctions coming from natal watershed in October
#' @param south_delta_watershed Variable indicator if watershed feeds into South Delta
#' @param cross_channel_gates_closed Variable describing number of days gates are closed for each month
#' @param prop_bay_trans Variable describing proportion transport to the bay
#' @param prop_delta_trans Variable describing proportion transport to the delta
#' @param .intercept Intercept
#' @param .wild Coefficient for \code{wild} variable
#' @param .natal_flow Coefficient for \code{natal_flow} variable
#' @param .cross_channel_gates_closed Coefficient for \code{cross_channel_gates_closed} variable
#' @param .prop_bay_trans Coefficient for \code{prop_bay_trans} variable
#' @param .prop_delta_trans Coefficient for \code{prop_delta_trans} variable
#' @source IP-117068
#' @export
adult_stray <- function(wild, natal_flow, south_delta_watershed, cross_channel_gates_closed,
                        prop_bay_trans = 0, prop_delta_trans = 0, # bring these up top level so we can have as params based on hatchery logic (can also manipualate props in R2R scenario)
                        .intercept = winterRunDSM::params$.adult_stray_intercept,
                        .wild = winterRunDSM::params$.adult_stray_wild,
                        .natal_flow = winterRunDSM::params$.adult_stray_natal_flow,
                        .cross_channel_gates_closed = winterRunDSM::params$.adult_stray_cross_channel_gates_closed,
                        .prop_bay_trans = winterRunDSM::params$.adult_stray_prop_bay_trans,
                        .prop_delta_trans = winterRunDSM::params$.adult_stray_prop_delta_trans){

  boot::inv.logit(
    .intercept +
    .wild * wild +
    .natal_flow * natal_flow +
    .cross_channel_gates_closed * south_delta_watershed * cross_channel_gates_closed +
    .prop_bay_trans * prop_bay_trans * ( 1 - wild) +
    .prop_delta_trans * prop_delta_trans * (1 - wild)
  )

}

#' @title Adult En Route Survival
#' @description Calculate adult survival en route to spawning grounds
#' @details See \code{\link{params}} for details on parameter sources
#' @param migratory_temp Variable representing proportion of migratory corridor temperature above  20°C
#' @param bypass_overtopped Indicator for bypass overtopped
#' @param ..surv_adult_enroute_int Intercept
#' @param .migratory_temp Coefficient for \code{migratory_temp} variable
#' @param .bypass_overtopped Coefficient for \code{bypass_overtopped} variable
#' @source IP-117068
#' @export
surv_adult_enroute <- function(migratory_temp, bypass_overtopped, 
                               ..surv_adult_enroute_int = winterRunDSM::params$..surv_adult_enroute_int,
                               .migratory_temp = winterRunDSM::params$.adult_en_route_migratory_temp,
                               .bypass_overtopped = winterRunDSM::params$.adult_en_route_bypass_overtopped) {

  pmax(boot::inv.logit(..surv_adult_enroute_int +
                       .migratory_temp * migratory_temp +
                       .bypass_overtopped * bypass_overtopped), 0)
}

#' Apply enroute survival to adult salmon
#'
#' This function calculates the survival of adult salmon during their enroute migration.
#'
#' @param year The year of the simulation.
#' @param month The starting month of the simulation.
#' @param adults A data frame containing information about adult salmon.
#' @param month_return_proportions A vector of proportions for each month.
#' @param gates_overtopped A three-dimensional array containing information about gates overtopped.
#' @param tisdale_bypass_watershed A parameter related to the Tisdale bypass watershed.
#' @param yolo_bypass_watershed A parameter related to the Yolo bypass watershed.
#' @param migratory_temperature_proportion_over_20 A matrix containing proportions of migratory temperatures over 20 degrees.
#' @param ..surv_adult_enroute_int Additional parameters for adult enroute survival.
#' @param .adult_en_route_migratory_temp Parameter for adult enroute survival related to migratory temperature.
#' @param .adult_en_route_bypass_overtopped Parameter for adult enroute survival related to bypass overtopped.
#' @param .adult_en_route_adult_harvest_rate Parameter for adult enroute survival related to adult harvest rate.
#' @param stochastic Logical indicating whether to include stochastic elements in the simulation.
#'
#' @return A list containing the initial number of adults, the proportion of natural adults, and the initial number of adults by month.
#'
#'
#' @export
apply_enroute_survival <- function(year,
                                   month,
                                   adults,
                                   month_return_proportions,
                                   gates_overtopped,
                                   tisdale_bypass_watershed,
                                   yolo_bypass_watershed,
                                   migratory_temperature_proportion_over_20,
                                   ..surv_adult_enroute_int,
                                   .adult_en_route_migratory_temp,
                                   .adult_en_route_bypass_overtopped,
                                   hatchery_release,
                                   stochastic) {
  # Do adults by month
  # Natural Adults
  natural_adults_by_month <- t(sapply(1:31, function(watershed) {
    if (stochastic) {
      rmultinom(1, rowSums(adults$natural)[watershed], month_return_proportions)
    } else {
      round(rowSums(adults$natural)[watershed] * month_return_proportions)
    }
  }))
  
  # Hatchery Adults
  hatchery_adults_by_month <- t(sapply(1:31, function(watershed) {
    if (stochastic) {
      rmultinom(1, rowSums(adults$hatchery)[watershed], month_return_proportions)
    } else {
      round(rowSums(adults$hatchery)[watershed] * month_return_proportions)
    }
  }))
  
  
  # for all years and months 10-12 there is always at least one true
  bypass_is_overtopped <- sapply(10:12, function(month) {
    
    tis <- gates_overtopped[month, year, 1] * tisdale_bypass_watershed
    yolo <- gates_overtopped[month, year, 2] * yolo_bypass_watershed
    as.logical(tis + yolo)
  })
  
  en_route_temps <- migratory_temperature_proportion_over_20[, 10:12]
  
  adult_en_route_surv <- pmin(sapply(1:3, function(month) {
    adult_en_route_surv <- surv_adult_enroute(migratory_temp = en_route_temps[,month],
                                              bypass_overtopped = bypass_is_overtopped[,month],
                                              ..surv_adult_enroute_int = ..surv_adult_enroute_int,
                                              .migratory_temp = .adult_en_route_migratory_temp,
                                              .bypass_overtopped = .adult_en_route_bypass_overtopped)
  }), 1)
  
  # Natural adults
  natural_adults_survived_to_spawning <- sapply(1:3, function(month) {
    if (stochastic) {
      rbinom(31, round(natural_adults_by_month[, month]), adult_en_route_surv[, month])
    } else {
      round(natural_adults_by_month[, month] * adult_en_route_surv[, month])
    }
  })
  # Hatchery
  # APPLY NAT ADULT REMOVAL RATE (APPLIED TO HATCHERY FISH instead of natural fish)
  hatchery_adults_survived_to_spawning <- sapply(1:3, function(month) {
    if (stochastic) {
      rbinom(31, round(hatchery_adults_by_month[, month]), adult_en_route_surv[, month])
    } else {
      round(hatchery_adults_by_month[, month] * adult_en_route_surv[, month])
    }
  })
  # Removes logic to remove hatchery fish if no
  if (sum(hatchery_release) > 0 ) {
    hatchery_adults_survived_to_spawning <- sapply(1:3, function(month) {
      if (stochastic) {
        rbinom(31, round(hatchery_adults_survived_to_spawning[, month]), (1 - natural_adult_removal_rate))
      } else {
        round(hatchery_adults_survived_to_spawning[, month] * (1 - natural_adult_removal_rate))
      }
    })
  }
  # TODO remove fish in non spawn regions
  surviving_natural_adults <- ifelse(rowSums(natural_adults_survived_to_spawning) < 0, 0,
                                     rowSums(natural_adults_survived_to_spawning))
  surviving_hatchery_adults <- ifelse(rowSums(hatchery_adults_survived_to_spawning) < 0, 0, rowSums(hatchery_adults_survived_to_spawning))
  init_adults <- surviving_natural_adults + surviving_hatchery_adults
  proportion_natural <- surviving_natural_adults / init_adults
  
  list(init_adults = round(init_adults),
       proportion_natural = replace(proportion_natural, is.nan(proportion_natural), 0)
  ) # TODO do we need inut adult by month
}

#' @title Adult Prespawn Survival
#' @description Calculate the adult prespawn survival
#' @details See \code{\link{params}} for details on parameter sources
#' @param deg_day Variable describing average degree days
#' @param .adult_prespawn_int Intercept
#' @param .deg_day Coefficient for \code{deg_day} variable
#' @source IP-117068
#' @export
surv_adult_prespawn <- function(deg_day,
                                .adult_prespawn_int = winterRunDSM::params$.adult_prespawn_int,
                                .deg_day = winterRunDSM::params$.adult_prespawn_deg_day){

  boot::inv.logit(.adult_prespawn_int + .deg_day * deg_day)
}

#' @title Prepare Beta-regression Data
#' @description
#' Create data frame of model data to be used in the beta-regression model for stray rates
#' @param hatchery name of hatchery to produce data for
#' @param type either in river or bay release
#' @param sim_year simulation year, used to calculate run year
#' @param flow_oct_nov the mean flow for Oct-Nov, use DSMflow::hatchery_oct_nov_flows
#' @param flow_apr_may the mean flow for Apr-May, use DSMflow::hatchery_apr_may_flows
#' @param released the total number of released hatchery fish
#' @param mean_PDO_return ...
#'
#' @details
#' Natural origin fish get a stray rate equal to Feather stray at distance = 0.
#'
#' @keywords internal
prepare_stray_model_data <- function(hatchery, type = c("natural", "hatchery"), sim_year,
                                     flow_oct_nov, flow_apr_may, releases, mean_PDO_return) {
  run_year <- sim_year + 1980
  flow_discrep <- flow_oct_nov - flow_apr_may
  return(
    tidyr::expand_grid(
      hatchery = hatchery,
      dist_hatch = normalize_with_params(
        if (type == "hatchery") c(fallRunDSM::hatchery_to_bay_distance[hatchery], 0) else  0,
        betareg_normalizing_context$dist_hatch$mean,
        betareg_normalizing_context$dist_hatch$sd),
      run_year = normalize_with_params(
        run_year, betareg_normalizing_context$run_year$mean, betareg_normalizing_context$run_year$sd
      ),
      age = normalize_with_params(2:5, betareg_normalizing_context$age$mean, betareg_normalizing_context$age$sd),
      Total_N = normalize_with_params(releases, betareg_normalizing_context$Total_N$mean, betareg_normalizing_context$Total_N$sd),
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