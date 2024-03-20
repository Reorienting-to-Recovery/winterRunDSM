#' Harvest Adults Function
#'
#' This function calculates the total adult salmon harvest based on various parameters.
#' This function is called within the larger model.R file before stray rates and prespawn survival is applied.
#'
#' @param adult_df A data frame containing information about adult salmon.
#' @param year The target year for the harvest calculation.
#' @param ocean_harvest_percentage The percentage of harvest from the ocean, same for all tributaries.
#' @param tributary_harvest_percentage The percentage of harvest from tributaries, can vary by tributary.
#' @param restrict_harvest_to_hatchery A logical vector indicating whether to restrict harvest to hatcheries for each tributary.
#' @param no_cohort_harvest_years A vector of years where no cohort harvest is allowed.
#' @param intelligent_crr_harvest A logical vector indicating whether to apply intelligent harvest based on Coho Retention Rate (CRR).
#'
#' @return A numeric value representing the total adult salmon harvest.
#'
#' @export

harvest_adults <- function(adult_df,
                           spawner_df,
                           year = year,
                           spawn_habitat,
                           terminal_hatchery_logic = c(T, F),
                           ocean_harvest_percentage, # same for all tribs
                           tributary_harvest_percentage, # can vary by trib
                           restrict_harvest_to_hatchery_ocean = c(F, T),
                           restrict_harvest_to_hatchery_trib = c(F, T),
                           no_cohort_harvest_years = NULL, # maybe we can get rid of this
                           intelligent_habitat_harvest = c(F, T),
                           intelligent_crr_harvest = c(F, T),
                           crr_scaling = 2
){
  # helper data ----------------------------------------------------------------
  return_prop <- matrix(c(.30, .60, .10, 0, .22, .47, .26, .05), nrow = 2, ncol = 4,
                        dimnames = list(c("hatchery", "natural"),
                                        c("V1", "V2", "V3", "V4")))
  
  min_spawn_habitat <- apply(spawn_habitat[ , 10:12, year], 1, min)
  capacity <- min_spawn_habitat / fallRunDSM::params$spawn_success_redd_size
  hab_capacity <- tibble::tibble(watershed = fallRunDSM::watershed_labels,
                                 habitat_capacity = capacity)
  
  watershed_order <- tibble::tibble(watershed = watershed_attributes$watershed,
                                    order = watershed_attributes$order)
  total_harvest <- rep(ocean_harvest_percentage, 31) + tributary_harvest_percentage
  # Apply harvest ---------------------------------------------------------------
  # Set no harvest adults based on no cohort years and no hatchery only harvest restrictions
  ocean_no_harvest_adults <- adult_df |>
    dplyr::filter(return_sim_year == year) |>
    dplyr::mutate(no_harvest = ifelse(sim_year %in% no_cohort_harvest_years, T, F), # Do we want to exclude hatchery here as well? follow up with technical team
                  no_harvest = ifelse(restrict_harvest_to_hatchery_ocean & origin == "natural", T, no_harvest),
                  age = return_sim_year - sim_year) |>
    dplyr::filter(no_harvest) |>
    dplyr::group_by(watershed, origin, age, sim_year, return_year, return_sim_year) |>
    dplyr::summarise(return_total = round(sum(return_total, na.rm = TRUE)),
                     adults_after_ocean_harvest = round(sum(return_total, na.rm = TRUE) * .9, 0)) |> # multipiles by .9 to remove 10 percent bycatch/hook mortality (high estimate)
    dplyr::ungroup()
  
  ocean_harvest_adults <- adult_df |>
    dplyr::filter(return_sim_year == year) |>
    dplyr::mutate(no_harvest = ifelse(sim_year %in% no_cohort_harvest_years, T, F), # Do we want to exclude hatchery here as well
                  no_harvest = ifelse(restrict_harvest_to_hatchery_ocean & origin == "natural", T, no_harvest)) |>
    dplyr::filter(!no_harvest) |>
    dplyr::rowwise() |>
    dplyr::mutate(age = return_sim_year - sim_year,
                  adults_after_ocean_harvest = dplyr::case_when(age == 2 ~ round((return_total) *
                                                                                   (1 - ocean_harvest_percentage), 0) ,
                                                                age == 3 ~ round(((return_total *
                                                                                     (1 - ocean_harvest_percentage)) *
                                                                                    (1 - ocean_harvest_percentage)), 0),
                                                                age == 4 ~ round((((return_total *
                                                                                      (1 - ocean_harvest_percentage)) *
                                                                                     (1 - ocean_harvest_percentage)) *
                                                                                    (1 - ocean_harvest_percentage)), 0),
                                                                age == 5 ~ round(((((return_total *
                                                                                       (1 - ocean_harvest_percentage)) *
                                                                                      (1 - ocean_harvest_percentage)) *
                                                                                     (1 - ocean_harvest_percentage)) *
                                                                                    (1 - ocean_harvest_percentage)), 0))) |>
    select(-no_harvest)
  
  # PREP TOTALS FOR TRIB HARVEST
  adults_after_ocean_harvest <- bind_rows(ocean_no_harvest_adults, ocean_harvest_adults)
  
  # TRIB
  trib_no_harvest_adults <- adults_after_ocean_harvest |>
    dplyr::mutate(no_harvest = ifelse(sim_year %in% no_cohort_harvest_years, T, F), # Do we want to exclude hatchery here as well? follow up with technical team
                  no_harvest = ifelse(restrict_harvest_to_hatchery_trib & origin == "natural", T, no_harvest),
                  age = return_sim_year - sim_year) |>
    dplyr::filter(no_harvest) |>
    dplyr::group_by(watershed, origin, age, sim_year, return_year, return_sim_year) |>
    dplyr::summarise(return_total = round(sum(return_total, na.rm = TRUE)),
                     remaining_adults = round(sum(return_total, na.rm = TRUE) * .99, 0), # multipiles by .9 to remove 1 percent bycatch/hook mortality in trib (much lower than in ocean)
                     actual_harvest = 0)
  
  trib_harvest_adults <- adults_after_ocean_harvest |>
    dplyr::left_join(hab_capacity, by = "watershed") |>
    dplyr::mutate(no_harvest = ifelse(sim_year %in% no_cohort_harvest_years, T, F), # Do we want to exclude hatchery here as well
                  no_harvest = ifelse(restrict_harvest_to_hatchery_trib & origin == "natural", T, no_harvest)) |>
    dplyr::filter(!no_harvest) |>
    dplyr::rowwise() |>
    dplyr::mutate(num_adults_required_after_harvest = ifelse(intelligent_crr_harvest,
                                                             round(spawner_df[watershed, sim_year] *
                                                                     return_prop[origin, return_year] * crr_scaling, 0), 0), # this is capturing total
                  age = return_sim_year - sim_year,
                  trib_harvest = tributary_harvest_percentage[watershed],
                  adults_after_trib_harvest = round((return_total * (1 - trib_harvest)), 0),
                  combined_harvest_number = (return_total - adults_after_ocean_harvest) + (return_total - adults_after_trib_harvest),
                  adults_after_harvest = return_total - combined_harvest_number,
                  utalize_crr_totals = ifelse(num_adults_required_after_harvest > adults_after_harvest & intelligent_crr_harvest, TRUE, FALSE),
                  utalize_hab_totals = ifelse(habitat_capacity > adults_after_harvest & intelligent_habitat_harvest, TRUE, FALSE),
                  remaining_adults = round(dplyr::case_when(utalize_crr_totals ~ min(num_adults_required_after_harvest, return_total),
                                                            utalize_hab_totals ~ min(habitat_capacity, return_total),
                                                            T ~ max(adults_after_harvest, 0))),
                  actual_harvest = round(dplyr::case_when(utalize_crr_totals ~ max(return_total - num_adults_required_after_harvest, 0),
                                                          utalize_hab_totals ~ max(return_total - habitat_capacity, 0),
                                                          T ~ min(combined_harvest_number, return_total)), 0)) |>
    dplyr::select(watershed, origin, age, remaining_adults, actual_harvest)
  
  
  adults_after_harvest <- dplyr::bind_rows(trib_no_harvest_adults,
                                           trib_harvest_adults)
  # Set to always allow - apply to no harvest and harvest adults, this will break crr logic (TODO talk with rene about that)
  # Recombine and synthesis
  hatchery_adults <- adults_after_harvest |>
    dplyr::filter(origin == "hatchery") |>
    dplyr::group_by(watershed, age) |>
    dplyr::summarise(remaining_adults = round(sum(remaining_adults, na.rm = TRUE), 0)) |>
    dplyr::ungroup() |>
    tidyr:: pivot_wider(names_from = age, values_from = remaining_adults) |>
    dplyr::left_join(watershed_order, by = "watershed") |>
    dplyr::arrange(order) |>
    dplyr::select(-watershed, -order) |>
    as.matrix()
  rownames(hatchery_adults) <- watershed_labels
  
  # harvested hatch_adults
  harvested_hatchery_adults <- adults_after_harvest |>
    dplyr::filter(origin == "hatchery") |>
    dplyr::group_by(watershed, age) |>
    dplyr::summarise(actual_harvest = round(sum(actual_harvest, na.rm = TRUE), 0)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = age, values_from = actual_harvest) |>
    dplyr::left_join(watershed_order, by = "watershed") |>
    dplyr::arrange(order) |>
    dplyr::select(-watershed, -order) |>
    as.matrix()
  rownames(harvested_hatchery_adults) <- watershed_labels
  
  # Nat adults remaining
  natural_adults <- adults_after_harvest |>
    dplyr::filter(origin == "natural") |>
    dplyr::group_by(watershed, age) |>
    dplyr::summarise(remaining_adults = round(sum(remaining_adults, na.rm = TRUE), 0)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = age, values_from = remaining_adults) |>
    dplyr::left_join(watershed_order, by = "watershed") |>
    dplyr::arrange(order) |>
    dplyr::select(-watershed, -order) |>
    as.matrix()
  rownames(natural_adults) <- watershed_labels
  
  # harvested natural
  harvested_natural_adults <-adults_after_harvest |>
    dplyr::filter(origin == "natural") |>
    dplyr::group_by(watershed, age) |>
    dplyr::summarise(actual_harvest = round(sum(actual_harvest, na.rm = TRUE), 0)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = age, values_from = actual_harvest) |>
    dplyr::left_join(watershed_order, by = "watershed") |>
    dplyr::arrange(order) |>
    dplyr::select(-watershed, -order) |>
    as.matrix()
  rownames(harvested_hatchery_adults) <- watershed_labels
  
  # Total adults (non confirming arrays so have to do differently)
  total_adults <- adults_after_harvest |>
    dplyr::group_by(watershed, age) |>
    dplyr::summarise(remaining_adults = round(sum(remaining_adults, na.rm = TRUE), 0)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = age, values_from = remaining_adults) |>
    dplyr::left_join(watershed_order, by = "watershed") |>
    dplyr::arrange(order) |>
    dplyr::select(-watershed, -order) |>
    as.matrix()
  rownames(total_adults) <- watershed_labels
  
  # prepare outpults
  total_adults <- total_adults
  hatchery_adults <- hatchery_adults
  natural_adults <- natural_adults
  proportion_natural <- natural_adults / total_adults
  
  # change nan to 0 for non spawn regions
  # recalculate pHOS
  list(hatchery_adults = replace(hatchery_adults, is.nan(hatchery_adults), 0),
       natural_adults = replace(natural_adults, is.nan(natural_adults), 0),
       harvested_hatchery_adults = harvested_hatchery_adults,
       harvested_natural_adults = harvested_natural_adults,
       proportion_natural = replace(proportion_natural, is.nan(proportion_natural), 0))
}