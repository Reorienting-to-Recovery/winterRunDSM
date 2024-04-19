#' Apply Straying
#' @title Apply Straying
#' @description Function to apply adult upstream straying
#' @param year Simulation year
#' @param natural_adults Number of natural adults
#' @param hatchery_adults Number of hatchery adults
#' @param total_releases Total hatchery fish released from..params$hatchery_release,
#' @param release_month Hatchery release month, defaults to 1,
#' @param flows_oct_nov Mean flows from October and November from ..params$flows_oct_nov,
#' @param flows_apr_may Mean flows from April and May ..params$flows_apr_may,
#' @param monthly_mean_pdo Mean pdo from winterRunDSM::monthly_mean_pdo,
#' @export
apply_straying <- function(year, natural_adults, hatchery_adults, total_releases,
                           release_month, flows_oct_nov, flows_apr_may, monthly_mean_pdo) {
  
  # TODO just use the values that are generated as part of the hatchery calculation for the natural stray rates, this way we only make the calculation once
  # calculate the straying
  natural_stray_rates <- compute_adult_stray_rates(type = "natural",
                                                   sim_year = year,
                                                   total_releases = total_releases,
                                                   released_month = release_month,
                                                   flows_oct_nov = flows_oct_nov,
                                                   flows_apr_may = flows_apr_may,
                                                   mean_pdo_return = monthly_mean_pdo)
  
  hatchery_stray_rates <- compute_adult_stray_rates(type = "hatchery",
                                                    sim_year = year,
                                                    total_releases = total_releases,
                                                    released_month = release_month,
                                                    flows_oct_nov = flows_oct_nov,
                                                    flows_apr_may = flows_apr_may,
                                                    mean_pdo_return = monthly_mean_pdo)
  
  natural_ages <- ncol(natural_adults)
  hatchery_ages <- ncol(hatchery_adults)
  # apply stray to natural
  strayed_natural_adults <- round(natural_adults * natural_stray_rates$natural[,natural_ages])
  
  
  # apply stray to hatchery
  # prop of in river vs prop in bay releases
  in_bay_releases <- hatchery_adults * winterRunDSM::hatchery_release_proportion_bay
  in_river_releases <- hatchery_adults * (1 - winterRunDSM::hatchery_release_proportion_bay)
  
  # Give upper sac same stray rates as battle 
  hatchery_stray_rates$release_river["Upper Sacramento River",] <- hatchery_stray_rates$release_river["Battle Creek",]
  hatchery_stray_rates$release_bay["Upper Sacramento River",] <- hatchery_stray_rates$release_bay["Battle Creek",]
  
  strayed_hatchery_adults <- ceiling(in_bay_releases * hatchery_stray_rates$release_bay[,hatchery_ages] + in_river_releases * hatchery_stray_rates$release_river[,hatchery_ages])
  strayed_hatchery_adults[is.nan(strayed_hatchery_adults)] <- 0
  strayed_natural_adults[is.nan(strayed_natural_adults)] <- 0
  
  
  # Stop straying to non spawn destinations
  straying_destinations[16, ] <- rep(0, 6) # uper mid sac
  straying_destinations[17, ] <- rep(0, 6) # sutter bypass
  straying_destinations[21, ] <- rep(0, 6) # lower mid sac
  straying_destinations[22, ] <- rep(0, 6) # yolo bypass
  straying_destinations[24, ] <- rep(0, 6) # lower sac
  straying_destinations[31, ] <- rep(0, 6) # san joaquin
  # hatchery origin
  hatchery_strays <- lapply(1:hatchery_ages, function(age) {
    
    age_strays <- lapply(winterRunDSM::hatchery_to_watershed_lookup, function(w) {
      rmultinom(n = 1, size = strayed_hatchery_adults[w, age], prob = straying_destinations[, winterRunDSM::watershed_to_hatchery_lookup[w]])
    })
    
    allocated <- matrix(0, ncol = 1, nrow = 31)
    for (i in seq_along(winterRunDSM::hatchery_to_watershed_lookup)) {
      allocated <- allocated + age_strays[[i]]
    }
    
    return(allocated)
    
  })
  
  if (hatchery_ages > 1) {
    hatchery_strays_allocated <- do.call(cbind, hatchery_strays)
  } else {
    hatchery_strays_allocated <- hatchery_strays[[1]]
  }
  
  hatchery_adults_after_stray <- hatchery_adults - strayed_hatchery_adults + hatchery_strays_allocated
  
  # Stop straying to non spawn destinations
  straying_destinations[16, ] <- rep(0, 6) # uper mid sac
  straying_destinations[17, ] <- rep(0, 6) # sutter bypass
  straying_destinations[21, ] <- rep(0, 6) # lower mid sac
  straying_destinations[22, ] <- rep(0, 6) # yolo bypass
  straying_destinations[24, ] <- rep(0, 6) # lower sac
  straying_destinations[31, ] <- rep(0, 6) # san joaquin
  
  # natural origin
  natural_strays <- lapply(1:natural_ages, function(age) {
    
    age_strays <- lapply(winterRunDSM::watershed_labels, function(w) {
      rmultinom(n = 1, size = strayed_natural_adults[w, age], prob = straying_destinations[, "default"])
    })
    
    allocated <- matrix(0, ncol = 1, nrow = 31)
    for (i in 1:31) {
      allocated <- allocated + age_strays[[i]]
    }
    
    return(allocated)
    
  })
  
  if (natural_ages > 1) {
    natural_strays_allocated <- do.call(cbind, natural_strays)
  } else {
    natural_strays_allocated <- natural_strays[[1]]
  }
  
  natural_adults_after_stray <- natural_adults - strayed_natural_adults + natural_strays_allocated
  
  # TODO what prop natural to report when 0 fish present at the watershed
  proportion_natural <- rowSums(natural_adults_after_stray, na.rm = TRUE) /
    (rowSums(natural_adults_after_stray, na.rm = TRUE) + rowSums(hatchery_adults_after_stray, na.rm = TRUE))
  
  return(list(
    natural = natural_adults_after_stray,
    hatchery = hatchery_adults_after_stray,
    proportion_natural = proportion_natural
  ))
  
}

#' @title Adult Straying for Hatchery Origin Fish
#' @description
#' Calculates stray rates for all hatchery originating fish.
#' @param release_type river or bay representing where the hatchery fish were released
#' @param run_year year of run
#' @param age age of fish
#' @param released total number of fish released at hatchery
#' @param flow_oct_nov the median flow for October and November
#' @param flow_apr_may the median flow for April and May
#' @param mean_pdo_return PDO return
#' @export
#' @md
compute_adult_stray_rates <- function(type = c("natural", "hatchery"), sim_year,
                                      total_releases, released_month, flows_oct_nov, flows_apr_may, mean_pdo_return) {
  
  
  # create "newdata" for each of the hatcheries to be used in the prediction
  new_data <- purrr::map_df(names(winterRunDSM::hatchery_to_watershed_lookup), function(x) {
    
    # prepare initial data
    w <- winterRunDSM::hatchery_to_watershed_lookup[x]
    flow_10_11 <- flows_oct_nov[w, sim_year]
    flow_4_5 <- flows_apr_may[w, sim_year]
    releases <- sum(total_releases[w, ])
    pdo <- mean_pdo_return[mean_pdo_return$year == sim_year + 1979 & mean_pdo_return$month == 1, ]$PDO
    
    prepare_stray_model_data(hatchery = x, type = type, sim_year = sim_year, flow_oct_nov = flow_10_11, flow_apr_may = flow_4_5,
                             releases = releases, mean_PDO_return = pdo)
  })
  predictions <- betareg::predict(fallRunDSM::hatchery_stray_betareg, newdata = new_data)
  new_data$prediction <- predictions
  age_unorm <- if (type == "natural") c(rep(2:5, 5)) else c(rep(2:5, 10))
  stray_type <-  if (type == "natural") "natural" else rep(rep(c("release bay", "release river"),
                                                               each = 4), 5)
  
  stray_rates <- new_data |> transmute(
    sim_year = sim_year,
    watershed = winterRunDSM::hatchery_to_watershed_lookup[hatchery],
    age = age_unorm,
    stray_type = stray_type,
    stray_rate = prediction
  )
  
  stray_rates_to_matrix(stray_rates, type = type)
}

#' @title Normalize data with context data
#' @description transform data to be normalized given data to calculate mean and standard deviation from
#' @keywords internal
normalize_with_context <- function(x, context_data) {
  (x - mean(context_data, na.rm = TRUE))/sd(context_data, na.rm = TRUE)
}

#' @title Normalize data with known params
#' @description transform data to be normalized given the mean and standard deviation from the data
#' @keywords internal
normalize_with_params <- function(x, mean_val, sd_val) {
  (x - mean_val)/sd_val
}

#' @title Creates matrix from stray rates
#' @description transforms stray rates from betareg output into matrix
#' @keywords internal
stray_rates_to_matrix <- function(data, type) {
  out <- vector(mode = "list")
  out$natural <- NA
  out$release_bay <- NA
  out$release_river <- NA
  
  if (type == "natural") {
    # natural origin fish
    out$natural <- data |>
      dplyr::filter(watershed == "American River", stray_type == "natural") |>
      tidyr::pivot_wider(names_from = "age", values_from = "stray_rate") |>
      dplyr::slice(rep(1:dplyr::n(), each = 31)) |>
      dplyr::select(`2`:`5`) |>
      as.matrix() |>
      `row.names<-`(watershed_labels)
  } else {
    # rates dataframe to the matrix for bay hatchery
    out$release_bay <- data |>
      dplyr::filter(stray_type == "release bay") |>
      tidyr::pivot_wider(values_from = "stray_rate", names_from = "age") |>
      dplyr::select(-sim_year, -stray_type) |>
      dplyr::right_join(dplyr::select(watershed_attributes, watershed, order), by = "watershed") |>
      dplyr::arrange(order) |>
      dplyr::mutate(across(everything(), \(x) ifelse(is.na(x), 0, x))) |>
      dplyr::select(-watershed, -order) |>
      as.matrix() |>
      `row.names<-`(watershed_labels)
    
    # rates for river release fish
    out$release_river <- data |>
      dplyr::filter(stray_type == "release river") |>
      tidyr::pivot_wider(values_from = "stray_rate", names_from = "age") |>
      dplyr::select(-sim_year, -stray_type) |>
      dplyr::right_join(dplyr::select(watershed_attributes, watershed, order), by = "watershed") |>
      dplyr::arrange(order) |>
      dplyr::mutate(across(everything(), \(x) ifelse(is.na(x), 0, x))) |>
      dplyr::select(-watershed, -order) |>
      as.matrix() |>
      `row.names<-`(watershed_labels)
  }
  
  return(out)
}