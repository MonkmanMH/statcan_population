#
# from Kyle Walker
# {tidycensus}
# https://github.com/walkerke/tidycensus/blob/master/R/utils.R
#

as_dot_density <- function(
    input_data,
    value,
    values_per_dot,
    group = NULL,
    erase_water = FALSE,
    area_threshold = NULL,
    water_year = 2020
) {
  
  # Ensure that terra package is installed
  if (!"terra" %in% installed.packages()) {
    stop("`as_dot_density()` uses the terra package to generate dots. Please install terra first then re-run.", call. = FALSE)
  }
  
  # If erase water is selected, erase water area from the polygons first
  if (erase_water) {
    
    if (is.null(area_threshold)) {
      area_threshold = 0.75
    }
    
    input_data <- tigris::erase_water(input_data,
                                      area_threshold = area_threshold,
                                      year = water_year)
  } else {
    if (!is.null(area_threshold)) {
      message("`area_threshold` is to be used when erasing water from input polygons; ignoring as `erase_water` is currently `FALSE`.")
    }
  }
  
  # If group is identified, iterate over the groups, shuffle, and combine
  if (!is.null(group)) {
    
    groups <- unique(input_data[[group]])
    
    output_dots <- purrr::map_dfr(groups, function(g) {
      
      group_data <- input_data[input_data[[group]] == g, ]
      
      group_data %>%
        terra::vect() %>%
        terra::dots(field = value,
                    size = values_per_dot) %>%
        sf::st_as_sf()
    }) %>%
      dplyr::slice_sample(prop = 1)
    
  } else {
    output_dots <- input_data %>%
      terra::vect() %>%
      terra::dots(field = value,
                  size = values_per_dot) %>%
      sf::st_as_sf()
  }
  
  # Sometimes, a strange dot gets thrown in.  Ensure this doesn't get returned.
  output_dots <- sf::st_filter(output_dots, input_data)
  
  return(output_dots)
  
  
}



