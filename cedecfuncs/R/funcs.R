
#' Calculate Distance
#'
#' This function calculates the distance between two points specified by their
#' latitude and longitude.
#'
#' @param lat1 Latitude of the first point.
#' @param lon1 Longitude of the first point.
#' @param lat2 Latitude of the second point.
#' @param lon2 Longitude of the second point.
#' @return Distance in miles.
#' @export
calculate_distance <- function(lat1, lon1, lat2, lon2) {
  dist <- distGeo(matrix(c(lon1, lat1), nrow = 1), matrix(c(lon2, lat2), nrow = 1))
  return(dist/1609.344)
}

#' Get Census Code Vector
#'
#' This function calculates the census code for a vector of latitude and longitude pairs.
#'
#' @param lat A vector of latitudes.
#' @param lon A vector of longitudes.
#' @return A vector of census codes corresponding to the input latitude and longitude pairs.
#' If a latitude or longitude within a pair is NA the census code will be NA
#' @export
get_census_code_vec <- function(lat, lon) {
  if(length(lat) != length(lon)){
    stop("Latitude and longitude vectors are not the same length")
  }

  # Initialize vector to store census tract information
  census_code <- character(length(lat))

  # Loop through each pair of latitude and longitude
  for (i in seq_along(lat)) {
    # Check if latitude or longitude is NA
    if (!is.na(lat[i]) && !is.na(lon[i])) {
      census_code[i] <- call_geolocator_latlon(lat[i],lon[i])
    } else {
      # If latitude or longitude is NA, set census tract to NA
      census_code[i] <- NA
    }
  }

  return(census_code)
}


#' Get Traffic Density
#'
#' This function retrieves the traffic density (free flow speed) for a given latitude and longitude using the TomTom Traffic API.
#'
#' @param latitude A value representing the latitude of the location.
#' @param longitude A value representing the longitude of the location.
#' @param apiKey A string representing the API key for the TomTom Traffic API.
#' @return A value representing the free flow speed at the specified location. If the API request fails, it returns an error message.
#' @export
get_traffic_density <- function(latitude, longitude, apiKey) {
  url <- paste0("https://api.tomtom.com/traffic/services/4/flowSegmentData/absolute/10/json?point=",
                latitude, ",", longitude, "&key=", apiKey)

  response <- GET(url)

  if (http_status(response)$category == "Success") {
    traffic_data <- content(response, "parsed")

    freeFlowSpeed <- traffic_data$flowSegmentData$freeFlowSpeed

    return(freeFlowSpeed)
  } else {
    return("Error, could not retrieve data")
  }
}


#' Search Organization (Single)
#'
#' This function searches for a single organization based on a search term and address using the Yelp Fusion API.
#'
#' @param term A string specifying the search term (e.g., business name, category).
#' @param address A string representing the location address.
#' @param yelp_api_key the Yelp API key for authentication.
#' @param lat Optional value representing the latitude of the location. If this parameter is provided, address should not be provided.
#' @param lon Optional value representing the longitude of the location. If this parameter is provided, address should not be provided.
#' @return A vector containing the review count, rating, and phone number of the found organization.
#' If the organization is not found or an error occurs, NULL is returned.
#' @export
search_organization_single <- function(term, address, yelp_api_key,lat=NULL,lon=NULL) {
  # Define the base URL for Yelp API
  base_url <- "https://api.yelp.com/v3/businesses/search"

  if(!is.null(address)){
    params <- list(
      term = term,
      location = address,
      limit = 1
    )
  }
  else if(!is.null(lat) & !is.null(lon)){
    params <- list(
      term = term,
      latitude = lat,
      longitude = lon,
      limit = 1
    )
  }
  else{
    stop("An address must be provided")
  }

  response <- httr::GET(base_url, query = params, httr::add_headers("Authorization" = paste("Bearer", yelp_api_key)))

  if (httr::status_code(response) == 200) {
    data <- httr::content(response, "text") %>%
      jsonlite::fromJSON(flatten = TRUE)

    if (is.null(data$businesses)) {
      print(paste("Not found"))
      return(NULL)
    }

    return(c(data$businesses$review_count, data$businesses$rating, data$businesses$phone))
  } else {
    print(paste("Error searching for", term, ":", httr::content(response, "text")))
    return(NULL)
  }
}

#' Search Organization (Multiple)
#'
#' This function searches for a organizations matching a search term near a location
#'
#' @param term A string specifying the search term (e.g., business name, category).
#' @param address A string representing the location address.
#' @param yelp_api_key the Yelp API key for authentication.
#' @param lat Optional value representing the latitude of the location. If this parameter is provided, address should not be provided.
#' @param lon Optional value representing the longitude of the location. If this parameter is provided, address should not be provided.
#' @param radius Optional value representing a maximum search radius in miles
#' @return A data table of information about each matching search result
#' @export
search_organization_multiple <- function(term, address, yelp_api_key,num_responses,lat=NULL,lon=NULL,radius=NULL) {
  # Define the base URL for Yelp API
  base_url <- "https://api.yelp.com/v3/businesses/search"
  if(!is.null(address)){
    params <- list(
      term = term,
      location = address,
      # Adjust this to change how many businesses are fetched on each function call
      # Each call to this function only uses 1 API call regardless of limit
      limit = num_responses
    )
  }
  else if(!is.null(lat) & !is.null(lon)){
    params <- list(
      term = term,
      latitude = lat,
      longitude = lon,
      limit = num_responses
    )
  }
  else{
    stop("An address must be provided")
  }

  if(!is.null(radius)){
    params <- c(params, list(radius = radius/1609))
  }

  response <- httr::GET(base_url, query = params, httr::add_headers("Authorization" = paste("Bearer", yelp_api_key)))

  if (httr::status_code(response) == 200) {
    data <- httr::content(response, "text") %>%
      jsonlite::fromJSON(flatten = TRUE)

    if (is.null(data$businesses)) {
      print(paste("Not found"))
      return(NULL)
    }

    return(data$businesses)
  } else {
    print(paste("Error searching for", term, ":", httr::content(response, "text")))
    return(NULL)
  }
}
