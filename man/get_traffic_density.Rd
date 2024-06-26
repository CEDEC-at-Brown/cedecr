% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcs.R
\name{get_traffic_density}
\alias{get_traffic_density}
\title{Get Traffic Density}
\usage{
get_traffic_density(latitude, longitude, apiKey)
}
\arguments{
\item{latitude}{A value representing the latitude of the location.}

\item{longitude}{A value representing the longitude of the location.}

\item{apiKey}{A string representing the API key for the TomTom Traffic API.}
}
\value{
A value representing the free flow speed at the specified location. If the API request fails, it returns an error message.
}
\description{
This function retrieves the traffic density (free flow speed) for a given latitude and longitude using the TomTom Traffic API.
}
