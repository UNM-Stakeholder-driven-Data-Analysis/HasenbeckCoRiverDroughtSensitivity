% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_analysisservices.R
\name{get_source_route_analysis}
\alias{get_source_route_analysis}
\title{Returns all WDID(s), and their stream mile, located between two different stream locations on the DWR Water Source Framework}
\usage{
get_source_route_analysis(
  lt_gnis_id = NULL,
  lt_stream_mile = NULL,
  ut_gnis_id = NULL,
  ut_stream_mile = NULL,
  api_key = NULL
)
}
\arguments{
\item{lt_gnis_id}{character or numeric,    lower terminus GNIS ID}

\item{lt_stream_mile}{character or numeric, lower terminus stream mile}

\item{ut_gnis_id}{character or numeric, upper terminus GNIS ID}

\item{ut_stream_mile}{character or numeric, upper terminus stream mile}

\item{api_key}{character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.}
}
\value{
dataframe of water source route framework analysis
}
\description{
Makes a request to the analysisservices/watersourcerouteanalysis/ endpoint to retrieve the DWR source route framework analysis data
}
