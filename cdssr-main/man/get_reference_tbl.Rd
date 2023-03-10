% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_reference_tbl.R
\name{get_reference_tbl}
\alias{get_reference_tbl}
\title{Return reference table information, which may help with other searches}
\usage{
get_reference_tbl(table_name = NULL, api_key = NULL)
}
\arguments{
\item{table_name}{character,name of the reference table to return. Must be one of: ("county", "waterdistricts", "waterdivisions", "designatedbasins", "managementdistricts", "telemetryparams", "climateparams", "divrectypes", "flags"). Defaults to NULL}

\item{api_key}{character, API authorization token, optional. If more than maximum number of requests per day is desired, an API key can be obtained from CDSS.}
}
\value{
dataframe of CDSS endpoint Reference Table
}
\description{
Makes requests to the /referencetables/ endpoints and returns helpful reference tables. Reference tables can help identify valid inputs for querying CDSS API resources using cdssr. For more detailed information visit: https://dwr.state.co.us/rest/get/help#Datasets&#ReferenceTablesController&#gettingstarted&#jsonxml.
}
\examples{
\dontrun{
# Retrieve station flag reference table
flag_tbl <- get_reference_tbl(
   table_name = "flags"
   )

# Retrieve available telemetry station parameters
telemetry_params <- get_reference_tbl(
   table_name = "telemetryparams"
   )

# Retrieve available climate station parameters
climate_params <- get_reference_tbl(
   table_name = "climateparams"
   )

# Retrieve water districts
water_districts <- get_reference_tbl(
   table_name = "waterdistricts"
   )
}
}
