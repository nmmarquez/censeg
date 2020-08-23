#' Core based statistical areas of the United States county mapping.
#'
#' Metropolitan and micropolitan areas of the United States and their associated
#' counties as designated by OMB 2013 descriptions.
#'
#' @format A data frame with 1808 rows and 6 variables:
#' \describe{
#'   \item{cbsacode}{FIPS code for the CBSA}
#'   \item{cbsattitle}{Common name of the CBSA}
#'   \item{STATE}{State FIPS code as character with leading zeros}
#'   \item{COUNTY}{County FIPS code as character with leading zeros}
#'   \item{statename}{name of state}
#'   \item{type}{Type of Area (either Metropolitan or Micropolitan)}
#' }
#' @source \url{https://data.nber.org/cbsa-csa-fips-county-crosswalk/cbsa2fipsxw.csv}
"cbsa_county_df"

#' Core based statistical areas of the United States shape file.
#'
#' Metropolitan and micropolitan areas of the United States and their associated
#' shape geometries for mapping.
#'
#' @format A data frame with 1808 rows and 6 variables:
#' \describe{
#'   \item{cbsacode}{FIPS code for the CBSA}
#'   \item{cbsattitle}{Common name of the CBSA}
#'   \item{type}{Type of Area (either Metropolitan or Micropolitan)}
#' }
#' @source \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}
"cbsa_sf"
