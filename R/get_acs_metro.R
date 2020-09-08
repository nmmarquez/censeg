#' A wrapper function of get_acs which gets data for 50 states plus DC
#'
#' @description get_acs_metro uses the census api to download data for all
#' 50 states plus DC for "on spine" geographies county and below. This method
#' will associate those geographies with a CBSA as well.
#'
#' @param geography unit for which to pull data
#' @param variables Character string or vector of character strings of
#' variable IDs.
#' @param table The Census table for which you would like to request all
#' variables. Uses lookup tables to identify the variables; performs faster
#' when variable table already exists through load_variables(cache = TRUE).
#' Only one table may be requested per call.
#' @param cache_table Whether or not to cache table names for faster future
#' access. Defaults to FALSE; if TRUE, only needs to be called once per dataset.
#' If variables dataset is already cached via the load_variables function,
#' this can be bypassed.
#' @param year The year for which you are requesting data. Default 2018.
#' @param output One of "tidy" (the default) in which each row represents an
#' enumeration unit-variable combination, or "wide" in which each row
#' represents an enumeration unit and the variables are in the columns.
#' @param summary_var Character string of a "summary variable" from the
#' decennial Census to be included in your output. Usually a variable
#' (e.g. total population) that you'll want to use as a denominator.
#' @param key Your Census API key. Obtain one at
#' http://api.census.gov/data/key_signup.html
#' @param moe_level The confidence level of the returned margin of error.
#' One of 90 (the default), 95, or 99.
#' @param survey The ACS contains one-year, three-year, and five-year surveys
#' expressed as "acs1", "acs3", and "acs5". The default selection is "acs5."
#' @param show_call if TRUE, display call made to Census API. This can be very
#' useful in debugging and determining if error messages returned are due to
#' tidycensus or the Census API. Copy to the API call into a browser and see
#' what is returned by the API directly. Defaults to FALSE.
#' @param ... Other argumenets to pass to get_decennial.
#'
#' @return data frame with census info and adjoined cbsa location
#'
#' @examples
#' \dontrun{
#' # get Guat population for all tracts in US with appropriate metro info
#' get_acs_metro("tract", c(Guatemalan = "B03001_010"))
#' }
#'
#' @export

get_acs_metro <- function(
    geography,
    variables = NULL,
    table = NULL,
    cache_table = FALSE,
    year = 2018,
    output = "tidy",
    summary_var = NULL,
    key = NULL,
    moe_level = 90,
    survey = "acs5",
    show_call = FALSE,
    ...){

    all_df <- as.data.table(rbindlist(lapply(c(state.abb, "DC"), function(s){
        get_acs(
            geography = geography,
            variables = variables,
            table = table,
            cache_table = cache_table,
            year = year,
            state = s,
            county = NULL,
            geometry = FALSE,
            output = output,
            keep_geo_vars = FALSE,
            shift_geo = FALSE,
            summary_var = summary_var,
            key = key,
            moe_level = moe_level,
            survey = survey,
            show_call = show_call,
            ...
        )
    })))

    all_df[,STATE := substr(GEOID, 1, 2)]
    all_df[,COUNTY := substr(GEOID, 3, 5)]

    merge(
        all_df, cbsa_county_df, all.x = TRUE, by = c("STATE", "COUNTY"))
}
