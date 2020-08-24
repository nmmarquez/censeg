#' A wrapper function for get_acs_race and a particular segregation function.
#'
#' @description census_seg uses the census api to download race & ethnicity
#' data from a specified 5 year ACS survey from the table "B03002" or the
#' decennial census tables P0040\*\* P0050\*\* for the 2000 and 2010 census
#' respectively. From these
#' tables the racial groups White, Black, American Indian or Alaskan Native,
#' Asian, Native Hawaiian or Pacific Islander, some other race, and two or more
#' Races are considered and treated as mutually exclusive categories for
#' individuals who mark Non-Hispanic. Hispanic individuals are designated as
#' Hispanic no matter what they mark for their Racial identifier and are treated
#' as a mutually exclusive racial group for the purposes of the analysis. Then,
#' segregation measures are calculated on the resulting data.
#'
#' @param state An optional vector of states for which you are requesting data.
#' State names, postal codes, and FIPS codes are accepted. Defaults to NULL.
#' @param county The county for which you are requesting data. County names and
#' FIPS codes are accepted. Must be combined with a value supplied to 'state'.
#' Defaults to NULL.
#' @param metro The cbsa name or fips code. Pulls all counties within a given
#' CBSA. See cbsa_county_df for more info on county to cbsa mapping. Defaults
#' to NULL.
#' @param geography The geography of your data. Defaults to 'tract'.
#' @param cache_table Whether or not to cache table names for faster future
#' access. Defaults to FALSE; if TRUE, only needs to be called once per dataset.
#' If variables dataset is already cached via the load_variables function, this
#' can be bypassed.
#' @param year The year, or endyear, of the ACS sample or Census. 2012 through
#' 2018 are available for ACS and 2000 and 2010 for Census. Defaults to 2010.
#' @param key Your Census API key. Obtain one at
#' http://api.census.gov/data/key_signup.html
#' @param seg_func a segregation function from the censeg package. Defaults to
#' `calc_theilsH`
#'
#' @return data frame with geography-race specific counts for a given state,
#' county, or CBSA.
#'
#' @examples
#' # get segregation data by tract for all of King county Washington
#' census_seg("WA", "King")
#'
#' # get segregation data by block group for all of Seattle metro area for 2015
#' census_seg(
#'     metro = "Seattle-Tacoma-Bellevue, WA", geography = "block group",
#'     year = 2015)
#'
#' @export

census_seg <- function(
    state = NULL,
    county = NULL,
    metro = NULL,
    geography = "tract",
    cache_table = FALSE,
    year = 2010,
    key = NULL,
    seg_func = calc_theilsH){

    if(year == 2010 | year == 2000){
        race_df <- get_decennial_race(
            state = state,
            county = county,
            metro = metro,
            geography = geography,
            cache_table = cache_table,
            year = year,
            key = key)
    }

    else{
        race_df <- get_acs_race(
            state = state,
            county = county,
            metro = metro,
            geography = geography,
            cache_table = cache_table,
            year = year,
            key = key)
    }

    seg_df <- seg_func(race_df)

    if(is.null(metro)){
        seg_df[, state := state]
        seg_df[, county := county]
    }

    else{
        seg_df[, metro := metro]
    }

    copy(seg_df)
}
