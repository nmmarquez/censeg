#' A wrapper function for get_acs_race and a particular segregation function.
#'
#' @description all_census_seg uses the census api to download race & ethnicity
#' data from a specified 5 year ACS survey from the table "B03002" or the
#' decennial census tables P0040\*\* P0050\*\* for the 2000 and 2010 census
#' respectively. From these
#' tables the racial groups White, Black, American Indian or Alaskan Native,
#' Asian, Native Hawaiian or Pacific Islander, some other race, and two or more
#' Races are considered and treated as mutually exclusive categories for
#' individuals who mark Non-Hispanic. Hispanic individuals are designated as
#' Hispanic no matter what they mark for their Racial identifier and are treated
#' as a mutually exclusive racial group for the purposes of the analysis. Then,
#' segregation measures are calculated on the resulting data by either county or
#' metropolitan area across the US 50 states and DC.
#'
#' @param geography The geography of your data. Defaults to 'tract'.
#' @param cache_table Whether or not to cache table names for faster future
#' access. Defaults to FALSE; if TRUE, only needs to be called once per dataset.
#' If variables dataset is already cached via the load_variables function, this
#' can be bypassed.
#' @param year The year, or endyear, of the ACS sample or Census. 2012 through
#' 2018 are available for ACS and 2000 and 2010 for Census. Defaults to 2010.
#' @param key Your Census API key. Obtain one at
#' http://api.census.gov/data/key_signup.html
#' @param counties Do analysis of all counties rather than metro areas. Default
#' FALSE.
#' @param seg_func a segregation function from the censeg package. Defaults to
#' `calc_theilsH`
#' @param ... Other parameters to pass to the seg_func.
#'
#' @return data frame with geography-race specific counts for all
#' counties or CBSAs.
#'
#' @import sf
#'
#' @examples
#' \dontrun{
#' # Calculate segregation for all CBSAs from the 2010 census
#' all_census_seg(year = 2010)
#' }
#'
#' @export

all_census_seg <- function(
    geography = "tract",
    cache_table = FALSE,
    year = 2010,
    key = NULL,
    counties = FALSE,
    seg_func = calc_theilsH,
    ...){

    if(year == 2010 | year == 2000){
        race_df <- rbindlist(lapply(
            c(state.abb, "DC"), get_decennial_race, geography = geography,
            year = year, key = key, cache_table = cache_table))
    }

    else{
        race_df <- rbindlist(lapply(
            c(state.abb, "DC"), get_acs_race, geography = geography,
            year = year, key = key, cache_table = cache_table))
    }

    race_df[,STATE := substr(GEOID, 1, 2)]
    race_df[,COUNTY := substr(GEOID, 3, 5)]
    hier_df <- merge(race_df, cbsa_county_df[,-5], all.x = TRUE)

    if(counties){
        out_df <- seg_func(hier_df, parent_geo = c("STATE", "COUNTY"), ...)
    }

    else{
        hier_df <- hier_df[!is.na(type),]
        p_g <- c("cbsacode", "cbsatitle", "type")
        out_df <- seg_func(hier_df, parent_geo = p_g, year = year, ...)
    }

    copy(out_df)
}
