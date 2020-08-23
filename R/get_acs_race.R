#' A wrapper function of get_acs that downloads race data from census API
#'
#' @description get_acs_race uses the census api to download race and ethnicity
#' data from a specified 5 year ACS survey from the table "B03002". From this
#' table the racial groups White, Black, American Indian or Alaskan Native,
#' Asian, Native Hawaiian or Pacific Islander, some other race, and two or more
#' Races are considered and treated as mutually exclusive categories for
#' individuals who mark Non-Hispanic. Hispanic individuals are designated as
#' Hispanic no matter what they mark for their Racial identifier and are treated
#' as a mutually exclusive racial group for the purposes of the analysis.
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
#' @param year The year, or endyear, of the ACS sample. 2009 through 2018 are
#' available. Defaults to 2018.
#' @param key Your Census API key. Obtain one at
#' http://api.census.gov/data/key_signup.html
#' @param as.data.table Return output as data table? Deafaults to TRUE.
#' @param ... other arguments passed to get_acs
#'
#' @import tidycensus
#' @import data.table
#'
#' @return data frame with geography-race specific counts for a given state,
#' county, or CBSA.
#'
#' @examples
#' # get race data by tract for all of King county Washington
#' get_acs_race("WA", "King")
#'
#' # get race data by block group for all of Seattle metro area for 2015
#' get_acs_race(
#'     metro = "Seattle-Tacoma-Bellevue, WA", geography = "block group",
#'     year = 2015)
#'
#'
#' @export

get_acs_race <- function(
    state = NULL,
    county = NULL,
    metro = NULL,
    geography = "tract",
    cache_table = FALSE,
    year = 2018,
    key = NULL,
    as.data.table = TRUE,
    ...){

    if(!is.null(metro)){
        if(is.numeric(metro)){
            metro <- cbsa_county_df[cbsa_county_df$cbsacode == metro, ]
            metro <- metro$cbsatitle[[1]]
            cat(paste0("Using metro area: ", metro, "\n"))
        }

        metro_df <- cbsa_county_df[cbsa_county_df$cbsatitle == metro,]

        comb_df <- do.call(rbind, lapply(1:nrow(metro_df), function(i){
            get_acs_race(
                state = as.numeric(metro_df$STATE[i]),
                county = as.numeric(metro_df$COUNTY[i]),
                metro = NULL,
                geography = geography,
                cache_table = cache_table,
                year = year,
                key = key,
                as.data.table = as.data.table)
        }))

        return(comb_df)
    }

    else{

        variables <- c(
             "NH White" = "B03002_003",
             "NH Black" = "B03002_004",
             "NH AIAN"  = "B03002_005",
             "NH Asian" = "B03002_006",
             "NH NHOPI" = "B03002_007",
             "NH Other" = "B03002_008",
             "NH TOMR"  = "B03002_009",
             "Hispanic" = "B03002_012"
        )

        race_df <- get_acs(
            geography,
            variables = variables,
            cache_table = cache_table,
            year = year,
            state = state,
            county = county,
            key = key,
            ...)

        race_df$race <- race_df$variable
        race_df$value <- race_df$estimate
        race_df <- race_df[,c("GEOID", "race", "value")]

        if(as.data.table){
            race_df <- as.data.table(race_df)
        }

        return(copy(race_df))
    }
}
