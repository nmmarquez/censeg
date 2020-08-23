rm(list=ls())
library(dplyr)
library(readr)
library(stringr)
library(devtools)
library(data.table)
library(sf)
library(tigris)

cbsa_county_df <- read_csv(
    "https://data.nber.org/cbsa-csa-fips-county-crosswalk/cbsa2fipsxw.csv") %>%
    filter(!is.na(fipscountycode) & !is.na(cbsacode)) %>%
    rename(countyname = countycountyequivalent) %>%
    rename(type = metropolitanmicropolitanstatis) %>%
    rename(STATE = fipsstatecode, COUNTY = fipscountycode) %>%
    select(cbsacode, cbsatitle, STATE, COUNTY, statename, type) %>%
    mutate(
        statename = str_replace_all(statename, " ", "_")) %>%
    filter(statename != "Puerto_Rico") %>%
    as.data.table()

county_df <- counties(class = "sf")

sub_county_df <- rename(county_df[
    county_df$GEOID %in% paste0(cbsa_county_df$STATE, cbsa_county_df$COUNTY),],
    STATE = STATEFP, COUNTY = COUNTYFP)

cbsa_sf_old <- do.call(
    sf:::rbind.sf, lapply(unique(cbsa_county_df$cbsacode), function(c){
        sub_cbsa_df <- cbsa_county_df[cbsacode == c,]

        sub_county_df %>%
            filter(GEOID %in% paste0(sub_cbsa_df$STATE, sub_cbsa_df$COUNTY)) %>%
            st_union() %>%
            {st_sf(geometry=.)} %>%
            mutate(cbsacode = c, cbsatitle = sub_cbsa_df$cbsatitle[1]) %>%
            mutate(type = sub_cbsa_df$type[1])
    }))

cbsa_sf <- cbsa_sf_old %>%
    st_transform(29101) %>%
    st_simplify(dTolerance = 1000)

use_data(cbsa_county_df, overwrite = TRUE)
use_data(cbsa_sf, overwrite = TRUE)
