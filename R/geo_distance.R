#' Calculate a distance matrix from an sf object
#'
#' @description Calculates a distance matrix from a list of geoids which
#' correspond to either tract of block group ids for a given year. Alternatively
#' calculates distance matrix for a sf data frame of polygons.
#'
#' @param GEOIDs Character vector of GEOIDs of either tracts or block groups
#' @param year The year, or endyear, of the ACS sample or Census. 2012 through
#' 2018 are available for ACS and 2000 and 2010 for Census. Defaults to 2010.
#' @param geoid_sf Alternative specification where an sf object is supplied and
#' distances are calculated directly from that object
#' @param matrix Logical whether to return a matrix rather than data frame.
#' @param adjust Logical, adjust using Massey Denton specification.
#'
#' @return data frame or matrix indicating distance between geometry centroids
#' in kilometers.
#'
#' @import sf
#' @import tigris
#' @import lwgeom
#'
#' @examples
#' # get distances of all block groups in DC
#'
#' geo_distance(geoid_sf = block_groups("DC", class = "sf"), matrix = TRUE)
#'
#' @export

geo_distance <- function(
    GEOIDs = NULL, year = 2018, geoid_sf = NULL, matrix = FALSE, adjust = TRUE){
    if(is.null(geoid_sf)){
        geo_char <- unique(nchar(GEOIDs))
        st <- unique(substr(GEOIDs, 1, 2))
        mes <- "GEOIDs must all be 11|12 characters for tract|block groups"

        if(length(geo_char) > 1 | !(geo_char %in% c(11, 12)) ){
            stop(mes)
        }

        if(geo_char == 11){
            geo_sf <- do.call(
                sf:::rbind.sf,
                lapply(st, tracts, year = year, class = "sf", cb = TRUE))
        }

        if(geo_char == 12){
            geo_sf <- do.call(
                sf:::rbind.sf,
                lapply(st, block_groups, year = year, class = "sf", cb = TRUE))
        }

        if(year == 2010 | year == 2000){
            geo_sf$GEOID <- paste0(
                geo_sf$STATEFP, geo_sf$COUNTYFP, geo_sf$TRACT)
        }

        if(year == 1990){
            geo_sf$GEOID <- paste0(
                geo_sf$ST, geo_sf$CO, geo_sf$TRACTBASE, "0")
        }

        sub_geo_sf <- geo_sf[geo_sf$GEOID %in% GEOIDs,]
    }

    else{
        sub_geo_sf <- geoid_sf
    }

    st_agr(sub_geo_sf) <- "constant"
    sub_cen_sf <- st_centroid(st_transform(sub_geo_sf, 29101))

    # first create matrix of distances
    m <- (st_distance(sub_cen_sf, sub_cen_sf) / 1000) %>%
        # next assign names
        matrix(
            nrow = nrow(sub_cen_sf), ncol = nrow(sub_cen_sf),
            dimnames = list(sub_cen_sf$GEOID, sub_cen_sf$GEOID))

    # adjust based on Massey Denton paper
    if(adjust){
        diag(m) <- (.6 * (st_area(sub_geo_sf) / 1000000))^.5
    }

    if(matrix){
        return(m)
    }

    data.table(org=colnames(m)[col(m)], dest=rownames(m)[row(m)], dist=c(m))
}
