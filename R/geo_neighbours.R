#' Calculate a neighborhood matrix from an sf object
#'
#' @description Calculates a neighborhood matrix from a list of geoids which
#' correspond to either tract of block group ids for a given year. Alternatively
#' calculates neighborhood matrix for a sf data frame of polygons
#'
#' @param GEOIDs Character vector of GEOIDs of either tracts or block groups
#' @param year The year, or endyear, of the ACS sample or Census. 2012 through
#' 2018 are available for ACS and 2000 and 2010 for Census. Defaults to 2010.
#' @param geoid_sf Alternative specification where an sf object is supplied and
#' neighborhoods are calculated directly from that object
#' @param matrix Logical whether to return a matrix rather than data frame.
#' @param queen Logical, use queen algorithm of adjacency rather than rook.
#'
#' @return data frame or matrix indicating adjacency with logical values
#'
#' @import sf
#' @import tigris
#'
#' @examples
#' # get neighbors of all 2010 tracts from Wake county North Carolina
#' geo_neighbors(get_decennial_race("NC", "Wake")$GEOID)
#'
#' @export

geo_neighbors <- function(
    GEOIDs = NULL, year = 2010, geoid_sf = NULL, matrix = FALSE, queen = TRUE){
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
                lapply(st, tracts, year = year, class = "sf"))
        }

        if(geo_char == 12){
            geo_sf <- do.call(
                sf:::rbind.sf,
                lapply(st, block_groups, year = year, class = "sf"))
        }

        sub_geo_sf <- st_transform(geo_sf[geo_sf$GEOID %in% GEOIDs,], 29101)
    }

    else{
        sub_geo_sf <- st_transform(geoid_sf, 29101)
    }

    if(queen){
        nb_ <- st_relate(sub_geo_sf, sub_geo_sf, pattern = "F***T****")
    }

    else{
        nb_ <- st_relate(sub_geo_sf, sub_geo_sf, pattern = "F***1****")
    }

    m <- as.matrix(nb_)
    dimnames(m) <- list(sub_geo_sf$GEOID, sub_geo_sf$GEOID)

    if(matrix){
        return(m)
    }

    data.table(org=colnames(m)[col(m)], dest=rownames(m)[row(m)], neigh=c(m))
}
