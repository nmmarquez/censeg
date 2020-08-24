#' Calculate index of dissimilarity measure of segregation
#'
#' @description calculates index of dissimilarity on a dataset
#'
#' @param group_tract_df race specific df with a count for every census tract
#' @param out_groups character vector indicating the out group of the
#' comparison. The default behavior selects "NH White" such that
#' White-Non White segregation is being evaluated.
#' @param parent_geo character vector of additional columns to subdivide analysis
#' @param ... added for compatibility
#'
#' @import data.table
#'
#' @return single row data frame with index of dissimilarity
#'
#' @examples
#' calc_dissimilarity(get_acs_race("CA", "Orange", year = 2013))
#'
#' @export

calc_dissimilarity <- function(
    group_tract_df, out_groups = "NH White", parent_geo = c(), ...){

    ptr_g <- c("GEOID", "OG", "sp", parent_geo)
    pt_g <- c("GEOID", "sp", parent_geo)
    pr_g <- c("OG", "sp", parent_geo)
    p_g <- c("sp", parent_geo)

    group_tract_df[,sp:=1]
    group_tract_df[,OG := race %in% out_groups]
    g_df <- group_tract_df[, list(value = sum(value)) , by = ptr_g]
    g_df[,denom := sum(value), by = pr_g]
    g_df[,term := value/denom]
    g_df[,term := ifelse(!is.finite(term), 0, term)]
    coll_df <- g_df[,list(dt = abs(diff(term))), by = pt_g]

    out_df <- coll_df[,list(DI = sum(dt) * .5), by = p_g]
    out_df[, sp := NULL]

    copy(out_df)
}
