#' Calculate index of dissimilarity measure of segregation
#'
#' @description calculates index of dissimilarity on a dataset
#'
#' @param group_df race specific df with a count for every GEOID
#' @param out_groups character vector indicating the out group of the
#' comparison. The default behavior selects "NH White" such that
#' White-Non White segregation is being evaluated.
#' @param alt_groups character vector indicating the alt groups of the
#' comparison. The default behavior selects NULL such that
#' all groups except the out group are selected.
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
    group_df, out_groups = "NH White", alt_groups = NULL, parent_geo = c(), ...){

    if(is.null(alt_groups)){
        alt_groups <- unique(group_df$race)
        alt_groups <- alt_groups[!(alt_groups %in% out_groups)]
    }

    group_tract_df <- group_df[race %in% out_groups | race %in% alt_groups,]
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
    out_df[,out_groups := paste0(out_groups, collapse = "_")]
    out_df[,alt_groups := paste0(alt_groups, collapse = "_")]

    copy(out_df)
}
