#' Calculate location quotient measure of segregation
#'
#' @description calculates location quotient on a dataset
#'
#' @param group_df race specific df with a count for every GEOID
#' @param out_groups character vector indicating the out group of the
#' comparison. The default behavior selects "NH White" such that
#' White clustering is being calculated relative the total population.
#' @param parent_geo character vector of additional columns to subdivide analysis
#' @param ... added for compatibility
#'
#' @import data.table
#'
#' @return data frame with location quotient
#'
#' @examples
#' calc_locquo(get_acs_race("CO", "Boulder", year = 2017))
#'
#' @export

calc_locquo <- function(
    group_df, out_groups = "NH White", parent_geo = c(), ...){

    group_tract_df <- copy(group_df)
    pt_g <- c("GEOID", "sp", parent_geo)
    p_g <- c("sp", parent_geo)

    group_tract_df[,sp:=1]
    b_df <- group_tract_df[race %in% out_groups, .(b = sum(value)) , by = pt_g]
    t_df <- group_tract_df[, .(t = sum(value)) , by = pt_g]
    out_df <- merge(b_df, t_df, by = pt_g)

    out_df[, B_ := sum(b), by = p_g]
    out_df[, T_ := sum(t), by = p_g]
    out_df[, LQ := (b/t)/(B_/T_)]

    out_df[, sp := NULL]
    out_df[, B_ := NULL]
    out_df[, T_ := NULL]
    out_df[, b := NULL]
    out_df[, t := NULL]
    out_df[,out_groups := paste0(out_groups, collapse = "_")]

    copy(out_df)
}
