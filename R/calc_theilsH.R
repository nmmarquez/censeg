#' Calculate entropy and Theil's-H measure of segregation
#'
#' @description calculates Theil's-H and Entropy on a dataset
#'
#' @param group_df race specific df with a count for every GEOID
#' @param parent_geo character vector of additional columns to subdivide analysis
#' @param ... added for compatibility
#'
#' @import data.table
#'
#' @return single row data frame with measures of Theil's-H and entropy
#'
#' @examples
#' calc_theilsH(get_acs_race("WA", "King"))
#'
#' @export

calc_theilsH <- function(group_df, parent_geo = c(), ...){

    group_tract_df <- copy(group_df)
    ptr_g <- c("GEOID", "race", "sp", parent_geo)
    pt_g <- c("GEOID", "sp", parent_geo)
    pr_g <- c("race", "sp", parent_geo)
    p_g <- c("sp", parent_geo)

    group_tract_df[,sp:=1]

    tractEDF <- group_tract_df[, list(N = sum(value)), by=ptr_g]
    tractEDF[, Total := sum(N), by=pt_g]
    tractEDF[, Qr := N / Total]
    tractEDF[, Qrt := Qr * log(1/Qr)]
    tractEDF[, Qrt := ifelse(is.na(Qrt), 0, Qrt)]
    tractEDF <- tractEDF[, list(E = sum(Qrt) / log(exp(1)), Pop = sum(N)),
                         by=pt_g]

    topEDF <- group_tract_df[, list(N = sum(value)), by=pr_g]
    topEDF[, Total := sum(N), by = p_g]
    topEDF[, Qr := N / Total]
    topEDF[, Qrt := Qr * log(1/Qr)]
    topEDF[, Qrt := ifelse(is.na(Qrt), 0, Qrt)]
    topEDF <- topEDF[, list(tE = sum(Qrt) / log(exp(1))), by = p_g]

    tractEDF <- merge(tractEDF, topEDF, all.x = TRUE)
    tractEDF[, totalPop := sum(Pop), by = p_g]
    tractEDF[, numer := Pop / totalPop * (tE - E)]

    out_df <- tractEDF[,list(H = sum(numer) / first(tE), E = first(tE)),
                       by = p_g]
    out_df[, sp := NULL]

    copy(out_df)
}
