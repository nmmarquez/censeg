#' Calculate entropy and Theil's-H measure of segregation
#'
#' @description calculates Theil's-H and Entropy on a dataset
#'
#' @param group_tract_df race specific df with a count for every census tract
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

calc_theilsH <- function(group_tract_df , ...){
    tractEDF <- group_tract_df[, list(N = sum(value)), by=list(GEOID, race)]
    tractEDF[, Total := sum(N), by=list(GEOID)]
    tractEDF[, Qr := N / Total]
    tractEDF[, Qrt := Qr * log(1/Qr)]
    tractEDF[, Qrt := ifelse(is.na(Qrt), 0, Qrt)]
    tractEDF <- tractEDF[, list(E = sum(Qrt) / log(exp(1)), Pop = sum(N)),
                         by=list(GEOID)]

    topEDF <- group_tract_df[, list(N = sum(value)), by=list(race)]
    topEDF[, Total := sum(N)]
    topEDF[, Qr := N / Total]
    topEDF[, Qrt := Qr * log(1/Qr)]
    topEDF[, Qrt := ifelse(is.na(Qrt), 0, Qrt)]
    topEDF <- sum(topEDF$Qrt) / log(exp(1))

    tractEDF[, tE := topEDF]
    tractEDF[, totalPop := sum(Pop)]
    tractEDF[, numer := Pop / totalPop * (tE - E)]

    data.table(H = sum(tractEDF$numer) / topEDF, E = topEDF)
}
