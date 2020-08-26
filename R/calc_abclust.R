#' Calculate absolute clustering measure of segregation
#'
#' @description calculates absolute clustering on a dataset
#'
#' @param group_df race specific df with a count for every GEOID
#' @param out_groups character vector indicating the out group of the
#' comparison. The default behavior selects "NH White" such that
#' Non Hispanic White segregation is being evaluated.
#' @param parent_geo character vector of additional columns to subdivide analysis
#' @param year year to get geography information. Defaults to 2010
#' @param ... other arguments to pass to geo_distance
#'
#' @import data.table
#'
#' @return data frame with measures of absolute clustering
#'
#' @examples
#' calc_abclust(get_acs_race("DC"))
#'
#' @export

calc_abclust <- function(
    group_df, out_groups = "NH White", parent_geo = c(), year = 2010, ...){

    group_tract_df <- copy(group_df)

    pt_g <- c("GEOID", "sp", parent_geo)
    p_g <- c("sp", parent_geo)

    group_tract_df[,sp:=1]
    group_tract_df[,OG:=race %in% out_groups]

    tot_df <- group_tract_df[, list(Total = sum(value)), by = pt_g]
    og_df <- group_tract_df[OG == TRUE, list(N = sum(value)), by = pt_g]
    comb_df <- merge(tot_df, og_df, all.x = TRUE)[Total != 0,]
    comb_df[, X:=sum(N)]

    id_df <- unique(comb_df[,..p_g])

    dist_df <- rbindlist(lapply(1:nrow(id_df), function(i){
        geo_distance(
            merge(id_df[i,], comb_df, all.x = TRUE, by = p_g)$GEOID,
            year = year, ...)
    }))

    setnames(comb_df, "GEOID", "org")
    setnames(comb_df, "Total", "tj")
    setnames(comb_df, "N", "xj")

    calc_df <- merge(dist_df, comb_df, all.x=TRUE, by = "org")

    setnames(comb_df, "org", "dest")
    setnames(comb_df, "tj", "ti")
    setnames(comb_df, "xj", "xi")
    comb_df[,X:=NULL]

    calc_df <- merge(calc_df, comb_df, all.x=TRUE, by = c("dest", p_g))
    calc_df[,cij := exp(-dist)]

    term_df <- calc_df[,list(
        term1 = sum((xi / X) * cij * xj),
        term3 = sum((xi / X) * cij * tj),
        term2_4 = (first(X) / .N) * sum(cij)
    ), by = p_g]

    term_df[,AC := (term1 - term2_4) / (term3 - term2_4)]
    term_df[,term1 := NULL]
    term_df[,term3 := NULL]
    term_df[,term2_4 := NULL]
    term_df[,sp := NULL]
    term_df[,out_groups := paste0(out_groups, collapse = "_")]

    copy(term_df)
}
