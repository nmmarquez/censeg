#' Calculate local indicator of spatial association (LISA) measure of segregation
#'
#' @description calculates LISA on a dataset
#'
#' @param group_df race specific df with a count for every GEOID
#' @param out_groups character vector indicating the out group of the
#' comparison. The default behavior selects "NH White" such that
#' Non Hispanic White clustering is being evaluated.
#' @param parent_geo character vector of additional columns to subdivide analysis
#' @param year year to get geography information. Defaults to 2010
#' @param method character method for p value adjustment
#' @param ... other arguments to pass to geo_distance
#'
#' @import data.table
#'
#' @return data frame with measures of lisa local clusters
#'
#' @examples
#' calc_lisa(get_decennial_race("DC"))
#'
#' @export

calc_lisa <- function(
    group_df, out_groups = "NH White", parent_geo = c(), year = 2010, method = "none", ...){

    group_tract_df <- copy(group_df)

    pt_g <- c("GEOID", "sp", parent_geo)
    p_g <- c("sp", parent_geo)

    group_tract_df[,sp:=1]
    sub_df <- group_tract_df[race %in% out_groups, .(value = sum(value)), by = pt_g]
    mean_df <- sub_df[,.(mu = mean(value), N = .N), by = p_g]
    id_df <- unique(sub_df[,..p_g])

    neigh_df <- rbindlist(lapply(1:nrow(id_df), function(i){
        geo_neighbors(
            merge(id_df[i,], sub_df, all.x = TRUE, by = p_g)$GEOID,
            year = year)
    }))

    setnames(neigh_df, "org", "GEOID")
    setnames(sub_df, "value", "xi")

    calc_df <- merge(neigh_df, sub_df, by = "GEOID")

    setnames(sub_df, "GEOID", "dest")
    setnames(sub_df, "xi", "xj")

    calc_df <- merge(calc_df, sub_df, all.x=TRUE, by = c("dest", p_g))
    calc_df <- merge(calc_df, mean_df, by = p_g)
    calc_df[,wij := as.numeric(neigh)/sum(neigh), by = pt_g]

    out_df <- calc_df[,.(
        I = (first(xi) - first(mu)) / sum((xj - mu)^2 / (N)) * sum(wij * (xj - mu)),
        Ei = - 1 / first(N-1),
        wi2 = sum(wij^2),
        value = first(xi),
        b2 = sum((xj - mu)^4 / N) / sum((xj - mu)^2 / N)^2,
        cardp1 = sum(neigh)+1),
        by=c("GEOID", p_g)]
    out_df[, Vi := wi2 * (.N - b2) / (.N-1) + (1-wi2) * (2*b2 - .N) /
               ((.N-1) * (.N-2)) - (-1 / (.N-1))^2, by = p_g]
    out_df[, Zi := (I - Ei)/sqrt(Vi)]
    out_df[, pv := pnorm(Zi, lower.tail = FALSE), by = p_g]
    out_df[, Ip := apply(cbind(pv, cardp1), 1, function(x) p.adjust(
        x[1], method = method, n = x[2])), by = p_g]
    out_df[, hiclust := value > mean(value), by = p_g]
    out_df[,sp := NULL]
    out_df[,wi2 := NULL]
    out_df[,b2 := NULL]
    out_df[,cardp1 := NULL]
    out_df[,pv := NULL]
    out_df[,value := NULL]
    out_df[,out_groups := paste0(out_groups, collapse = "_")]

    copy(out_df)
}
