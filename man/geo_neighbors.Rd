% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo_neighbours.R
\name{geo_neighbors}
\alias{geo_neighbors}
\title{Calculate a neighborhood matrix from an sf object}
\usage{
geo_neighbors(
  GEOIDs = NULL,
  year = 2010,
  geoid_sf = NULL,
  matrix = FALSE,
  queen = TRUE
)
}
\arguments{
\item{GEOIDs}{Character vector of GEOIDs of either tracts or block groups}

\item{year}{The year, or endyear, of the ACS sample or Census. 2012 through
2018 are available for ACS and 2000 and 2010 for Census. Defaults to 2010.}

\item{geoid_sf}{Alternative specification where an sf object is supplied and
neighborhoods are calculated directly from that object}

\item{matrix}{Logical whether to return a matrix rather than data frame.}

\item{queen}{Logical, use queen algorithm of adjacency rather than rook.}
}
\value{
data frame or matrix indicating adjacency with logical values
}
\description{
Calculates a neighborhood matrix from a list of geoids which
correspond to either tract of block group ids for a given year. Alternatively
calculates neighborhood matrix for a sf data frame of polygons
}
\examples{
# get neighbors of all 2010 tracts from Wake county North Carolina
geo_neighbors(get_decennial_race("NC", "Wake")$GEOID)

}
