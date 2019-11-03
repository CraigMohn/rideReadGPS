#' rideReadGPS: A package for reading cycling .fit and .gpx files into
#'    dataframes containing (corrected) trackpoints and summary statistics
#'
#' @section import gps data:
#'   \link{read_ridefiles}, \link{read_ride}
#'
#' @import magrittr tibble dplyr stringr
#' @import XML
#' @importFrom stats approx median quantile setNames weighted.mean
#' @importFrom raster pointDistance
#' @importFrom lubridate mday month year second minute hour ymd_hms
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom foreach foreach %dopar% %do%
#' @importFrom parallel detectCores
#' @importFrom lutz tz_lookup_coords
#'
#' @name rideReadGPS
NULL

###  make the R checker happy
tedious <- utils::globalVariables(c("start.hour","start.time",
                                    "startbutton.date","stoplabels","timestamp.s",
                                    "distance.m","segment","timestamp",
                                    "x","xtext.stop","y","xend","xcol",
                                    "verticalMultiplier","color","hjust","label",
                                    "yend","group","timeBeg","timeEnd","pauseSize",
                                    "timelaststop","segbegtime","segendtime",
                                    "movingrun","maxdist","startofstop","sosNA",
                                    "joinseg","subsegment"))


