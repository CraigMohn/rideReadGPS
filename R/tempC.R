#' generate power statistics for a track
#'
#' \code{statsTemp}  processes a gps track file to summarize the temperature data
#'
#' @param trackdf data frame or tibble with gps track data
#' @param tempTimeIgnore number of seconds after start to ignore temp data
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{repairHR}},
#'    \code{\link{repairCadence}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @return a list containing summary data
#'
#' @seealso \code{\link{read_ride}},\code{\link{excludeCalibrate}},
#'    \code{\link{repairHR}},
#'    \code{\link{repairCadence}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
statsTemp <- function(trackdf,tempTimeIgnore=600,...) {
  ttime <- cumsum(trackdf$deltatime)
  keep <- ttime >= tempTimeIgnore
  if (sum(keep) > 0) {
    return(list(
      meanTemp = mean(trackdf$temperature.C[keep],na.rm=TRUE),
      minTemp = min(trackdf$temperature.C[keep],na.rm=TRUE),
      maxTemp = max(trackdf$temperature.C[keep],na.rm=TRUE)
    ))
  } else {
    return(list(
      meanTemp = NA,
      minTemp = NA,
      maxTemp = NA
    ))
  }
}
