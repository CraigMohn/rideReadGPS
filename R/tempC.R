#' generate power statistics for a track
#'
#' \code{tempC}  processes a gps track file to summarize the temperature data
#'
#' @param trackdf data frame or tibble with gps track data
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
statsTemp <- function(trackdf,...) {
  return(list(
    meanTemp = mean(trackdf$temperature.C,na.rm=TRUE),
    minTemp = min(trackdf$temperature.C,na.rm=TRUE),
    maxTemp = min(trackdf$temperature.C,na.rm=TRUE)
  ))
}
