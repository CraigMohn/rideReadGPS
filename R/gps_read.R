#' read multiple gps data files
#'
#' \code{read_ridefiles} reads a vector of gps .fit and/or .gpx track files
#'  and put into summary and detail data tibbles for modeling or graphical
#'  summary.  A wrapper for \code{\link{read_ride}}.
#'
#'
#' @param ridefilevec a vector of filenames to process
#' @param cores number of cores (default is #CPUs - 1)
#' @param loud display summary of re/segmenting actions
#' @param ... parameters passed for track cleaning
#'
#' @return a list of two data tibbles:  \eqn{summaries} and \eqn{tracks}
#'    These are linked by integer fields \eqn{startbutton.date}
#'    and \eqn{startbutton.time}
#'
#' @seealso \code{\link{read_ride}}
#'
#' @export
read_ridefiles <- function(ridefilevec,cores=4,loud=FALSE,...)  {

  nfiles <- length(ridefilevec)
  if(missing(cores)) cores <- parallel::detectCores()
  if ((nfiles > 10) & !is.na(cores) & (cores>1)) {
    doParallel::registerDoParallel(cores)
    `%dopar%` <- foreach::`%dopar%`
    cfun <- function(a,b) list(summary=dplyr::bind_rows(a[["summary"]],b[["summary"]]),
                               trackpoints=dplyr::bind_rows(a[["trackpoints"]],b[["trackpoints"]]))
    ridelist <- foreach (x = ridefilevec,.combine=`cfun`,
                        .packages=c("bikeCadHr")) %dopar% {
      read_ride(x,loud=loud,...)
    }
    doParallel::stopImplicitCluster()
    return(list(summaries=dplyr::arrange(ridelist[["summary"]],date,start.hour),
                tracks=dplyr::arrange(ridelist[["trackpoints"]],startbutton.date,timestamp.s)))
  } else {
    outdf <- NULL
    outtracks <- NULL
    for (x in ridefilevec) {
      ride <- read_ride(x,loud=loud,...)
      obsdf <- ride[["summary"]]
      obstrack <- ride[["trackpoints"]]
      if (is.null(outdf)) {
        outdf <- obsdf
      } else {
        outdf <- dplyr::bind_rows(outdf,obsdf)
      }
      if (is.null(outtracks)) {
        outtracks <- obstrack
      } else {
        outtracks <- dplyr::bind_rows(outtracks,obstrack)
      }
    }
    return(list(summaries=dplyr::arrange(outdf,date,start.hour),
                tracks=dplyr::arrange(outtracks,startbutton.date,timestamp.s)))
  }
}
#' read and clean a gps data file
#'
#' \code{read_ride}  processes a gps .fit and/or .gpx track file to create
#'   summary and detail data tibbles for modeling or graphical summary.  Track
#'   segments are adjusted to remove many false autostop-start and startline
#'   delay sequences.  Several measures of cadence are calculated (including
#'   zeros, excluding zeros, and midsegment),spurious and missing cadence values
#'   are repaired, and summary stats are calculated for analysis.
#'
#' @param ridefile a filenames (.fit or .gpx extension) to process
#' @param tz string containing the timezone for the track data frame
#' @param fixDistance repair nonmonotonicities in distance which are on
#'    segment breaks - this occurs when power is lost or on some device lockups
#' @param stopSpeed - speed in m/s below which the bike is considered stopped
#'    for time and cadence calculations
#' @param loud print information about hr/cadence data issues/fixes
#' @param loudSegment print information about re/segmenting track data
#' @param readpkg which package reads the fit file -
#'     valid options are "fitdc","fitdecodeR", or "FITfileR"
#' @param lutzmethod method to use to locate timezone, see package lutz
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{repairSensorDropOut}},
#'    \code{\link{repairHR}},
#'    \code{\link{repairCadence}},
#'    \code{\link{repairPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}},
#'    \code{\link{statsTemp}}
#'
#' @return a list of three data tibbles:  \eqn{summary} and \eqn{trackpoints}
#'    and \eqn{session}
#'    These are linked by integer fields \eqn{startbutton.date}
#'    and \eqn{startbutton.time}
#'
#' @seealso \code{\link{read_ridefiles}},
#'    \code{\link{processSegments}},
#'    \code{\link{repairSensorDropOut}},
#'    \code{\link{repairHR}},
#'    \code{\link{repairCadence}},
#'    \code{\link{repairPower}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
read_ride <- function(ridefile,tz, #="America/Los_Angeles",
                      stopSpeed=0.0,
                      fixDistance=FALSE,loud=FALSE,loudSegment=FALSE,
                      readpkg="fitdecodeR",
                      lutzmethod="fast",...)  {

  cat("\nreading: ",ridefile,"\n")
  if (missing(tz)) {
    tz <- Sys.timezone()
  }
  if (substr(ridefile,nchar(ridefile)-3,nchar(ridefile))==".fit") {
    temp <- read_fittrack(ridefile,readpkg=readpkg)
    time.fn.string <- basename(ridefile)
    fit.fn.time.parse <- getOption("bCadHr.fit.fn.time.parse")
    fit.fn.lead <- getOption("bCadHr.fit.fn.lead")
    fit.fn.trail <- getOption("bCadHr.fit.fn.trail")
    if (nchar(fit.fn.lead)>0) time.fn.string <-
      substr(time.fn.string,
             regexpr(fit.fn.lead,time.fn.string)+nchar(fit.fn.lead)+1,1000000)
    if (nchar(fit.fn.trail)>0) time.fn.string <-
      substr(time.fn.string,1,regexpr(fit.fn.trail,time.fn.string)-1)
    time.turned.on <- strptime(time.fn.string,fit.fn.time.parse,tz=tz)
  } else if (substr(ridefile,nchar(ridefile)-3,nchar(ridefile))==".gpx") {
    temp <- read_gpxtrack(ridefile)
    time.fn.string <- basename(ridefile)
    gpx.fn.time.parse <- getOption("bCadHr.gpx.fn.time.parse")
    gpx.fn.lead <- getOption("bCadHr.gpx.fn.lead")
    gpx.fn.trail <- getOption("bCadHr.gpx.fn.trail")
    if (nchar(gpx.fn.lead)>0) time.fn.string <-
      substr(time.fn.string,
             regexpr(gpx.fn.lead,time.fn.string)+nchar(gpx.fn.lead)+1,1000000)
    if (nchar(gpx.fn.trail)>0) time.fn.string <-
      substr(time.fn.string,1,regexpr(gpx.fn.trail,time.fn.string)-1)
    time.turned.on <- strptime(time.fn.string,gpx.fn.time.parse,tz=tz)
    readpkg <- "gpx-xml"
  } else {
    stop("unknown file extension")
  }
  trackdata <- temp[["track"]]
  recovery_hr <- temp[["recovery_hr"]]
  session <- temp[["session"]]
  hrv <- temp[["hrv"]]
  device_info <- temp[["device_info"]]
  if (!is.null(device_info)) {
    device_info_list <- devices(device_info=device_info,loud=loud)
  }
  attr(trackdata$timestamp.s,"tzone") <- tz
  if (is.na(time.turned.on)){
    #  if filename not successfully turned into a start-button time, use 1st
    #   timestamp value recorded in the track.  Likely but not certain to match
    #   between different versions of the same ride
    if (loud) print(" setting time.turned.on to first obs")
    time.turned.on <- trackdata$timestamp.s[1]
  }
  startbuttonDate <- as.integer(lubridate::mday(time.turned.on)+
                                   100*lubridate::month(time.turned.on)+
                                   10000*lubridate::year(time.turned.on))
  startbuttonTime <- as.integer(lubridate::second(time.turned.on)+
                                   100*lubridate::minute(time.turned.on)+
                                   10000*lubridate::hour(time.turned.on))
  trackdata$startbutton.date <- startbuttonDate
  trackdata$startbutton.time <- startbuttonTime
  ###   minimal data validity checks after dumping unusable obs
  trackdata <- stripDupTrackRecords(trackdata,fixDistance=fixDistance)
  if (is.unsorted(trackdata$segment,strictly=FALSE))
    stop(paste0(ridefile," yields segment ids that are not nondecreasing!"))
  if (is.unsorted(trackdata$distance.m,strictly=FALSE))
    stop(paste0(ridefile," yields distances that are not nondecreasing!"))
  if (is.unsorted(trackdata$timestamp.s,strictly=TRUE))
    stop(paste0(ridefile," yields timestamps that are
                            not strictly increasing!"))

  trackdata$deltatime <- as.numeric(difftime(trackdata$timestamp.s,
                                             lag_one(trackdata$timestamp.s),units="secs"),
                                    units="secs")

  #  repair problems with data
  trackdata <- repairSensorDropOut(trackdata,loud=loud,...)
  repHR <- repairHR(trackdata,loud=loud,...)
  trackdata <- repHR[["trackdf"]]
  repCad <- repairCadence(trackdata,loud=loud,...)
  trackdata <- repCad[["trackdf"]]
  repPower <- repairPower(trackdata,loud=loud,...)
  trackdata <- repPower[["trackdf"]]
  #  split/revise track into segments separated by non-negligile stops
  trackdata <- processSegments(trackdf=trackdata,loud=loudSegment,...)

  sessionStats <- statsSession(session)
  cadStats <- statsCadence(trackdf=trackdata,
                sessionpedalstrokes=sessionStats[["sessionPedalStrokes"]],
                loud=loud,...)
  powStats <- statsPower(trackdf=trackdata,...)
  hrStats <- statsHeartRate(trackdf=trackdata,recovery_hr=recovery_hr,...)
  gearStats <- statsGearing(trackdf=trackdata,...)
  climbStats <- statsGrade(trackdf=trackdata,...)
  stopsStats <- statsStops(trackdf=trackdata,...)
  tempStats <- statsTemp(trackdf=trackdata)

  nonNAs <- which(!is.na(trackdata$position_lon.dd) & !is.na(trackdata$position_lat.dd))
  if (length(nonNAs > 0)) {
    firstLonLat <- min(nonNAs)
    lastLonLat <- max(nonNAs)
  } else {
    firstLonLat <- NA
  }
  if (!is.na(firstLonLat)) {
    tracktz <- lutz::tz_lookup_coords(trackdata$position_lat.dd[firstLonLat],
                                      trackdata$position_lon.dd[firstLonLat],
                                      method=lutzmethod,warn=FALSE)
  } else {
    print(paste0("cannot determine timezone without valid lon/lat, using unknown"))
    tracktz <- "unknown"
  }

  if (!is.na(firstLonLat)){
    begEndGap <-
      raster::pointDistance(cbind(trackdata$position_lon.dd[firstLonLat],
                                  trackdata$position_lat.dd[firstLonLat]),
                            cbind(trackdata$position_lon.dd[lastLonLat],
                                  trackdata$position_lat.dd[lastLonLat]),
                            lonlat=TRUE)
    if (begEndGap>100) {
      cat("  *non-loop - distance between start and end = ",begEndGap,"m \n")
      cat("   start = ",trackdata$position_lon.dd[firstLonLat],"  ",
          trackdata$position_lat.dd[firstLonLat],"  \n")
      cat("   stop  = ",trackdata$position_lon.dd[lastLonLat],"  ",
          trackdata$position_lat.dd[lastLonLat],"\n")
      rideLoop <- FALSE
    } else {
      rideLoop <- TRUE
    }
  } else {
    begEndGap <- NA
    rideLoop <- NA
  }

  rideDate <- as.Date(format(trackdata$timestamp.s[1],tz=tz))
  startTime <- as.character(trackdata$timestamp.s[1])
  startHour <- as.numeric(lubridate::hour(format(time.turned.on,tz=tz))
           + 0.25*(round((60*lubridate::minute(format(time.turned.on,tz=tz))+
                             lubridate::second(format(time.turned.on,tz=tz)))/
                           (60*15))))
  track.cleaned <-
    tibble::tibble(date = rideDate,
                   start.time = startTime,
                   start.hour = startHour,
                   tracktz =tracktz,
                   nwaypoints = nrow(trackdata),
                   numsegs = max(trackdata$segment),
                   pct.trkpts.hr = sum(!is.na(trackdata$heart_rate.bpm))/
                                   nrow(trackdata),  # based on corrected HR (too big = NA)
                   nHRTooHigh = repHR[["nHRTooHigh"]],
                   pct.trkpts.cad = sum(!is.na(trackdata$cadence.uncorrected))/
                                      nrow(trackdata),  #  cadence issues complicated, use raw
                   nCadMissingtoZero = repCad[["nCadMissingtoZero"]],
                   nCadTooHigh = repCad[["nCadTooHigh"]],
                   nCadTooLow = repCad[["nCadTooLow"]],
                   nCadStoppedPos = repCad[["nCadStoppedPos"]],
                   nCadStuck = repCad[["nCadStuck"]],
                   begEndGap = begEndGap,
                   deltaElev = trackdata$altitude.m[nrow(trackdata)] -
                               trackdata$altitude.m[1],
                   ride.loop = rideLoop,
                   distance = trackdata$distance.m[nrow(trackdata)],
                   total.time = totalTime(trackdata),
                   rolling.time = rollingTime(trackdata,
                                              stopSpeed=stopSpeed),
                   pedal.time = pedalTime(trackdata,
                                          stopSpeed=stopSpeed,...),
                   speed.rolling.m.s = trackdata$distance.m[nrow(trackdata)]/
                                          rollingTime(trackdata,
                                                      stopSpeed=stopSpeed),
                   speed.all.m.s = trackdata$distance.m[nrow(trackdata)]/
                                          totalTime(trackdata),
                   speed.max.m.s = max(trackdata$speed.m.s,na.rm=TRUE),
                   temp.C.mean = tempStats[["meanTemp"]],
                   temp.C.min = tempStats[["minTemp"]],
                   temp.C.max = tempStats[["maxTemp"]],
                   avgcadence.nozeros.session = cadStats[["avgcadenceNoZerosSession"]],
                   avgcadence.withzeros.session = cadStats[["avgcadenceWithZerosSession"]],
                   avgcadence.nozeros = cadStats[["avgcadenceNoZerosSum"]],
                   avgcadence.withzeros = cadStats[["avgcadenceWithZerosSum"]],
                   avgcadence.midsegment = cadStats[["avgcadenceMidsegment"]],
                   summed.pedal.strokes = cadStats[["summedPedalStrokes"]],
                   avgpower.nozeros=powStats[["avgpowerNoZeros"]],
                   avgpower.withzeros=powStats[["avgpowerWithZeros"]],
                   power.calibrations.detected=repPower[["nCalibrateSequences"]],
                   ascent = climbStats[["ascent"]],
                   descent = climbStats[["descent"]],
                   distance.ascending = climbStats[["distanceAscending"]],
                   distance.descending = climbStats[["distanceDescending"]],
                   pct.low.gear = gearStats[["pctLowGear1"]],
                   low.gear = gearStats[["lowGear1"]],
                   pct.low.gear2 = gearStats[["pctLowGear2"]],
                   low.gear2 = gearStats[["lowGear2"]],
                   hr.max = hrStats[["hrMax"]],
                   hr.min = hrStats[["hrMin"]],
                   hr.mean = hrStats[["hrMean"]],
                   hr.m2 = hrStats[["hrMoment2"]],
                   hr.at.start = hrStats[["hrAtStart"]],
                   hr.at.stop = hrStats[["hrAtStop"]],
                   hr.recovery = hrStats[["hrRecovery"]],
                   startline.time = stopsStats[["startlineTime"]],
                   stops.subminute = stopsStats[["stopsSubMinute"]],
                   stops.1to10minutes = stopsStats[["stops1to10Minutes"]],
                   stops.10to30minutes = stopsStats[["stops10to30Minutes"]],
                   stops.long = stopsStats[["stopsLong"]],
                   session.distance = sessionStats[["sessionDistance"]],
                   session.elapsed.time = sessionStats[["sessionElapsedTime"]],
                   session.timer.time = sessionStats[["sessionTimerTime"]],
                   session.pedal.strokes = sessionStats[["sessionPedalStrokes"]],
                   session.total.calories = sessionStats[["sessionTotalCalories"]],
                   session.avg.speed = sessionStats[["sessionAvgSpeed"]],
                   session.max.speed = sessionStats[["sessionMaxSpeed"]],
                   session.total.ascent = sessionStats[["sessionTotalAscent"]],
                   session.total.descent = sessionStats[["sessionTotalDescent"]],
                   session.avg.cadence = sessionStats[["sessionAvgCadence"]],
                   session.avg.hr = sessionStats[["sessionAvgHr"]],
                   session.max.hr = sessionStats[["sessionMaxHr"]],
                   session.avg.power = sessionStats[["sessionAvgPower"]],
                   session.max.power = sessionStats[["sessionMaxPower"]],
                   session.pedal.smoothness = sessionStats[["sessionPedalSmoothness"]],
                   session.left.pedal.smoothness = sessionStats[["sessionLeftPedalSmoothness"]],
                   session.right.pedal.smoothness = sessionStats[["sessionRightPedalSmoothness"]],
                   session.left.torque.eff = sessionStats[["sessionLeftTorqueEff"]],
                   session.right.torque.eff = sessionStats[["sessionRightTorqueEff"]],
                   session.time.standing = sessionStats[["sessionTimeStanding"]],
                   session.left.right.balance = sessionStats[["sessionLeftRightBalance"]],
                   sourcefile = basename(ridefile),
                   readpkg = readpkg,
                   processed.time = Sys.time(),
                   startbutton.date=startbuttonDate,
                   startbutton.time=startbuttonTime)
  if (!is.null(device_info)) {
    track.cleaned <- cbind(track.cleaned,as_tibble(device_info_list)[1,])
  }
  return(list(summary=track.cleaned,trackpoints=trackdata,session=session))
}

