#' collect gps session statistics for a track
#'
#' \code{statsSession}  processes a gps session file
#'
#' @param session data frame or tibble with gps session data
#'
#' @return a list containing summary data
#'
#' @seealso \code{\link{read_ride}},\code{\link{processSegments}},
#'    \code{\link{repairSensorDropOut}},
#'    \code{\link{repairHR}},
#'    \code{\link{repairCadence}},
#'    \code{\link{repairPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsStops}},
#'    \code{\link{statsTemp}}
#'
#' @export
statsSession <- function(session) {
  if (is.null(session)) {
    sessionDistance <- NA
    sessionElapsedTime <- NA
    sessionTimerTime <- NA
    sessionPedalStrokes <- NA
    sessionTotalCalories <- NA
    sessionAvgSpeed <- NA
    sessionMaxSpeed <- NA
    sessionTotalAscent <- NA
    sessionTotalDescent <- NA
    sessionAvgCadence <- NA
    sessionAvgHr <- NA
    sessionMaxHr <- NA
    sessionAvgPower <- NA
    sessionMaxPower <- NA
    sessionPedalSmoothness <- NA
    sessionLeftPedalSmoothness <- NA
    sessionRightPedalSmoothness <- NA
    sessionLeftTorqueEff <- NA
    sessionRightTorqueEff <- NA
    sessionTimeStanding <- NA
    sessionLeftRightBalance <- NA
  } else {
    sessionDistance <- ifelse("total_distance" %in% colnames(session),
                              session$total_distance[1],NA)
    sessionElapsedTime <- ifelse("total_elapsed_time" %in% colnames(session),
                                 session$total_elapsed_time[1],NA)
    sessionTimerTime <- ifelse("total_timer_time" %in% colnames(session),
                               session$total_timer_time[1],NA)
    sessionPedalStrokes <- ifelse("total_cycles" %in% colnames(session),
                                  session$total_cycles[1],NA)
    sessionTotalCalories <- ifelse("total_calories" %in% colnames(session),
                                   session$total_calories[1],NA)
    sessionAvgSpeed <- ifelse("avg_speed" %in% colnames(session),
                              session$avg_speed[1],NA)
    sessionMaxSpeed <- ifelse("max_speed" %in% colnames(session),
                              session$max_speed[1],NA)
    sessionTotalAscent <- ifelse("total_ascent" %in% colnames(session),
                                 session$total_ascent[1],NA)
    sessionTotalDescent <- ifelse("total_descent" %in% colnames(session),
                                  session$total_descent[1],NA)
    sessionAvgCadence <- ifelse("avg_cadence" %in% colnames(session),
                                session$avg_cadence[1],NA)
    sessionAvgHr <- ifelse("avg_heart_rate" %in% colnames(session),
                           session$avg_heart_rate[1],NA)
    sessionMaxHr <- ifelse("max_heart_rate" %in% colnames(session),
                           session$max_heart_rate[1],NA)
    sessionAvgPower <- ifelse("avg_power" %in% colnames(session),
                              session$avg_power[1],NA)
    sessionMaxPower <- ifelse("max_power" %in% colnames(session),
                              session$max_power[1],NA)
    sessionPedalSmoothness <- ifelse("avg_combined_pedal_smoothness" %in% colnames(session),
                                     session$avg_combined_pedal_smoothness[1],NA)
    sessionLeftPedalSmoothness <- ifelse("avg_left_pedal_smoothness" %in% colnames(session),
                                         session$avg_left_pedal_smoothness[1],NA)
    sessionRightPedalSmoothness <- ifelse("avg_right_pedal_smoothness" %in% colnames(session),
                                          session$avg_right_pedal_smoothness[1],NA)
    sessionLeftTorqueEff <- ifelse("avg_left_torque_effectiveness" %in% colnames(session),
                                         session$avg_left_torque_effectiveness[1],NA)
    sessionRightTorqueEff <- ifelse("avg_right_torque_effectiveness" %in% colnames(session),
                                    session$avg_right_torque_effectiveness[1],NA)
    sessionTimeStanding <- ifelse("time_standing" %in% colnames(session),
                                    session$time_standing[1],NA)
    sessionLeftRightBalance <- ifelse("left_right_balance" %in% colnames(session),
                                      rightPedalShare(session$left_right_balance[1]),NA)
  }
  return(list(
    sessionDistance = sessionDistance,
    sessionElapsedTime = sessionElapsedTime,
    sessionTimerTime = sessionTimerTime,
    sessionPedalStrokes = sessionPedalStrokes,
    sessionTotalCalories = sessionTotalCalories,
    sessionAvgSpeed = sessionAvgSpeed,
    sessionMaxSpeed = sessionMaxSpeed,
    sessionTotalAscent = sessionTotalAscent,
    sessionTotalDescent = sessionTotalDescent,
    sessionAvgCadence = sessionAvgCadence,
    sessionAvgHr = sessionAvgHr,
    sessionMaxHr = sessionMaxHr,
    sessionAvgPower = sessionAvgPower,
    sessionMaxPower = sessionMaxPower,
    sessionPedalSmoothness = sessionPedalSmoothness,
    sessionLeftPedalSmoothness = sessionLeftPedalSmoothness,
    sessionRightPedalSmoothness = sessionRightPedalSmoothness,
    sessionLeftTorqueEff = sessionLeftTorqueEff,
    sessionRightTorqueEff = sessionRightTorqueEff,
    sessionTimeStanding = sessionTimeStanding,
    sessionLeftRightBalance = sessionLeftRightBalance
    ))
}

#' clean up sensor dropout data for a track
#'
#' \code{repairSensorDropOut}  processes a gps track file for data dropouts
#'
#'
#' @param trackdf data frame or tibble with gps track data
#' @param loud display actions taken
#' @param fixAllSensorDrop logical, if TRUE fix case of all sensor dropouts
#' @param fixSpeedSensorDrop logical, if TRUE fix case of speed sensor dropouts
#' @param sensorDropSmooth fill in values
#' @param lookBackward number of seconds to look before dropout when fixing
#' @param lookForward number of seconds to look after dropout when fixing
#' @param ... parameters for \code{\link{processSegments}},
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
#' @return dataframe with data repaired
#'
#' @seealso \code{\link{read_ride}}
#'
#' @export
repairSensorDropOut <- function(trackdf,
                                fixAllSensorDrop=TRUE,
                                fixSpeedSensorDrop=TRUE,
                                sensorDropSmooth=TRUE,
                                lookBackward=3,
                                lookForward=5,
                                loud=FALSE,...) {

  flagdrop <- function(varname) {
    if (varname %in% names(trackdf)) {
      return(is.na(deframe(trackdf[,varname])))
    } else {
      return(TRUE)
    }
  }
  repairdrops <- function(trackdf,varname,NAOK,zeroOK,allDrop) {
    dropsrle <- rle(allDrop)
    dropids <- dropsrle[["values"]]
    ndropouts <- sum(dropids)
    dropids[dropids] <- cumsum(dropids)[dropids]
    dropsrle[["values"]] <- dropids

    allDrop <- inverse.rle(dropsrle)

    if (ndropouts > 0) {
      for (d in 1:ndropouts) {
        dd=allDrop==d
        trackdf <- repairdrop(trackdf,d,varname,NAOK,zeroOK,allDrop=dd)
      }
    }
    return(trackdf)
  }
  repairdrop <- function(trackdf,d,varname,NAOK,zeroOK,allDrop) {
    begdrow <- max(which.max(allDrop) - 1,1)
    enddrow <- min(length(allDrop) + 1 - which.max(rev(allDrop)) + 1,
                   length(allDrop))
    begtime <- max(trackdf$timestamp.s[1],
                   trackdf$timestamp.s[begdrow] - lookBackward)
    endtime <- min(trackdf$timestamp.s[nrow(allDrop)],
                   trackdf$timestamp.s[enddrow] + lookForward)
    begdrow <- max(begdrow-1,1)
    enddrow <- min(enddrow+1,length(allDrop))
    begrow <- which.max(trackdf$timestamp.s >= begtime)
    endrow <- length(allDrop) + 1 - which.max(rev(trackdf$timestamp.s <= endtime))
    if (NAOK) {
      ytemp <- deframe(trackdf[begdrow:enddrow,varname])
      if (!zeroOK) ytemp[ytemp==0] <- NA
      if (sum(!is.na(trackdf[begdrow:enddrow,varname])) >= 2) {
        yreplace <- approx(x=deframe(trackdf[begdrow:enddrow,"timestamp.s"]),
                           y=ytemp,
                           xout=deframe(trackdf[begdrow:enddrow,"timestamp.s"]),
                           method="linear")[[2]]
        trackdf[begdrow:enddrow,varname] <- yreplace
      }
    } else {
      ytemp <- deframe(trackdf[begrow:endrow,varname])
      if (!zeroOK) ytemp[ytemp==0] <- NA
      if (sum(!is.na(ytemp)) >= 2) {
        yreplace <- approx(x=deframe(trackdf[begrow:endrow,"timestamp.s"]),
                           y=ytemp,
                           xout=deframe(trackdf[begrow:endrow,"timestamp.s"]),
                           method="linear")[[2]]
        trackdf[begrow:endrow,varname] <- yreplace
      }
    }
    return(trackdf)
  }

  spdDrop <- flagdrop("speed.m.s")
  cadDrop <- flagdrop("cadence.rpm")
  hrDrop <-  flagdrop("heart_rate.bpm")
  powDrop <- flagdrop("power.watts" )

  allDrop <- spdDrop & cadDrop & hrDrop & powDrop

  if (sum(allDrop) > 0) {
    if (loud) {
      cat("   there are ",sum(allDrop)," records with no external sensor data\n")
      cat("     ",paste(sort(unique(trackdf$timestamp.s[allDrop])),sep=","),"\n")
    }
    if (fixAllSensorDrop) {
      if (sensorDropSmooth) {
        trackdf <- repairdrops(trackdf,"speed.m.s",NAOK=FALSE,
                               zeroOK=TRUE,allDrop)
        trackdf <- repairdrops(trackdf,"cadence.rpm",NAOK=FALSE,
                               zeroOK=TRUE,allDrop)
        trackdf <- repairdrops(trackdf,"heart_rate.bpm",NAOK=FALSE,
                               zeroOK=FALSE,allDrop)
        trackdf <- repairdrops(trackdf,"power.watts",NAOK=FALSE,
                               zeroOK=FALSE,allDrop)
      } else {
        if (loud) cat("    removing them\n")
        trackdf <- trackdf[!allDrop,]
      }
    }
  }
  if (sum(spdDrop) > 0) {
    if (loud) {
      cat("  ** there are ",sum(spdDrop)," records with no speed data\n")
      cat("  ",paste(sort(unique(trackdf$timestamp.s[spdDrop])),sep=","),"\n")
    }
    if (fixSpeedSensorDrop) {
      trackdf$speed.m.s[spdDrop] <- 0.0
      if (loud) cat("      setting them to zero\n")
    }

  }
  if (loud & sum(cadDrop) > 0 & sum(!cadDrop) > 0)
    cat("  ** there are ",sum(cadDrop)," records with no cadence data\n")
  if (loud & sum(hrDrop) > 0 & sum(!hrDrop) > 0)
    cat("  ** there are ",sum(hrDrop)," records with no heartrate data\n")
  if (loud & sum(powDrop) > 0 & sum(!powDrop) > 0)
    cat("  ** there are ",sum(powDrop)," records with no power data\n")

  return(trackdf)
}
rightPedalShare <- function(l_r_b) {
  if (is.na(l_r_b)) {
    rps <- NA
  } else if (l_r_b >= 256) {
    # pedal is known to be right if 1st bit set ( l_r_b & 8000H )
    # ignore top 2 bits ( l_r_b & 3FFF ) and shift 4 decimal places
    rps <- bitwAnd(as.integer(l_r_b),16383L) / 10000
  } else if (l_r_b < 256 & l_r_b >= 128) {
    #  single byte data, top bit means right pedal ( l_r_b & 7FH )
    rps <- bitwAnd(as.integer(l_r_b),63L) / 100
  } else {
    # otherwise not sure which pedal is described
    rps <- NA
  }
  return(rps)
}
