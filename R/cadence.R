#' summarize cadence statistics for a track
#'
#' \code{statsCadence}  processes a gps track file to correct and
#'   summarize cadence data
#'
#'
#' @param trackdf data frame or tibble with gps track data
#' @param sessionpedalstrokes number of pedalstrokes in session records of
#'    fit file
#' @param loud print useful trace information
#' @param cadTrimBegSecs number of seconds after the beginning of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param cadTrimEndSecs number of seconds before the end of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param cadTrimBegMeters number of meters after the beginning of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param cadTrimEndMeters number of meters before the end of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{repairSensorDropOut}},
#'    \code{\link{repairHR}},
#'    \code{\link{repairCadence}},
#'    \code{\link{repairPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}},
#'    \code{\link{statsTemp}}
#'
#' @return a list containing summary data
#'
#' @seealso \code{\link{read_ride}},\code{\link{processSegments}},
#'    \code{\link{repairSensorDropOut}},
#'    \code{\link{repairHR}},
#'    \code{\link{repairCadence}},
#'    \code{\link{repairPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}},
#'    \code{\link{statsTemp}}
#'
#' @export
statsCadence <- function(trackdf,sessionpedalstrokes=NA,
                         loud=FALSE,
                         cadTrimBegSecs=15,cadTrimBegMeters=10,
                         cadTrimEndSecs=20,cadTrimEndMeters=15,...) {

  pedaling <- !is.na(trackdf$cadence.rpm) & trackdf$cadence.rpm > 0
  segtimes <- tibble::tibble(timestamp=cumsum(trackdf$deltatime),
                                 distance.m=trackdf$distance.m,
                                 segment=trackdf$segment)    %>%
    dplyr::group_by(segment) %>%
    dplyr::mutate(segbegtime=min(timestamp),
                  segendtime=max(timestamp),
                  segbegdist=min(distance.m),
                  segenddist=max(distance.m)) %>%
    dplyr::arrange(timestamp)

  innersegment <- segtimes$timestamp >= segtimes$segbegtime+cadTrimBegSecs &
    segtimes$timestamp <= segtimes$segendtime-cadTrimEndSecs &
    segtimes$distance.m >= segtimes$segbegdist+cadTrimBegMeters &
    segtimes$distance.m <= segtimes$segenddist-cadTrimBegMeters

  cadenceNumerator <- sum(trackdf$deltatimestart[pedaling]*
                          trackdf$cadence.rpm[pedaling])
  cadenceNumeratorInner <- sum(trackdf$deltatimestart[pedaling&innersegment]*
                               trackdf$cadence.rpm[pedaling&innersegment])

  avgcadenceNoZerosSum <- cadenceNumerator / pedalTime(trackdf,...)
  avgcadenceWithZerosSum <- cadenceNumerator / rollingTime(trackdf,...)
  avgcadenceMidsegment <- cadenceNumeratorInner /
    sum(trackdf$deltatime[pedaling & innersegment])
  if (is.na(sessionpedalstrokes)) {
    avgcadenceNoZerosSession <- NA
    avgcadenceWithZerosSession <- NA
  } else {
    if (loud) {
      cat("pedalstrokes - session = ",sessionpedalstrokes," , summed = ",
                   cadenceNumerator/60,"   summed exceeds session by  ",
                   round(((cadenceNumerator/(60*sessionpedalstrokes))-1)*100,2),
          "percent\n")
    }
    avgcadenceNoZerosSession <- sessionpedalstrokes / (pedalTime(trackdf,...)/60)
    avgcadenceWithZerosSession <- sessionpedalstrokes / (rollingTime(trackdf,...)/60)
    if (abs(avgcadenceNoZerosSession-avgcadenceNoZerosSum) >= 0.1 & loud)
      print(paste0("  cadence nozeros (session,sum) = ",avgcadenceNoZerosSession," , ",avgcadenceNoZerosSum))
    if (abs(avgcadenceWithZerosSession-avgcadenceWithZerosSum) >= 0.1 & loud)
      print(paste0("  cadence withzeros (session,sum) = ",avgcadenceWithZerosSession," , ",avgcadenceWithZerosSum))
  }

  return(list(avgcadenceNoZerosSum=avgcadenceNoZerosSum,
              avgcadenceWithZerosSum=avgcadenceWithZerosSum,
              avgcadenceNoZerosSession=avgcadenceNoZerosSession,
              avgcadenceWithZerosSession=avgcadenceWithZerosSession,
              avgcadenceMidsegment=avgcadenceMidsegment,
              summedPedalStrokes=cadenceNumerator/60,
              sessionPedalStrokes=sessionpedalstrokes))
}


#' clean up cadence data for a track
#'
#' \code{repairCadence}  processes a gps track file to correct and
#'   summarize cadence data
#'
#'
#' @param trackdf data frame or tibble with gps track data
#' @param cadNAtoZeroInit either "all", "stopped","downhill" or "none" to
#'   specify when to replace inital missing cadence values with zero
#' @param cadNAtoZeroMid an integer, set midride missing cadence values to zero
#'   if preceded by this many zeros
#' @param cadNAtoZeroFinal either "all", "stopped","downhill" or "none" to
#'   specify when to replace final missing cadence values with zero
#' @param fixCadence repair cadence errors
#' @param cadMax max credible cadence value, larger values are errors
#' @param cadMin min nonzero cadence value, smaller values set to 0
#' @param cadCorrectTooHigh "smooth" to repair excessive cadence values
#'    using triangular-kernel-weighted average of the nearest nonmissing values
#'    in the same segment or "cap" to cap them at cadMax
#' @param cadCorrectNA "smooth" to repair cadence missing values using
#'    triangular-kernel-weighted average of the nearest nonmissing values in
#'    the same segment or "zero" to set them to zero
#' @param cadCorrectWindowSec window size for kernel smoothing of cadence
#' @param cadStuckMax threshold cadence value for removing repeated low values,
#'    useful for some sensor/GPS combinations.  0 means no checking for this.
#' @param cadStuckRep minimum length of runs of low cadence values to remove,
#'    useful for some sensor/GPS combinations
#' @param cadStuckSpdDelta proportionate change from initial speed to remove
#'    useful for some sensor/GPS combinations
#' @param cadCorrectStopped repair cadence by setting cadence to 0 when speed
#'    is zero.  This is what would be appropriate if magnet parked near sensor
#'    was generating spurious clicks or if moving pedal while at stoplight
#' @param loud display actions taken
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{repairSensorDropOut}},
#'    \code{\link{repairHR}},
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
#' @return dataframe with cadence data repaired
#'
#' @seealso \code{\link{read_ride}},
#'    \code{\link{repairSensorDropOut}},
#'    \code{\link{repairHR}},
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
#' @export
repairCadence <- function(trackdf,
                          cadNAtoZeroInit="stopped",
                          cadNAtoZeroMid=3,
                          cadNAtoZeroFinal="downhill",
                          fixCadence=TRUE,
                          cadMax=160,cadMin=0,
                          cadCorrectTooHigh="smooth",
                          cadCorrectNA="zero",
                          cadCorrectWindowSec=7,
                          cadStuckMax=0,cadStuckRep=4,cadStuckSpdDelta=0.07,
                          cadCorrectStopped=TRUE,
                          loud=FALSE,...) {
  trackdf$cadence.uncorrected <- trackdf$cadence.rpm
  cadChanged <- FALSE

  ##  correct missing values which are zeros
  cadtemp <- fixSensorMissing(trackdf$cadence.rpm,
                              trackdf$speed.m.s,
                              trackdf$altitude.m,
                              initialNAs=cadNAtoZeroInit,
                              midNAs=cadNAtoZeroMid,
                              finalNAs=cadNAtoZeroFinal,
                              varstr="cadence",
                              loud=loud)
  nCadMissingtoZero <- cadtemp[["nchanged"]]
  if (cadtemp[["nchanged"]]>0) {
    cadChanged <- TRUE
    trackdf$cadence.rpm <- cadtemp[["sensorvec"]]
  }

  ## too-large cadence values - count and optionally fix by smoothing or capping
  cadTooHigh <- trackdf$cadence.rpm > cadMax
  cadTooHigh[is.na(cadTooHigh)] <- FALSE
  if (sum(cadTooHigh) > 0) {
    if (loud) {
      if (fixCadence  & tolower(cadCorrectTooHigh) %in% c("smooth","cap"))
        cat("fixing (using ",cadCorrectTooHigh,") ",sep="")
      cat(sum(cadTooHigh)," too-large cadence values\n",sep="")
      cat("   ",paste(sort(unique(trackdf$cadence.rpm[cadTooHigh])),sep=","),"\n")
    }
    if (fixCadence) {
      if (tolower(cadCorrectTooHigh) == "smooth") {
        trackdf$cadence.rpm[cadTooHigh] <- NA
        cadenceSmoothed <- smoothDataSegments(yvec=trackdf$cadence.rpm,
                                              xvar=cumsum(trackdf$deltatime),
                                              segment=trackdf$segment,
                                              bw=cadCorrectWindowSec,
                                              nneighbors=cadCorrectWindowSec,
                                              kernel="triangular",
                                              replaceNAs=TRUE)
        trackdf$cadence.rpm[cadTooHigh] <- cadenceSmoothed[cadTooHigh]
        cadChanged <- TRUE
      } else if (tolower(cadCorrectTooHigh) == "cap") {
        trackdf$cadence.rpm[cadTooHigh] <- cadMax
        cadChanged <- TRUE
      }
    }
  }
  nCadTooHigh <- sum(cadTooHigh)

  ## too small cadence values  - count and optionally fix by setting to zero
  cadTooLow <- trackdf$cadence.rpm > 0 & trackdf$cadence.rpm < cadMin
  cadTooLow[is.na(cadTooLow)] <- FALSE
  if (sum(cadTooLow) > 0) {
    if (loud) {
      if (fixCadence) cat("  fixing (set=0 ")
      cat(sum(cadTooLow)," too-small cadence values\n",sep="")
      cat("   ",paste(sort(unique(trackdf$cadence.rpm[cadTooLow])),
                       sep=","),"\n")
    }
    if (fixCadence) {
      trackdf$cadence.rpm[cadTooLow] <- 0
      cadChanged <- TRUE
    }
  }
  nCadTooLow <- sum(cadTooLow)

  ######   Cadence stuck on low value - NAs may mean 0, no effect
  nCadStuck <- 0
  if (cadStuckMax > 0) {
    cadrle <- rle(trackdf$cadence.rpm)
    cadrlestops <- cumsum(cadrle[["lengths"]])
    cadrlestarts <- c(0,cadrlestops[-length(cadrlestops)]) + 1
    zeroinrun <- mapply(zerosinrange,
                        begin=cadrlestarts+1,
                        end=cadrlestops,
                        MoreArgs = list(vec=trackdf$speed.m.s))
    stuckrun <-
      #  stuck cadence is nonmissing and positive
      !is.na(cadrle[["values"]]) &
      cadrle[["values"]] > 0 &
      #  stuck value is less than threshold
      cadrle[["values"]] <= cadStuckMax &
      #  run of stuck values is long enough
      cadrle[["lengths"]] >= cadStuckRep &
      #  run hits a stop or next cadence is zero
      #( (trackdf$speed.m.s[cadrlestops] == 0) |
      ( zeroinrun |
          (c(cadrle[["values"]][-1]==0,TRUE) &
             !is.na(c(cadrle[["values"]][-1],TRUE))) )
    if (max(stuckrun) > 0) {
      #  zap cadence values when speed deviates from first in run
      stuckstarts <- cadrlestarts[stuckrun]
      stuckstops <- cadrlestops[stuckrun]
      ssspdrle <- cadrle
      ssspdrle[["values"]] <- trackdf$speed.m.s[cadrlestarts]
      ssspdrle[["values"]][!stuckrun] <- 0.0
      stuckstartspeed <- inverse.rle(ssspdrle)
      speedfact <- abs(stuckstartspeed - trackdf$speed.m.s)/stuckstartspeed
      speedfact[stuckstartspeed==0.0] <- 0
      zapcadence <- speedfact > cadStuckSpdDelta
      if (max(zapcadence) > 0) {
        if (loud) {
          if (fixCadence) cat("zeroing ")
          cat(sum(zapcadence)," apparently stuck cadence values \n",sep="")
          temp <- trackdf[zapcadence,c("timestamp.s","speed.m.s","cadence.rpm",
                                       "distance.m")]
          temp$newgroup <- (temp$timestamp.s - dplyr::lag(temp$timestamp.s,1) > 3) |
                           (temp$cadence.rpm != dplyr::lag(temp$cadence.rpm))
          temp$newgroup[1] <- TRUE
          temp$grp <- cumsum(temp$newgroup)
          temp <- temp %>% dplyr::group_by(grp) %>%
                  dplyr::summarize(begtime=min(timestamp.s),n=n(),cad=mean(cadence.rpm))
          print(as.data.frame(temp,n=30,na.print="NA"))
        }
        if (fixCadence) {
          trackdf$cadence.rpm[zapcadence] <- 0
          cadChanged <- TRUE
        }
        nCadStuck <- sum(zapcadence)
      }
    }
  }

  ######    cadence > 0 but not moving
  cadzero <-  trackdf$cadence.rpm>0 & !is.na(trackdf$cadence.rpm) &
    trackdf$speed.m.s==0 &
    c(0,trackdf$speed.m.s[-nrow(trackdf)]) == 0
  if (sum(cadzero)>0) {
    if (loud) {
      if (cadCorrectStopped & fixCadence) cat("zeroing ",sep="")
      cat(sum(cadzero)," positive cadence values while speed is 0\n")
      if (sum(cadzero)>0) {
        temp <- trackdf[cadzero,c("timestamp.s","speed.m.s","cadence.rpm",
                                  "distance.m")]
        temp$newgroup <- (temp$timestamp.s - dplyr::lag(temp$timestamp.s,1) > 3) |
          (temp$cadence.rpm != dplyr::lag(temp$cadence.rpm))
        temp$newgroup[1] <- TRUE
        temp$grp <- cumsum(temp$newgroup)
        temp <- temp %>% dplyr::group_by(grp) %>%
          dplyr::summarize(begtime=min(timestamp.s),n=n(),cad=mean(cadence.rpm))
        print(as.data.frame(temp,n=30,na.print="NA"))
      }
    }
    if (cadCorrectStopped & fixCadence) {
      trackdf$cadence.rpm[cadzero] <- 0
      cadChanged <- TRUE
    }
  }
  nCadStoppedPos <- sum(cadzero)

  ######   Missing cadence values
  cadenceNA <- is.na(trackdf$cadence.rpm)
  if (sum(cadenceNA) > 0 & any(!cadenceNA) &
      fixCadence & tolower(cadCorrectNA) %in% c("smooth","zero") ) {
    if (loud)
      cat("fixing (using ",cadCorrectNA,") ",
          sum(cadenceNA)," missing cadence values\n",sep="")
    if (tolower(cadCorrectNA) == "smooth") {
      cadenceSmoothed <- smoothDataSegments(yvec=trackdf$cadence.rpm,
                                            xvar=cumsum(trackdf$deltatime),
                                            segment=trackdf$segment,
                                            bw=cadCorrectWindowSec,
                                            nneighbors=cadCorrectWindowSec,
                                            kernel="triangular",
                                            replaceNAs=TRUE)
      trackdf$cadence.rpm[cadenceNA] <- cadenceSmoothed[cadenceNA]
      cadChanged <- TRUE
    } else if (tolower(cadCorrectNA) == "zero") {
      trackdf$cadence.rpm[cadenceNA] <- 0
      cadChanged <- TRUE
    }
  }
  #if (!cadChanged) trackdf$cadence.uncorrected <- NULL
  return(list(trackdf=trackdf,nCadMissingtoZero=nCadMissingtoZero,
              nCadTooHigh=nCadTooHigh,nCadTooLow=nCadTooLow,
              nCadStoppedPos=nCadStoppedPos,nCadStuck=nCadStuck))
}
zerosinrange <- function(vec,begin,end) {
  if ((begin > end) | (begin < 1) | (end > length(vec))) {
    FALSE
  } else {
    t <- vec[begin:end] == rep(0,end-begin+1)
    t[is.na(t)] <- FALSE
    max(t) == 1
  }
}
fixSensorMissing <- function(sensorvec,spd,elev,
                             initialNAs,midNAs,finalNAs,
                             varstr="",loud)  {

  nchanged <- 0
  sensorNAs <- is.na(sensorvec)
  if (sum(!sensorNAs) == 0)
    return(list(sensorvec=sensorvec,nchanged=0))

  firstNonNA <- which(!sensorNAs)[1]
  lastNonNA <- which(!sensorNAs)[length(which(!sensorNAs))]
  midNonNAs <- sum(is.na(sensorvec[firstNonNA:lastNonNA]))
  if (firstNonNA > 1) {
    sensorvec[1:(firstNonNA-1)] <-
      NApatch(sensorvec[1:(firstNonNA-1)],spd[1:(firstNonNA-1)],
              elev[1:(firstNonNA-1)],how=initialNAs)
    if (!is.na(sensorvec[1])) {
      nchanged <- nchanged + firstNonNA -1
      if (loud) print(paste0(firstNonNA-1,
                             " initial ",varstr,
                             " missing values set to zero using rule ",
                             initialNAs))
    }
  }
  if (midNonNAs > 0 & as.integer(midNAs) > 0) {
    temp <- rle(is.na(sensorvec[firstNonNA:lastNonNA]))
    starts <- cumsum(temp[["lengths"]])
    vstartNAs <- firstNonNA - 1 + starts[temp[["values"]]==TRUE]
    vlengthNAs <- firstNonNA - 1 + temp[["lengths"]][temp[["values"]]==TRUE]
    znavec <- is.na(sensorvec)
    znavec[sensorvec == 0] <- TRUE
    for (i in 1:as.integer(midNAs)) {
      znavec <- znavec & c(TRUE,znavec[-length(znavec)])
    }
    if (firstNonNA > 1)
      znavec[1:(firstNonNA-1)] <- FALSE
    if (lastNonNA < length(sensorvec))
      znavec[(lastNonNA+1):length(znavec)] <- FALSE
    sensorvec[znavec & is.na(sensorvec)] <- 0
    nfixed <- midNonNAs - sum(is.na(sensorvec[firstNonNA:lastNonNA]))
    if (nfixed > 0) {
      nchanged <- nchanged + nfixed
      if (loud) print(paste0(nfixed,
                             " interior ",varstr,
                             " missing values set to zero using window ",
                             midNAs))
    }
  }
  if (lastNonNA < length(sensorvec)) {
    sensorvec[(lastNonNA+1):length(sensorvec)] <-
      NApatch(sensorvec[(lastNonNA+1):length(sensorvec)],
              spd[(lastNonNA+1):length(sensorvec)],
              elev[(lastNonNA+1):length(sensorvec)],how=finalNAs)
    if (!is.na(sensorvec[length(sensorvec)])) {
      nchanged <- nchanged + length(sensorvec)-lastNonNA
      if (loud) print(paste0(length(sensorvec)-lastNonNA,
                             " final ",varstr,
                             " missing values set to zero using rule ",
                             finalNAs))
    }
  }
  return(list(sensorvec=sensorvec,nchanged=nchanged))
}
NApatch <- function(NAvec,svec,evec,how) {
  if (how == "all") {
    return(rep(0,length(NAvec)))

  } else if (how == "stopped") {
    if (max(svec <- 0.05)) {
      return(rep(0,length(NAvec)))
    } else {
      return(NAvec)
    }

  } else if (how == "downslope") {
    if (length(NAvec)==1) {
      downflat <- TRUE
    } else {
      downflat <-
        c(TRUE, (evec[-1] - evec[-length(evec)]) <= 0)
    }
    testvec <- downflat | (svec <= 0.05)
    if (min(testvec) > 0) {
      return(rep(0,length(NAvec)))
    } else {
      return(NAvec)
    }

  } else {
    return(NAvec)
  }
}

