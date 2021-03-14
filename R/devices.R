#' process sensor data from fit files
#'
#' \code{devices} process sensor data from fit files.
#'
#'
#' @param device_info a dataframe of device info messages
#' @param cadenceFromPower cadence signal came from powermeter - specify
#'   one of "always","never", or "auto"
#' @param noncadPowermeters a vector of manufacturer id strings for power meters which
#'   do NOT provide the cadence signal, possibly overriding any cadence sensors
#' @param loud display summary of re/segmenting actions
#'
#' @return a list of hardware info
#'
#' @seealso \code{\link{read_ride}}
#'
#' @export
devices <- function(device_info,
                    cadenceFromPower="auto",noncadPowermeters=c("ibike"),
                    loud=FALSE) {
  if (is.null(device_info)) {
    return(NULL)
  } else {
    headunit <- device_info %>%
      dplyr::filter(device_index=="creator") %>%
      dplyr::select(c("garmin_product","serial_number","software_version")) %>%
      dplyr::slice(1)

    if ("antplus_device_type" %in% names(device_info)) {
      ant_devs <- device_info %>%
        dplyr::filter(!is.na(antplus_device_type)) %>%
        dplyr::select(c("device_index","antplus_device_type",
                        "manufacturer","serial_number",
                        "hardware_version","software_version",
                        "timestamp")) %>%
        dplyr::filter(serial_number != -99999) %>%
        dplyr::filter(serial_number != 0 | !is.na(manufacturer) ) %>%
        dplyr::group_by(antplus_device_type,serial_number) %>%
        dplyr::summarize(device_type=mostcommon(antplus_device_type),
                         manufacturer=mostcommon(manufacturer),
                         serial_number=mostcommon(serial_number),
                         hardware_version=mostcommon(hardware_version),
                         software_version=mostcommon(software_version),
                         first_connect_time=first(timestamp),
                         nconnects=n()-1)
    } else {
      ant_devs <- device_info %>%
        dplyr::filter(device_type %in% c(11,120,121,122,123)) %>%
        dplyr::select(c("device_index","device_type",
                        "manufacturer","serial_number",
                        "software_version","timestamp")) %>%
        dplyr::mutate(device_type=case_when(device_type==11 ~ "bike_power",
                                            device_type==120 ~ "heart_rate",
                                            device_type==121 ~ "bike_speed_cadence",
                                            device_type==122 ~ "bike_cadence",
                                            device_type==123 ~ "bike_speed"),
                      hardware_version=NA) %>%
        dplyr::group_by(device_type,serial_number) %>%
        dplyr::summarize(device_type=mostcommon(device_type),
                         manufacturer=mostcommon(manufacturer),
                         serial_number=mostcommon(serial_number),
                         hardware_version=mostcommon(hardware_version),
                         software_version=mostcommon(software_version),
                         first_connect_time=first(timestamp),
                         nconnects=n()-1)
    }
    if(loud) print(as.data.frame(headunit))
    if(loud & nrow(ant_devs)> 0) print(as.data.frame(ant_devs))

    if ("bike_power" %in% ant_devs$device_type) {
      if (tolower(cadenceFromPower) == "always"){
        cadence_from_power <- TRUE
      } else if (tolower(cadenceFromPower) == "never") {
        cadence_from_power <- FALSE
      } else if (tolower(cadenceFromPower) == "auto") {
        dfnames <- names(ant_devs)
        dfdevs <- ant_devs$device_type
        powerrow <- min(which(dfdevs == "bike_power"))
        mfr <- ant_devs$manufacturer[powerrow]
        # powertap hubs and ibike (newton/powerpod) indirect force don't
        #  supply cadence signal to override any cadence sensor, most others do
        #  if no mfr available, assume power sensor gives cadence only if no
        #  other cadence sensor
        cadence_from_power <-
          ifelse(is.na(mfr),
                 #  if no info, assume from power if speed device and no cadence device
                 #  many rides with edge 800 have missing sensor check-ins
                 ((length(intersect(c("bike_speed_cadence","bike_cadence"),
                                    dfdevs)) == 0) &
                  (length(intersect(c("bike_speed_cadence","bike_speed"),
                                    dfdevs)) > 0)),
                 !(mfr %in% noncadPowermeters))
      } else {
        stop("unknown value for cadenceFromPower")
      }
    } else {
        cadence_from_power <- FALSE
    }
    cadencesource <- ifelse(cadence_from_power,"bike_power","bike_cadence")

    return(list(headunit.manufacturer=headunit[["garmin_product"]],
                headunit.software_version=headunit[["software_version"]],
                headunit.serial_number=headunit[["serial_number"]],
                hr.devicetype=getAntInfo("heart_rate","device_type",ant_devs),
                hr.manufacturer=getAntInfo("heart_rate","manufacturer",ant_devs),
                hr.serial_number=getAntInfo("heart_rate","serial_number",ant_devs),
                hr.hardware_version=getAntInfo("heart_rate","hardware_version",ant_devs),
                hr.software_version=getAntInfo("heart_rate","software_version",ant_devs),
                hr.connect_time=getAntInfo("heart_rate","first_connect_time",ant_devs),
                power.devicetype=getAntInfo("bike_power","device_type",ant_devs),
                power.manufacturer=getAntInfo("bike_power","manufacturer",ant_devs),
                power.serial_number=getAntInfo("bike_power","serial_number",ant_devs),
                power.hardware_version=getAntInfo("bike_power","hardware_version",ant_devs),
                power.software_version=getAntInfo("bike_power","software_version",ant_devs),
                power.connect_time=getAntInfo("bike_power","first_connect_time",ant_devs),
                speed.devicetype=getAntInfo("bike_speed","device_type",ant_devs),
                speed.manufacturer=getAntInfo("bike_speed","manufacturer",ant_devs),
                speed.serial_number=getAntInfo("bike_speed","serial_number",ant_devs),
                speed.hardware_version=getAntInfo("bike_speed","hardware_version",ant_devs),
                speed.software_version=getAntInfo("bike_speed","software_version",ant_devs),
                speed.connect_time=getAntInfo("bike_speed","first_connect_time",ant_devs),
                cadence.devicetype=getAntInfo(cadencesource,"device_type",ant_devs),
                cadence.manufacturer=getAntInfo(cadencesource,"manufacturer",ant_devs),
                cadence.serial_number=getAntInfo(cadencesource,"serial_number",ant_devs),
                cadence.hardware_version=getAntInfo(cadencesource,"hardware_version",ant_devs),
                cadence.software_version=getAntInfo(cadencesource,"software_version",ant_devs),
                cadence.connect_time=getAntInfo(cadencesource,"first_connect_time",ant_devs)
    ))
  }
}
getAntInfo <- function(devtype,fieldname,dframe) {
  #  power returned by power meter
  #  speed returned by speed or speed-cadence
  #  hr returned by hr
  #  cadence returned by power, cadence or speed-cadence
  dfnames <- names(dframe)
  dfdevs <- dframe$device_type
  if (devtype %in% c("bike_power","heart_rate")) {
    if (devtype %in% dfdevs) {
      fetchdevtype <- devtype
    } else {
      fetchdevtype <- NA
    }
  } else if (devtype %in% c("bike_speed","bike_cadence")) {
    if  (devtype %in% dfdevs) {
      fetchdevtype <- devtype
    } else if ("bike_speed_cadence" %in% dfdevs) {
      fetchdevtype <- "bike_speed_cadence"
    } else {
      fetchdevtype <- NA
    }
  }
  if (is.na(fetchdevtype)) {
    return(NA)
  } else {
    return(dframe[dframe$device_type==fetchdevtype,][[fieldname]])
  }
}
mostcommon <- function(vec) {
  uniqv <- unique(na.omit(vec))
  return(uniqv[which.max(tabulate(match(vec,uniqv)))])
}
