#' @title Apply exclusion criterion to data
#' @description By default, there are three criterion, age criterion, duration
#'   criterion, and frequency criterion.
#' @param preDat  A \emph{data.table} which should be the output of
#'   \code{\link{datapreparation}}
#' @param crtVec A boolean vector of length 3, indicating whether to exclude
#'   patients by length of stay, frequency of
#' @export
PerformExclusion <- function(preDat, crtVec = c(crt.age = TRUE, # Need to provide either birth date as BIRTH.DATE or age as AGE, need only provide one.
                                                crt.los = TRUE, crt.freq = TRUE, crt.1stday = TRUE), ageRange = c(16, 120)){
  # Follow the exclusion criterion
  # so criterionVec is an logic vector where each entry tells whether to perform the criteria
  # entry 1: age criteria
  # entry 2: duration criteria
  # entry 3: frequency criteria

  if (nrow(preDat) == 0) {
    return(NULL) # check if there is data to check
  }
  crt.age = NULL
  crt.los = NULL
  crt.freq = NULL
  crt.1stday = NULL
  preDat[, NO.OF.READINGS := .N, by = list(PATIENT.NO, EPISODE.ID)]
  if(crtVec[1]){

    # I.a) First exclusion: Remove those patient episodes with age < 16 years old

    if (!is.null(preDat$BIRTH.DATE)) {
      preDat[, AGE := (difftime(
        as.Date(RESULT.DATE, format = "%m-%d-%y"),
        as.Date(BIRTH.DATE),
        units = "days"
      ) / 365)]
    }

    #************ Modified: check whether there is an AGE column
    if (is.null(preDat$AGE)) {
      crt.age <- NA
    } else if (length(which(preDat$AGE != NA)) != 0) {
      preDat <- preDat[AGE >= 16,]# not available for current data
      crt.age <- nrow(unique(preDat[preDat$AGE < 16, c("PATIENT.NO","EPISODE.ID"), with = FALSE]))
    } else {
      crt.age <- NA
    }

  }

  if(crtVec[2]){
    # I.b) Second exclusion: Remove those patient episosdes with only 24 hours LOS

    preDat[, DURATION := (diff(range(as.double(.SD$RESULT.DATE))) / 3600),
           by = list(PATIENT.NO, EPISODE.ID)]

    crt.los <- nrow(unique(preDat[preDat$DURATION <= 24, c("PATIENT.NO","EPISODE.ID"), with = FALSE]))


  }

  if(crtVec[3]){
    # I.c) Third exclusion: Remove those patient episodes with less than 5 BG measurements
    if (nrow(preDat) == 0) {
      #for check purpose
      print(w)
      return(NULL)
    }
    crt.freq <- nrow(unique(preDat[preDat$NO.OF.READINGS <= 4, c("PATIENT.NO","EPISODE.ID"), with = FALSE]))

  }
  exlM <-
    c(
      crt.age = crt.age,
      crt.los = crt.los,
      crt.freq = crt.freq,
      totalcases = nrow(unique(preDat[, c("PATIENT.NO","EPISODE.ID"), with = FALSE]))
    )

  if(crtVec[1] & (!is.null(preDat$AGE))){
    preDat <- preDat[NO.OF.READINGS > 4 |(DURATION > 24) | (AGE < 16), ]
  }else{
    preDat <- preDat[NO.OF.READINGS > 4 |(DURATION > 24) , ]
  }

  if (crtVec[4]) {
    # version 2 means remove the first 24 hours results from a patient stay
    preDat <- preDat[preDat$LOS.PSUM >24, ]
  }

  out <- list(exlM, preDat)
  return(out)

}
