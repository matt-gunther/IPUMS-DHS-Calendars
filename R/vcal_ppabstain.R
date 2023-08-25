#' @title Post-partum Abstinence Calendar
#' @author Matt Gunther
#' @description Create all variables related to the Post-partum Abstinence
#' calendar
#' (see details). If this calendar was not included in the sample (or if some
#' of the required information is not available), all variables will still be
#'  created, but all values will be NA.
#' @details The following variables will be created using case logic provided
#' to the function \code{dplyr::case_when()}. Please note that
#' \code{case_when()} returns NA through implicit logic: \emph{if a "case"
#' exists and is not explicitly handled here, the value NA will be returned!}
#' \itemize{
#'   \item{
#'     \strong{ppabs} Logical: TRUE if \code{vcal_ppabstain} contains "X" or
#'     "x". FALSE if \code{vcal_ppabstain} is 0, and not available otherwise
#'     (NA).
#'   }
#' }
#' @param dat A data file created by \code{vcal_reprod()} (may be passed to
#' any other function starting with "vcal" first).
#' @export vcal_ppabstain
vcal_ppabstain <- function(dat){
  # preserve attributes
  dhs_path <- attr(dat, "dhs_path")
  samp <- attr(dat, "sample")

  # make new variable(s)
  dat <- dat %>%
    mutate(ppabs = case_when(
      grepl("x", vcal_ppabstain, ign = T) ~ T,
      vcal_ppabstain == "0" ~ F
    ))

  # Re-attach attributes
  attr(dat, "dhs_path") <- dhs_path
  attr(dat, "sample") <- samp
  return(dat)
}
