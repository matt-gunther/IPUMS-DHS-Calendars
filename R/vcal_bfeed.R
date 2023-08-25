#' @title Breastfeeding Calendar
#' @author Matt Gunther
#' @description Create all variables related to the Breastfeeding calendar
#' (see details). If this calendar was not included in the sample (or if some
#' of the required information is not available), all variables will still be
#'  created, but all values will be NA.
#' @details The following variables will be created using case logic provided
#' to the function \code{dplyr::case_when()}. Please note that
#' \code{case_when()} returns NA through implicit logic: \emph{if a "case"
#' exists and is not explicitly handled here, the value NA will be returned!}
#' \itemize{
#'   \item{
#'     \strong{breastf} Integer: may take one of three values, and is not
#'     available (NA) otherwise:
#'       \itemize{
#'         \item{1 - \code{vcal_bfeed} contains "X" or "x"}
#'         \item{2 - \code{vcal_bfeed} contains "N" or "n"}
#'         \item{0 - \code{vcal_bfeed} is 0}
#'       }
#'   }
#' }
#' @param dat A data file created by \code{vcal_reprod()} (may be passed to
#' any other function starting with "vcal" first).
#' @export vcal_bfeed
vcal_bfeed <- function(dat){
  # preserve attributes
  dhs_path <- attr(dat, "dhs_path")
  samp <- attr(dat, "sample")

  # make new variable(s)
  dat <- dat %>%
    mutate(breastf = case_when(
      grepl("x", vcal_bfeed, ign = T) ~ 1,
      grepl("N", vcal_bfeed, ign = T) ~ 2,
      vcal_bfeed == "0" ~ 0
    ))

  # Re-attach attributes
  attr(dat, "dhs_path") <- dhs_path
  attr(dat, "sample") <- samp

  return(dat)
}
