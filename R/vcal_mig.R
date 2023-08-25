#' @title Place of Residence
#' @author Matt Gunther
#' @description Create all variables related to the Place of Residence calendar
#' (see details). If this calendar was not included in the sample (or if some
#' of the required information is not available), all variables will still be
#'  created, but all values will be NA.
#' @details The following variables will be created using case logic provided
#' to the function \code{dplyr::case_when()}. Please note that
#' \code{case_when()} returns NA through implicit logic: \emph{if a "case"
#' exists and is not explicitly handled here, the value NA will be returned!}
#' \itemize{
#'   \item{
#'     \strong{move} Logical: TRUE if \code{vcal_mig} contains "X" or "x".
#'     FALSE if \code{vcal_mig} is available, but blank (i.e. " "). Otherwise,
#'     not available (NA).
#'   }
#'   \item{
#'     \strong{urban} Integer: records the value of \code{vcal_mig} if
#'     \code{vcal_mig} is an integer between 1 and 4. Otherwise, it is not
#'     available (NA).
#'   }
#' }
#' @param dat A data file created by \code{vcal_reprod()} (may be passed to
#' any other function starting with "vcal" first).
#' @export vcal_mig
vcal_mig <- function(dat){
  # preserve attributes
  dhs_path <- attr(dat, "dhs_path")
  samp <- attr(dat, "sample")

  # make new variable(s)
  dat <- dat %>%
    mutate(
      move = case_when(
        grepl("x", vcal_mig, ign = T) ~ T,
        vcal_mig != " " ~ F
      ),
      urban = case_when(
        vcal_mig == "1" ~ 1,
        vcal_mig == "2" ~ 2,
        vcal_mig == "3" ~ 3,
        vcal_mig == "4" ~ 4,
      )
    )

  # Re-attach attributes
  attr(dat, "dhs_path") <- dhs_path
  attr(dat, "sample") <- samp
  return(dat)
}
