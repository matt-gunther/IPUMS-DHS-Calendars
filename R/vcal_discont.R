#' @title Reasons for Discontinuation of Contraceptive Use
#' @author Matt Gunther
#'
#' @description  Create all variables related to the Discontinuation
#' calendar (see details). If this calendar was not included in the sample (or
#' if some of the required information is not available), all
#' variables will \emph{still be created}, but all values will be NA. See note
#' regarding interpretation guidelines for NA values.
#'
#' @note The Discontinuation calendar is used in conjunction with the
#' Reproductive Event calendar to identify months where a woman stopped using
#' a method of contraception (this appears on the last month of continuous
#' use). If a woman gave a "reason for discontinuation" for a given month, she
#' will be said to have "stopped" in that month. An NA value appears if the
#' woman was not using contraception in a given month, or if she provided no
#' contraceptive use information at all (either because she was NIU, provided
#' no responses, or because certain samples did not ask about contraceptive
#' use).
#'
#' Some samples do not include a Discontinuation calendar. In those cases,
#' the month were a woman stopped using a method of contraception can still
#' be determined for samples where monthly contraceptive use is recorded in the
#' Reproductive Events calendar. If neither the current month nor the next
#' future month is missing, a woman will be said to have "stopped" if in the
#' next month she switched methods, became pregnant, or began using no method at
#' all. For these cases, an NA value indicates that cessation could not be
#' determined (e.g. one or both months has a missing response).
#'
#' Because NA values may represent both "NIU" and "missing" depending on the
#' sample, \emph{the recommended value label for NA is for
#' this variable is "No response or NIU"}. The interpretation for
#' NA values in the total number of "stop" months can be treated
#' as "NIU" where the universe includes women who reported using contraception
#' in at least one month.
#'
#' @details The following variables will be created using case logic provided
#' to the function \code{dplyr::case_when()}. Please note that
#' \code{case_when()} returns NA through implicit logic: \emph{if a "case"
#' exists and is not explicitly handled here, the value NA will be returned!}
#' \itemize{
#'   \item{
#'     \strong{vcal_discont} Numeric: a recoded version of the Discontinuation
#'     calendar (usually \code{vcal_2} in the IR file).
#'     All values are a number, both common and
#'     sample-specific codes are harmonized using vcal_discont_recodes.csv
#'   }
#'
#'   \item{
#'     \strong{vcal_discont_dhs} Character: The alphanumeric DHS codes used in
#'     the Discontinuation calendar (usually \code{vcal_2} in the IR file).
#'     These are not likely to be published by IPUMS DHS, but they may be
#'     useful for quality checking.
#'   }
#'
#'   \item{
#'     \strong{reason} Numeric: an exact copy of \code{vcal_discont}
#'   }
#'
#'   \item{
#'     \strong{contr_stop} Logical: TRUE if either of these conditions
#'     are met:
#'       \itemize{
#'         \item{
#'           \code{vcal_reprod} is recoded 1:90 \emph{and} the next future
#'           month's value for \code{vcal_reprod} is any other non-missing
#'           value (i.e. the method used in the current month was not used
#'           in the next future month). Not available if \code{vcal_reprod}
#'           is not available for either month (e.g. excludes the month of
#'           the interview).
#'         }
#'         \item{
#'           \code{vcal_disccont} is any non-missing value (i.e. a reason
#'           was given for discontinuation). Not available if \code{
#'           vcal_discont} is not available.
#'         }
#'       }
#'     \code{contr_stop} is only not available (NA) if both criteria are
#'     not available. In the rare case that the criteria lead to opposite
#'     conclusions, the first criterion takes precedent.
#'   }
#'
#'   \item{
#'     \strong{contr_stop_total} Integer: total number of months per person
#'     where \code{contr_stop} is TRUE. NA if all months of \code{contr_stop}
#'     are NA (e.g. never used contraception, or contraception not included
#'     in the sample questionnaire).
#'     All months for the person reflect the same total (this is not a
#'     cumulative sum).
#'   }
#' }
#' @param dat A data file created by \code{vcal_reprod()}
#' @param vcal_discont_recodes Optional Character: the full path to a file
#' called vcal_discont_recodes.csv (contains all common and sample-specific
#' recodes for the Discontinuation calendar).
#' @export vcal_discont
vcal_discont <- function(
  dat,
  vcal_discont_recodes = NULL
){

  # preserve attributes
  dhs_path <- attr(dat, "dhs_path")
  samp <- attr(dat, "sample")

  # preserve the original DHS codes as `vcal_reprod_dhs`
  dat <- dat %>% mutate(vcal_discont_dhs = vcal_discont)

  # get vcal_discont_recodes.csv
  if(is.null(vcal_discont_recodes)){
    vcal_discont_recodes <- attr(dat, "dhs_path") %>%
      file.path("general/calendar/sample_tracking/vcal_discont_recodes.csv")
  }
  if(!file.exists(vcal_discont_recodes)){
    stop(
      "I could not find the vcal_discont_recodes file at \n",
      vcal_discont_recodes,
      "\n\n",
      "Please make sure that it has not moved from that location."
    )
  }  else {
    vcal_discont_recodes <- suppressMessages(read_csv(vcal_discont_recodes))
  }

  dat <- dat %>%
    # revise contr_stop (defined with vcal_reprod):
    # if this calendar is available, use it instead of our calculated version.
    # however, if the woman's response to vcal_reprod is missing, this should
    # be missing, too.
    mutate(contr_stop = case_when(
      contr & !all(is.na(vcal_discont)) ~ vcal_discont != " ",
      T ~ contr_stop # contr_stop not available | contr_stop already defined
    )) %>%
    group_by(caseid) %>%
    # contr_stop_total is total number of contr_stop per person, if available
    mutate(contr_stop_total = case_when(
      any(contr) & !all(is.na(contr_stop)) ~ sum(contr_stop, na.rm = T)
    )) %>%
    ungroup

  # recode vcal_discont using the recode CSV file
  # rename vcal_discont as `reason`
  dat <- vcal_discont_recodes %>%
    filter(
      sample %in% c("all", dat$sample),
      year == "all" | year %in% dat$v007
    ) %>%
    filter(!duplicated(input, fromLast = T)) %>%
    select(input, output) %>%
    rename(vcal_discont = input) %>%
    right_join(dat, by = "vcal_discont") %>%
    mutate(vcal_discont = if_else(
      is.na(output),
      suppressWarnings(as.numeric(vcal_discont)),
      output
    )) %>%
    select(-output) %>%
    relocate(vcal_discont, .after = vcal_reprod) %>%
    arrange(id, desc(cmc_month)) %>%
    mutate(reason = vcal_discont)

  # Re-attach attributes
  attr(dat, "dhs_path") <- dhs_path
  attr(dat, "sample") <- samp

  return(dat)
}
