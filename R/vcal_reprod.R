#' @title Reproductive Events Calendar
#' @author Matt Gunther
#'
#' @description Create all variables related to the Reproductive Events
#' calendar (see details). If this calendar was not included in the sample (or
#' if some of the required information is not available), all
#' variables will \emph{still be created}, but all values will be NA. See
#' note regarding NA values for contraceptive use.
#'
#' @note All samples seem to include a Reproductive Events calendar, but
#' \strong{some samples do no include responses for monthly contraceptive use}
#' (you will find only "pregnancy", "birth", or "termination" events listed).
#' In these samples, it is common to see blank values in the original DHS
#' string; we interpret these \strong{blanks as months in which a reproductive
#' event did not occur} and recode them as the value 0.
#' For example, if the Reproductive Events calendar for a
#' woman in one of these samples contains \emph{only blanks}, we assume that
#' she has zero "pregnant" months. These samples include \strong{no NA values
#' for missing data} because missing data are indistinguishable from the
#' original blank DHS values
#' (possibly except for blanks in the middle of a continuous pregnancy).
#'
#' The majority of samples \strong{do contain responses for monthly
#' contraceptive use}, and these samples \strong{do contain NA values for
#' missing data} (e.g. a woman could not recall what method she used in a
#' particular month). For example, if the Reproductive Events calendar for a
#' woman in one of these samples contains \emph{only missing values}, we
#' assume that the woman was NIU (not in universe) for the Reproductive Events
#' calendar section of the survey (or else declined to provide any responses);
#' all months for such a woman will be marked NA (e.g. the calculated sum
#' for her total number of "pregnant" months will be NA).
#'
#' Because NA values may represent both "NIU" and "missing" depending on the
#' sample, \emph{the recommended value label for NA is for
#' monthly indicators is "No response or NIU"}. The interpretation for
#' personal totals or event summaries (e.g. length of pregnancy) can be treated
#' as "NIU" where the universe includes relevant months (e.g. months that
#' were part of a single, continuous pregnancy).
#'
#' @details The following variables will be created using case logic provided
#' to the function \code{dplyr::case_when()}. Please note that
#' \code{case_when()} returns NA through implicit logic: \emph{if a "case"
#' exists and is not explicitly handled here, the value NA will be returned!}
#'
#' \itemize{
#'   \item{
#'     \strong{vcal_reprod} Numeric: a recoded version of
#'     the Reproductive Events calendar (usually \code{vcal_1} in the IR file).
#'     All values are a number; both common and
#'     sample-specific codes are harmonized using vcal_reprod_recodes.csv
#'   }
#'
#'   \item{
#'     \strong{vcal_reprod_dhs} Character: The alphanumeric DHS codes used in
#'     the Reproductive Events calendar (usually \code{vcal_1} in the IR file).
#'     These are not likely to be published by IPUMS DHS, but they may be
#'     useful for quality checking.
#'   }
#'
#'   \item{
#'     \strong{caseid_cmc} concatenates \code{caseid} and \code{cmc_month}
#'     (separated by \code{_})
#'   }
#'
#'   \item{
#'     \strong{seq} month index, where the earliest month gets 1 and the
#'     final month usually gets 80 (length of the original calendar string)
#'   }
#'
#'   \item{
#'     \strong{birth} Logical: TRUE if \code{vcal_reprod} is 100,
#'     FALSE if it is any other non-missing value (NA otherwise).
#'   }
#'
#'   \item{
#'     \strong{preg} Logical: TRUE if \code{vcal_reprod} is 200,
#'     FALSE if it is any other non-missing value (NA otherwise).
#'   }
#'
#'   \item{
#'     \strong{term} Logical: TRUE if \code{vcal_reprod} is 300,
#'     FALSE if it is any other non-missing value (NA otherwise).
#'   }
#'
#'   \item{
#'     \strong{contr} Logical: TRUE if \code{vcal_reprod} is
#'     a value between 1 and 90, FALSE if it is any other non-missing value
#'     (NA otherwise).
#'   }
#'
#'   \item{
#'     \strong{eventpbt} Logical: TRUE if \code{birth}, \code{preg}, or
#'     \code{term} is TRUE, FALSE if they are all FALSE and non-missing
#'     (NA otherwise).
#'   }
#'
#'   \item{
#'     \strong{eventwfp} Logical: TRUE if \code{eventpbt} or \code{contr}
#'     is TRUE, FALSE if both are FALSE and non-missing (NA otherwise).
#'   }
#'
#'   \item{
#'     \strong{contr_start} Logical: TRUE if \code{vcal_reprod} is between 1:90
#'     and the previous month's value for \code{vcal_reprod} is between 0:90
#'     \emph{and} different from the current month. FALSE if \code{vcal_reprod}
#'     is between 1:90 and the previous month's value for \code{vcal_reprod}
#'     is the \emph{same} as the current month. NA in any other
#'     case (e.g. the first month of the recall timeline is NA;
#'     months where \code{vcal_reprod} is beyond 1:90 are NA;
#'     months where the previous month \code{vcal_reprod} is beyond
#'     0:90 are NA).
#'   }
#'
#'   \item{
#'     \strong{contr_change} Logical: TRUE if \code{contr_start} is TRUE
#'     \emph{and} \code{vcal_reprod} in the previous month \strong{is not}
#'     0 (i.e. the method was started following a month of non-use of any
#'     method). FALSE if \code{contr_start} is TRUE  \emph{and}
#'     \code{vcal_reprod} in the previous month \strong{is} 0. NA if
#'     \code{contr_start} is NA.
#'   }
#'
#'   \item{
#'     \strong{contr_stop} see \code{vcal_discont()}
#'   }
#'
#'   \item{
#'     \strong{preg_total} Integer: the total number of months per person
#'     where \code{preg} is TRUE (NA if all values for \code{preg} are NA). All
#'     months for the person reflect the same total (this is not a cumulative
#'     sum).
#'   }
#'
#'   \item{
#'     \strong{birth_total} Integer: the total number of months per person
#'     where \code{birth} is TRUE (NA if all values for \code{birth} are NA).
#'     All months for the person reflect the same total (this is not a
#'     cumulative sum).
#'   }
#'
#'   \item{
#'     \strong{term_total} Integer: the total number of months per person
#'     where \code{term} is TRUE (NA if all values for \code{term} are NA). All
#'     months for the person reflect the same total (this is not a cumulative
#'     sum).
#'   }
#'
#'   \item{
#'     \strong{contr_total} Integer: the total number of months per person
#'     where \code{contr} is TRUE (NA if all values for \code{contr} are NA).
#'     All months for the person reflect the same total (this is not a
#'     cumulative sum).
#'   }
#'
#'   \item{
#'   \strong{preg_rc} Logical: TRUE for all months in a continuous
#'     pregnancy if the outcome of the pregnancy (birth or termination)
#'     happened after the date of the interview. FALSE for all months in a
#'     continuous pregnancy otherwise. NA for all months that
#'     were not part of a continuous pregnancy.
#'   }
#'
#'   \item{
#'     \strong{preg_lc} Logical: TRUE for all months in a continuous
#'     pregnancy (including months of birth or termination) that overlaps
#'     with the first month in the recall timeline. FALSE for all months in a
#'     continuous pregnancy otherwise. NA for months that were not part of a
#'     continuous pregnancy.
#'   }
#'
#'   \item{
#'     \strong{preg_length} Integer: the total number of months included
#'     in a single continuous pregnancy (including months of birth or
#'     termination) where \emph{both} \code{preg_rc} and
#'     \code{preg_lc} are FALSE. All months in the pregnancy reflect the
#'     same total (this is not a cumulative sum). NA for
#'     months that were not part of a continuous pregnancy.
#'   }
#'
#'   \item{
#'     \strong{preg_long} Logical: TRUE if \code{preg_length} is longer
#'     than 9 months. NA for months where \code{preg_length}
#'     is not available.
#'   }
#' }
#' @param dat A data file created by \code{reshape_vcal_input_data()}
#' @param vcal_reprod_recodes Optional Character: the full path to a file
#' called vcal_reprod_recodes.csv (contains all common and sample-specific
#' recodes for the Reproductive Events calendar).
#' @export vcal_reprod
vcal_reprod <- function(
  dat,
  vcal_reprod_recodes = NULL
){

  # preserve attributes
  dhs_path <- attr(dat, "dhs_path")
  samp <- attr(dat, "sample")

  # get vcal_reprod_recodes.csv
  if(is.null(vcal_reprod_recodes)){
    vcal_reprod_recodes <- attr(dat, "dhs_path") %>%
      file.path("general/calendar/sample_tracking/vcal_reprod_recodes.csv")
  }
  if(!file.exists(vcal_reprod_recodes)){
    stop(
      "I could not find the vcal_reprod_recodes file at \n",
      vcal_reprod_recodes,
      "\n\n",
      "Please make sure that it has not moved from that location."
    )
  }  else {
    vcal_reprod_recodes <- suppressMessages(read_csv(vcal_reprod_recodes))
  }

  # preserve the original DHS codes as `vcal_reprod_dhs`
  dat <- dat %>% mutate(vcal_reprod_dhs = vcal_reprod)

  # recode vcal_reprod using the recode CSV file
  dat <- vcal_reprod_recodes %>%
    filter(
      sample %in% c("all", dat$sample),
      year == "all" | year %in% dat$v007
    ) %>%
    filter(!duplicated(input, fromLast = T)) %>%
    select(input, output) %>%
    rename(vcal_reprod = input) %>%
    right_join(dat, by = "vcal_reprod") %>%
    mutate(vcal_reprod = if_else(
      is.na(output),
      suppressWarnings(as.numeric(vcal_reprod)),
      output
    )) %>%
    select(-output) %>%
    relocate(vcal_reprod, .after = cmc_month) %>%
    arrange(id, desc(cmc_month))

  # Is contraceptive availability included in the string?
  contr_avail <- any(dat$vcal_reprod < 90 & !is.na(dat$vcal_reprod))

  # Is information about pregnancy / birth / termination included?
  eventpbt_avail <- any(dat$vcal_reprod %in% 100:300 & !is.na(dat$vcal_reprod))

  # Monthly indicators
  dat <- dat %>%
    mutate(
      caseid_cmc = paste0(caseid, "_", cmc_month),
      seq = as.integer(cmc_month - v017 + 1),
      birth = vcal_reprod == 100,
      preg = vcal_reprod == 200,
      term = vcal_reprod == 300,

      # revise birth, preg, & term for special samples (B,P,T only):
      # if !contr_avail, NA values are assumed to be FALSE,
      # if contr_avail, NA values are assumed to be missing / NIU (still NA)
      across(
        c(birth, preg, term),
        ~case_when(
          is.na(vcal_reprod) & !contr_avail ~ FALSE,
          T ~ .x
        )
      ),

      contr = case_when(
        contr_avail & !is.na(vcal_reprod) ~ vcal_reprod %in% 1:90
      ),
      eventpbt = case_when(eventpbt_avail ~ birth | preg | term),
      eventwfp = eventpbt | contr,
    )

  # Person-wise totals
  dat <- dat %>%
    group_by(caseid) %>%
    mutate(
      # contraceptive use episodes
      contr_start = case_when(
        contr &
          {lead(vcal_reprod) != vcal_reprod} &
          {lead(vcal_reprod) < 90} ~ T,
        contr &
          {lead(vcal_reprod) == vcal_reprod} ~ F
      ),
      contr_change = case_when(
        contr_start == T & lead(vcal_reprod) > 0 ~ T,
        contr_start == T & lead(vcal_reprod) == 0 ~ F,
        T ~ contr_start
      ),
      contr_stop = case_when(
        contr & lag(vcal_reprod) != vcal_reprod ~ T,
        contr & lag(vcal_reprod) == vcal_reprod ~ F
      ),

      # count totals, treating NA as 0
      preg_total = case_when(eventpbt_avail ~ sum(eventpbt, na.rm = T)),
      birth_total = case_when(eventpbt_avail ~ sum(birth, na.rm = T)),
      term_total = case_when(eventpbt_avail ~ sum(term, na.rm = T)),
      contr_total = case_when(contr_avail ~ sum(contr, na.rm = T)),

      # revise totals: if all values were NA for the woman, change
      #   her total from 0 to NA (reflecting NIU)
      preg_total = case_when(!all(is.na(eventpbt)) ~ preg_total),
      birth_total = case_when(!all(is.na(birth)) ~ birth_total),
      term_total = case_when(!all(is.na(term)) ~ term_total),
      contr_total = case_when(!all(is.na(contr)) ~ contr_total)
    ) %>%
    ungroup()

  # Pregnancy-wise variables
  dat <- dat %>%
    filter(eventpbt) %>%
    select(id, cmc_month, v008, v017, preg, birth, term) %>%
    group_by(id) %>%
    mutate(
      preg_end = birth | term | (preg & cmc_month == v008),
      preg_rc = case_when(birth | term ~ F, preg_end ~ T),
      preg_lc = case_when(cmc_month == v017 ~ T),
      preg_count = preg_end & !preg_rc,
      preg_count = cumsum(preg_count)
    ) %>%
    group_by(id, preg_count) %>%
    mutate(
      preg_rc = preg_count == 0, # flags ALL months in rc preg, not just last
      preg_lc = any(preg_lc, na.rm = T), # flags ALL months in lc preg
      preg_length = case_when(
        preg_rc == F & preg_lc == F ~ sum(preg) + sum(birth, term, na.rm = T)
      ),
      preg_long = preg_length > 9
    ) %>%
    ungroup() %>%
    select(id, cmc_month, preg_rc, preg_lc, preg_length, preg_long) %>%
    full_join(dat, ., by = c("id", "cmc_month"))

  # Revise vcal_reprod for special samples (B,P,T only)
  # if !contr_avail, NA values are given code 0: non-missing, non-event
  # if contr_avail, no action: code 0 is non-event & non-use of FP
  dat <- dat %>%
    mutate(vcal_reprod = case_when(
      is.na(vcal_reprod) & !contr_avail ~ 0,
      TRUE ~ vcal_reprod
    ))

  # Re-attach attributes
  attr(dat, "dhs_path") <- dhs_path
  attr(dat, "sample") <- samp

  return(dat)
}
