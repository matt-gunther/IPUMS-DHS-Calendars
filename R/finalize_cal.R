#' @title Finish: Final Processing Steps for Calendar Data
#' @author Matt Gunther
#' @description Run this function after you have completed all of the vcal
#' functions in this package (a check will be run to ensure that all of the
#' expected variables are present, even if all values are NA). Returns a data
#' frame and writes a compressed CSV file by default.
#' @param dat A data file that has been passed to \emph{all} of the vcal
#' functions in this package.
#' @param write Logical: write file as compressed CSV? (defaults TRUE)
#' @param path Optional Character: path to the folder "unlinked_lr_data". If
#' not provided and write == TRUE, a file will be written to \code{
#' general/calendar/unlinked_lr_data} in the \code{dhs} folder.
#' @export finalize_cal
finalize_cal <- function(
  dat,
  write = TRUE,
  path = NULL
){

  # test for all of the expected variables
  names <- c(
    "caseid", "id", "cmc_month", "vcal_reprod", "reason",
    "vcal_marstat","vcal_mig","vcal_fpsource", "vcal_term",
    "vcal_ppa", "vcal_bfeed","vcal_ppabstain", "vcal_sep",
    "vcal_work", "vcal_ultra","vcal_aborplace", "caseid_cmc", "seq",
    "birth","preg", "term", "contr", "eventpbt","eventwfp","contr_start",
    "contr_change","contr_stop","contr_stop_total","preg_total","birth_total",
    "term_total","contr_total", "preg_rc","preg_lc", "preg_length",
    "preg_long"
  )
  if(!all(names %in% names(dat))){
    cal_setdiff <- setdiff(names, names(dat))
    warning(
      "The following variables have not been created: \n\n",
      paste(cal_setdiff, collapse = "\n")
    )
  }

  # cleanup
  dat <- dat %>%
    filter(cmc_month <= v008) %>%  # remove months after interview
    select(-c(                     # remove source vars
      starts_with("vcal") & matches("[0-9$]"),
      starts_with("v0"),
      sample
    )) %>%
    rename_with(                   # prepend names with "cal"
      ~paste0("cal", .x),
      -c(starts_with("vcal"), caseid)
    )

  # write output
  if(write == F){
    return(dat)
  } else {
    if(is.null(path)){
      path <- attr(dat, "dhs_path") %>%
        file.path("general/calendar/programming/unlinked_lr_data")
    }
    if(dir.exists(path)){
      dat %>%
        mutate(across(is.logical, ~as.numeric(.x))) %>%
        write_csv(
          file.path(path, gsub("ir", "_lr.csv.gz", attr(dat, "sample"))),
        )
    } else {
      stop("Attempted to write file to non-existing path: \n", path)
    }
  }

}

