#' @title Make a long (LR) file
#' @author Matt Gunther
#' @description This function is a convenience wrapper for all of the other
#' functions in this package. It will run them all in the correct order, and
#' you may pass a number of arguments to help find files or write to a custom
#' location. The only required input is "sample", and the result will be a
#' compressed CSV file.
#' @param sample The name of a valid DHS IR sample (e.g. "et2016ir")
#' @param write Logical: write file as compressed CSV? (defaults TRUE). If
#' FALSE, a data frame will be returned to the console.
#' @param dhs_path Optional: you can help this package find the IPUMS DHS
#' folder if you're working in a local version of R (but working on an MPC
#' server like rstudio.pop.umn.edu makes this unnecessary). If you provide this
#' information, none of the remaining arguments will be necessary (as long as
#' you are OK with the defaults).
#' @param output_override Optional Character: path to the folder
#' "unlinked_lr_data". If not provided and write == TRUE, a file will be
#' written to \code{general/calendar/unlinked_lr_data} in the \code{dhs} folder.
#' @param vcal_availability Optional Character: the full path to a file called
#' vcal_availability.csv (this can usually be found automatically).
#' @param vcal_reprod_recodes Optional Character: the full path to a file
#' called vcal_reprod_recodes.csv, which contains all common and sample-specific
#' recodes for the Reproductive Events calendar (this can usually be found
#'  automatically).
#' @param vcal_discont_recodes Optional Character: the full path to a file
#' called vcal_discont_recodes.csv, which contains all common and
#' sample-specific recodes for the Discontinuation calendar (this can usually
#' be found automatically).
#' @param pad_months Optional Named List: if necessary, the user can specify a
#' list of calendar variables in the IR file that need to be padded with left
#' or right spaces to reach a desired length (usually 80). Note that this
#' function will generate an error if all calendars are \emph{not} the same
#' length (this will help you determine whether use of pad_months is needed).
#' For example: \code{
#' reshape_vcal_input_data(dat, list(vcal_1 = c(80, 'right')))}
#' @export make_lr
make_lr <- function(
  sample,
  write = TRUE,
  dhs_path = NULL,
  output_override = NULL,
  vcal_availability = NULL,
  vcal_reprod_recodes = NULL,
  vcal_discont_recodes = NULL,
  pad_months = NULL
){
  sample %>%
    get_vcal_input_data(dhs_path = dhs_path) %>%
    reshape_vcal_input_data(
      vcal_availability = vcal_availability,
      pad_months = pad_months
    ) %>%
    vcal_reprod(vcal_reprod_recodes = vcal_reprod_recodes) %>%
    vcal_discont(vcal_discont_recodes = vcal_discont_recodes) %>%
    vcal_ppa() %>%
    vcal_ppabstain() %>%
    vcal_bfeed() %>%
    vcal_marstat() %>%
    vcal_mig() %>%
    vcal_work() %>%
    vcal_term() %>%
    vcal_ultra() %>%
    finalize_cal(write = write, path = output_override)
}
