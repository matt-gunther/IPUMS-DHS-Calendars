#' @title Reshape calendar data: one row per woman-month
#' @author Matt Gunther
#' @description Returns a data frame where the calendar strings have been
#' parsed into single characters, then pivoted "from wide to long" so that
#' each character gets stored in a separate row (the number of rows in the
#' data frame usually grows by a factor of 80). Several different tests are
#' conducted to ensure that all available calendars are identified and
#' reshaped correctly. If the user is aware that one or more calendar columns
#' contains the wrong number of months (e.g. 79, while all others have 80),
#' the user may pad the offending calendar(s) with spaces via the \code{
#' pad_months} argument.
#' @details This function will stop and issue a specific error message (with
#' instructions for next steps) if any of the following issues are found:
#'
#' \itemize{
#'   \item{The file vcal_availability.csv cannot be found}
#'   \item{The file vcal_availability.csv does not contain the sample}
#'   \item{The file vcal_availability.csv does not include all of the
#'   available calendars found in the data dictionary for the sample}
#'   \item{The 'CMC start of calendar' variable (v017) contains multiple values
#'   (a non-fatal warning is issued, and the function will align the
#'   calendars - results should be manually verified)}
#'   \item{Individual persons have different calendar lengths (usually 80).
#'   This would indicate a problem with DHS processing.}
#'   \item{One or more of the calendars in a sample have different lengths
#'   (usually 80). Note: this may be resolved with the pad_months argument.}
#'   \item{The user has requested padding for a calendar named in pad_months,
#'   but the name of the requested calendar does not exist}
#'   \item{The user has requested padding for a calendar named in pad_months,
#'   but did not specify the desired length of the string for the requested
#'   calendar}
#'   \item{The user has requested padding for a calendar named in pad_months,
#'   but did not specify whether to pad the calendar with spaces on the left
#'   or right side of the existing string}
#' }
#' @note As an intermediate processing step, all existing calendar names
#' (e.g. vcal_1, vcal_2, ... vcal_9) will be replaced with human-readable
#' names (e.g. vcal_reprod, vcal_discont, ... vcal_ultra) based on the
#' availability of calendars shown in vcal_availability.csv. Any calendars
#' that are \emph{not} available for the sample will be attached as
#' placeholders, where all values are NA. All calendar variables will be
#' removed from the data in a later function.
#' @param dat An IR data file created by \code{get_vcal_input_data()}
#' @param pad_months Optional Named List: if necessary, the user can specify a
#' list of calendar variables in the IR file that need to be padded with left
#' or right spaces to reach a desired length (usually 80). Note that this
#' function will generate an error if all calendars are \emph{not} the same
#' length (this will help you determine whether use of pad_months is needed).
#' For example: \code{
#' reshape_vcal_input_data(dat, list(vcal_1 = c(80, 'right')))}
#' @param vcal_availability Optional Character: the full path to a file called
#' vcal_availability.csv (this can usually be found automatically)
#' @export reshape_vcal_input_data
reshape_vcal_input_data <- function(
  dat,
  pad_months = NULL,
  vcal_availability = NULL
){

  # Read the sample tracking sheet from the DHS folder
  # Note: dhs_path is an attribute of the data frame returned by
  ## get_vcal_input_data.R
  if(is.null(vcal_availability)){
    vcal_availability <- attr(dat, "dhs_path") %>%
      file.path("general/calendar/sample_tracking/vcal_availability.csv")
  }
  if(!file.exists(vcal_availability)){
    stop(
      "I could not find the vcal_availability file at \n",
      vcal_availability,
      "\n\n",
      "Please make sure that it has not moved from that location."
    )
  }  else {
    sample_tracking <- suppressMessages(read_csv(vcal_availability))
  }

  # Create any vcal numbered 0-9 if it does not exist, then fill with NA
  dat <- dat %>%
    select(starts_with("vcal")) %>%
    names() %>%
    setdiff(paste0("vcal_", 0:9), .) %>%
    map_dfc(~tibble(!!.x := NA)) %>%
    bind_cols(dat, .)

  # Known padding issues as of July 2021
  # For these samples, the argument pad_month is not needed
  # We create a temporary variable vcal_0 in case there are unusual cal names
  dat <- dat %>%
    mutate(
      vcal_0 = ifelse(v000 == "EG3", str_pad(scal, 80, "left"), vcal_0),
      vcal_2 = ifelse(v000 == "LB6", str_pad(vcal_2, 80, "right"), vcal_2),
      vcal_6 = ifelse(v000 == "EG4", str_pad(vcal_6, 80, "right"), vcal_6),
      vcal_7 = ifelse(v000 == "EG4", str_pad(vcal_7, 80, "right"), vcal_7)
    ) %>%
    select(-any_of("scal"))

  # pad_months
  if(!is.null(pad_months)){
    if(!is_list(pad_months)){
      stop(
        "If you want to pad months to a certain length, you must specify ",
        "which calendars need to be padded, along with their desired ",
        "length and whether to pad the left or right side of the string. \n\n",
        "(This is only neccessary if one or more of the calendars ",
        "in your sample is not the same length as the others.) \n\n ",
        "Here is an example: \n ",
        "reshape_vcal_input_dat(dat, pad_months = list(vcal_1 = c(80, 'left')))"
      )
    } else{
      if(!all(names(pad_months) %in% names(dat))){
        stop(
          "One of the names you provided to pad_months could not be found ",
          "in the data. \n\n",
          "You requested padding for: ",
          paste(names(pad_months), collapse = " "), "\n\n",
          "The data contains: ", paste(names(dat), collapse = " ")
        )
      }
    }

    stop_pad_numeric <- pad_months %>%
      map_lgl(~as.numeric(.x[1]) %>% is.na() %>% suppressWarnings()) %>%
      any()

    stop_pad_lr <- pad_months %>%
      map_lgl(~!.x[2] %in% c("left", "right", "l", "r")) %>%
      any()

    if(stop_pad_numeric){
      stop(
        "Invalid number of months requested for padding in pad_months.\n\n",
        "For every month you want to pad, you must specify the total ",
        "number of months you want in the string (usually 80). \n\n",
        "(This is only neccessary if one or more of the calendars ",
        "in your sample is not the same length as the others.) \n\n ",
        "Here is an example: \n ",
        "reshape_vcal_input_dat(dat, pad_months = list(vcal_1 = c(80, 'left')))"
      )
    }

    if(stop_pad_lr){
      stop(
        "Invalid padding direction requested for padding in pad_months.\n\n",
        "For every month you want to pad, you must specify whether to ",
        "pad spaces on the 'left' or 'right' side of the existing string. \n\n",
        "(This is only neccessary if one or more of the calendars ",
        "in your sample is not the same length as the others.) \n\n ",
        "Here is an example: \n ",
        "reshape_vcal_input_dat(dat, pad_months = list(vcal_1 = c(80, 'left')))"
      )
    }

    dat <- pad_months %>%
      map2_dfc(names(pad_months), ~{
        dat %>%
          select(!!.y) %>%
          mutate(!!.y := str_pad(!!sym(.y), .x[1], .x[2]))
      }) %>%
      bind_cols(dat %>% select(!names(pad_months)))
  }

  # check if sample is in the tracking sheet
  samp <- attr(dat, "sample")
  if(samp %in% sample_tracking$SAMPLE){
    calendar_locations <- sample_tracking %>%
      filter(SAMPLE == samp) %>%
      select(-c(SAMPLE, Comments)) %>%
      pivot_longer(
        everything(),
        values_drop_na = T,
        names_to = "cal_type",
        values_to = "vcal"
      ) %>%
      mutate(vcal = paste0("vcal_", vcal))
  } else {
    stop(
      samp, " was not found in vcal_availability.csv \n\n",
      "Without external information from this file, I cannot guess ",
      "which `vcal` source variable contains which type of calendar!\n\n",
      "Suggested action: go to vcal_availability.csv and check that ", samp,
      " appears in the column SAMPLE (case-sensitive).\n"
    )
  }

  # check if all cals have the same start data, else WARN
  if(length(unique(dat$v017)) != 1){
    warning(
      "The 'CMC start of calendar' variable (v017) contains multiple values.",
      "\n\n",
      "I will attempt to align calendars with different start dates.\n\n",
      "Even so, you might want to check sample documentation to ensure that ",
      "multiple calendar start dates are expected!"
    )
  }

  # check if all cals are the same width for every person
  vcal_widths <- dat %>%
    summarise(across(starts_with("vcal"), ~unique(nchar(.x))))
  if(nrow(vcal_widths) != 1){
    stop(
      "One or more `vcal` source variables is not the same width for ",
      "every person.\n\n",
      "Within any given variable, every person should have the same ",
      "number of values / months (usually 80).\n\n",
      "Please check the Data Dictionary or source reference materials."
    )
  }

  # check vcals planned for processing: are all accounted for?
  vcals_todo <- vcal_widths %>%
    pivot_longer(everything(), names_to = "vcal", values_to = "nchar") %>%
    filter(nchar != 1)
  if(all(vcals_todo$vcal %in% calendar_locations$vcal)){
    vcals_todo <- full_join(vcals_todo, calendar_locations, by = "vcal")
  } else {
    stop(
      "I found non-empty `vcal` source variables in the input data ",
      "that have not been identified in vcal_availability.csv \n\n",
      "Without external information from this file, I cannot guess ",
      "which `vcal` source variable contains which type of calendar!\n\n",
      "Suggested action: go to vcal_availability.csv and identify all ",
      "available `vcal` source variables for ", samp, "."
    )
  }

  # check if cals are all the same length
  if(length(unique(vcals_todo$nchar)) != 1){
    stop(
      "One or more of the `vcal` source variables are different lengths. \n\n",
      "This excludes `vcal` source variables where the length is 1 ",
      "(e.g. 'B' values).\n\n",
      "Every `vcal` variable should have the same number of values / months ",
      "(even if some are blank).\n\n",
      "Please check the Data Dictionary or source reference materials."
    )
  }

  # if no errors have caused this to stop, print a happy message
  message(
    "\033[32m",
    "FYI: Input calendar data was checked with no formatting issues",
    "\033[0m"
  )

  # Make a new column `id` containing the original row index for each person
  dat <- dat %>%  rowid_to_column("id")

  # Parse all available calendar strings and pivot longer
  ## (For example, if a calendar has 80 characters, pivot values into 80 rows)
  ## If there are multiple calendar start-dates in a sample, handle separately
  dhs_path <- attr(dat, "dhs_path")
  dat <- map_df(unique(dat$v017), ~{
    timeline <- .x
    vcals_todo$vcal %>%
      map(~{
        width <- vcal_widths %>% pull(.x)
        dat %>%
          filter(v017 == timeline) %>%
          separate(
            col = .x,
            sep = 1:width,
            into = paste0(.x, "_", seq(
              to = timeline,
              by = -1,
              length = width
            ))
          ) %>%
          pivot_longer(
            starts_with(.x),
            names_pattern = paste0(.x,"_(.*)"),
            names_to = "cmc_month",
            values_to = .x
          )  %>%
          select(caseid, id, cmc_month, .x)
      }) %>%
      reduce(full_join, by = c("caseid", "id", "cmc_month")) %>%
      full_join(by = c("caseid", "id"), dat %>%
                  select(-vcals_todo$vcal) %>%
                  filter(v017 == timeline)
      ) %>%
      rename(sample = v000) %>%
      mutate(across(c(cmc_month, starts_with("v0")), ~as.integer(.x)))
  })

  # preserve attributes from the original data file
  attr(dat, "dhs_path") <- dhs_path
  attr(dat, "sample") <- samp

  # label the calendars included in this sample
  dat <- dat %>%
    rename_with(
      .cols = vcals_todo$vcal,
      .fn = ~vcals_todo %>% filter(vcal == .x) %>% pull(cal_type)
    )

  # add placeholders for all remaining calendars (all NA)
  dat <- sample_tracking %>%
    select(-c(SAMPLE, Comments)) %>%
    names() %>%
    setdiff(names(dat)) %>%
    map_dfc(~tibble(!!.x := NA)) %>%
    bind_cols(dat, .)

  return(dat)
}
