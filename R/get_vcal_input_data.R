#' @title Get calendar data from an IR file
#' @author Matt Gunther
#' @description Returns a modified version of the IR file for a sample,
#' including \emph{only the handful of variables that are relevant for calendar
#' processing}. As a helpful side-effect, the sample name and the path to the
#' IPUMS DHS folder on the user's system will be attached as an attribute of the
#' returned data frame.
#'
#' @details This function is basically a combination of
#' mpctools:::read_data_dict_as_ddi (modified to select a specific list of
#' variables and assume a few DHS data features, greatly improving speed) and
#' ipumsr::read_ipums_micro (modified to avoid trimming whitespace for each
#' of the calendar string variables).
#' @note All of the variables needed for calendar processing are stored as a
#' quosure list in the first line of this function. In the future, if a sample
#' includes a weird name for its calendar data, you may need to add variables
#' to this list (this happened with eg1995ir, where the termination
#' calendar was stored in a variable called "scal" for some reason). If a
#' sample does not include one or more of the variables listed, those variables
#' will be silently skipped: the resulting data frame will contain all of the
#' variables that were available for your sample.
#' @param sample Character: An IR sample that you want to process
#' (e.g. "af2015ir")
#' @param dd_path Optional Character: If you're working in a local
#' version of RStudio (or if the sample has not been added to the IPUMS DHS
#' control file), this function may have trouble finding the data
#' dictionary on its own. If so, you can just tell it where to look: specify
#' the file path here.
#' (e.g. "~/Z/dhs/country/afghanistan/2015/data/data_dict_af2015ir.xlsx")
#' @export get_vcal_input_data
get_vcal_input_data <- function(
  sample = NULL,
  dd_path = NULL
){

  # Enter the variables needed for calendar processing:
  vars <- rlang::quos(
    caseid,
    v000,
    v006,
    v007,
    v008,
    v017,
    v019,
    vcal_1,
    vcal_2,
    vcal_3,
    vcal_4,
    vcal_5,
    vcal_6,
    vcal_7,
    vcal_8,
    vcal_9,
    scal
  )

  # Feedback for function arguments
  if(is.null(sample) & is.null(dd_path)){
    stop("Please speficy either a sample or dd_path (not both)")
  }
  if(!is.null(sample) & !is.null(dd_path)){
    stop("Please speficy either a sample or dd_path (not both)")
  }

  # Try finding dd_path automatically for sample
  if(!is.null(sample)){
    if(file.path(mpc_root(), "dhs") %>% dir.exists()){
      dhs_path <- file.path(mpc_root(), "dhs")
      samp <- sample
      if(!grepl("ir", samp)){stop(samp, " is not an IR file!")}

      dd_path <- dhs_path %>%
        file.path("metadata/control_files/samples.csv") %>%
        readr::read_csv(
          col_types = readr::cols(.default = readr::col_character())
        ) %>%
        filter(sample == samp) %>%
        pull(datadict) %>%
        file.path(dhs_path, "country", .)

      # Test if data dictionary exists at path
      if(!file.exists(dd_path)){
        stop(
          "Control file points to ", dd_path,
          "\n but file not found there."
        )
      }

    } else {
      stop(
        "Path to ", sample, "data dicitonary could not be determined.",
        "\n\nIf you are running R somewhere other than ",
        "https://rstudio.pop.umn.edu/, \n check ",
        "to make sure that you are connected to VPN and have mapped the IPUMS ",
        "drive.\n\n",
        "Otherwise, try specifying dd_path instead of sample."
      )
    }

  # Test if user specified dd_path exists
  } else {
    if(!file.exists(dd_path)){
      stop("File not found at ", dd_path)
    }
  }

  # Find data file path
  data_file <- dd_path %>%
    str_replace("/data_dict_", "/") %>%
    str_replace("\\.xls(x)?$", "\\.dat")

  # Test if data file exists at path
  if(!file.exists(data_file)){
    stop(
      "No data file found with data dicitonary at \n", dd_path, "."
    )
  }

  # Read data dictionary as a data frame
  data_dict <- suppressMessages(
    suppressWarnings(
      read_data_dict(dd_path) # creates a useless warning AND message
    )
  )

  # Filter only `vars` specified above
  data_dict <- map_df(vars, ~data_dict %>% filter(Var %in% !!as_label(.x)))

  # Obtain variable metadata or specify as needed
  var_info <- tibble(
    var_name = data_dict$Var,
    var_label = data_dict$VarLabel,
    var_desc = data_dict$Notes,
    val_labels = NA,
    code_instr = NA_character_,
    start = as.numeric(data_dict$Col),
    end = as.numeric(data_dict$Col) + as.numeric(data_dict$Wid) - 1,
    imp_decim = ifelse(is.na(data_dict$Decim), 0, as.numeric(data_dict$Decim)),
    var_type = "character",
    rectypes = NA,
    var_type_svar = "character"
  )

  # make ddi from data dictionary and metdata in var_info
  ddi <- ipumsr::make_ddi_from_scratch(
    file_name = basename(data_file),
    file_path = dirname(data_file),
    file_type = "rectangular",
    ipums_project = "internal",
    rectypes = NULL,
    rectype_idvar = NA_character_,
    var_info = var_info,
    conditions = "Internal IPUMS Input Data"
  )

  # fix samp if not already provided
  samp <- ddi$file_name %>% str_remove(".dat")

  # IMPORTANT: col_spec specifies vars where white space should NOT be trimmed
  # White space is NOT trimmed for any variable name beginning with "vcal"
  # ... or for caseid
  col_spec <- ipumsr:::ddi_to_colspec(ddi, "long", verbose) %>%
    mutate(trim_ws = case_when(
      grepl("vcal", col_names) ~ FALSE,
      grepl("caseid", col_names) ~ FALSE,
      T ~ trim_ws
    ))

  # Read the input data into R (hipread is used by ipumsr)
  output <- hipread::hipread_long(
    file = data_file,
    var_info = col_spec,
    rt_info =  ipumsr:::ddi_to_rtinfo(ddi),
    n_max = Inf,
    encoding = ddi$file_encoding
  )

  # Get any variable labels / descriptions (if they are in the data dictionary)
  output <- ipumsr:::set_ipums_var_attributes(
    output, ddi, var_attrs = "var_label"
  )

  # Assign dd_path as an attribute of the output
  attr(output, "dd_path") <- dd_path

  # Assign dhs_path as an attribute of the output
  dhs_path <- dd_path %>% str_remove("country.*")
  attr(output, "dhs_path") <- dhs_path

  # Assign sample name as an attribute of the output
  attr(output, "sample") <- samp

  return(output)
}
