new_hid <- function(filepath,
                    keep_data = c("parsed", "raw", "both", "none"),
                    friendly_peak_names = TRUE,
                    dye_names = TRUE,
                    ...) {
  keep_data <- match.arg(keep_data)
  both <- FALSE

  if (keep_data == "both") {
    both <- TRUE
    keep_data <- c("parsed", "raw")
  }

  # Read the file
  raw_data <- read_hid(filepath)

  # Extract the ABIF header
  header <- abif_head(raw_data[1:128])

  # Extract the ABIF directory
  dir_entry_offsets <- seq(dir_offset(header),
    dir_offset(header) + 28L * (dir_n(header) - 1),
    by = 28
  )

  dir_list <- lapply(
    dir_entry_offsets,
    function(x) {
      abif_dir(x,
        raw_data,
        keep_data = ifelse(both, "both", keep_data)
      )
    }
  )
  names(dir_list) <- sapply(
    dir_list,
    function(x) dir_name(x, include_num = TRUE)
  )

  # Create the list that will become the hid object
  out <- list(
    file = filepath,
    header = header,
    directory = dir_list
  )
  if ("raw" %in% keep_data) out$raw_data <- raw_data

  structure(out, class = "hid")
}

#' Creates an hid object with the data from an .fsa or .hid file.
#'
#' @param filepath character vector of length 1. Path to the .fsa or .hid file.
#' @param keep_data character. Specifies what directory entry data to keep
#' (raw, parsed, both, or none). If "raw" or "both", also keeps the entire file
#' raw data.
#' @param ... Other parameters to customize the data parsing and/or object format
#' @param friendly_peak_names logical. When TRUE, uses informative names in the `hid_peaks` data frame
#' @param dye_names logical. When TRUE, adds dye names to `hid_peaks` data frame
#'
#' @returns hid object
#' @export hid
#'
#' @examples
#' \dontrun{
#' my_hid_file <- hid("/path/to/file.hid")
#' }
#'
hid <- function(filepath,
                keep_data = c("parsed", "raw", "both", "none"),
                friendly_peak_names = TRUE,
                dye_names = TRUE, ...) {
  new_hid(filepath, keep_data, friendly_peak_names, dye_names, ...)
}

#' Reads an hid file
#'
#' @param filepath Character vector of length 1 containing path to the .fsa or .hid file.
#' @param ... Additional arguments to pass to `readBin()`
#'
#' @returns Raw vector with contents of the file.
#'
read_hid <- function(filepath, ...) {
  stopifnot(file.exists(filepath))
  stopifnot(length(filepath) == 1)

  hid_file <- file(filepath, open = "rb")

  out <- readBin(
    hid_file,
    what = "raw",
    n = 1.2 * file.info(filepath)$size,
    ...
  )

  close(hid_file)

  out
}

#' @export
print.hid <- function(x, ...) {
  cat(
    "ABIF file object\n",
    "File format version:", x$header$file_format_version, "\n",
    "File:", x$file, "\n",
    "Elements:", x$header$num_elements, "\n",
    "Instrument:", x$data$MODL.1, "\n",
    paste("Run date:", x$data$RUND.1), "\n"
  )
  invisible(x)
}

#' Extracts the `data` element from an hid object.
#'
#' If `pattern` is specified, returns data from only the directory entries that
#' match `pattern` using `grep()`.
#'
#' @param x hid object
#' @param pattern character. A string containing the data name to search for. Passed to `grep()`.
#' @param what character. Specifies what directory entry data to keep.
#' @param ... Other arguments passed to `grep()`
#'
#' @returns list. Invisibly returns the data element of an hid object.
#' @export
#'
#' @examples
#' \dontrun{
#' my_hid_data <- hid_data(my_hid)
#' # Get the name of the first dye
#' my_hid_data$DyeN.1
#' # Get the instrument model name
#' my_hid_data$MODL.1
#' # Get all the data elements with "DATA" in the name
#' hid_data(my_hid, "DATA")
#' }
hid_data <- function(x, pattern = NULL,
                     what = c("parsed", "raw", "both"),
                     ...) {
  stopifnot(class(x) == "hid")
  stopifnot(is.null(pattern) || is.character(pattern))
  if (is.character(pattern) && length(pattern) > 1) {
    stop("pattern must be length 1")
  }

  what <- match.arg(what)

  if (is.null(pattern)) {
    dir_names <- names(x$directory)
  } else {
    dir_names <- grep(pattern, names(x$directory), ...)
  }

  out <- lapply(x$directory[dir_names], function(dir_entry) {
    dir_data(dir_entry, what)
  })
  invisible(out)
}

#' Extracts the `hid_peaks` element from an hid object.
#'
#' @param x hid object
#'
#' @returns dataframe. Invisibly returns the `hid_peaks` element from the hid object
#' @export
#'
#' @examples
#' \dontrun{
#' my_hid_peaks <- hid_peaks(my_hid)
#' # Calculate average peak height
#' mean(my_hid_peaks$height)
#' }
hid_peaks <- function(x) {
  stopifnot(class(x) == "hid")

  parsed_peaks <- hid_data(x, pattern = "Peak\\.[1-9]+", what = "parsed")
  parsed_peaks <- lapply(parsed_peaks, `[[`, "parsed_data")
  out <- peaks_to_df(parsed_peaks)

  invisible(out)
}
