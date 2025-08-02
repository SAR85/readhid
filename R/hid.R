new_hid <- function(filepath,
                    raw = FALSE,
                    friendly_peak_names = TRUE,
                    dye_names = TRUE,
                    ...) {
  # Read the file
  raw_data <- read_hid(filepath)
  # Extract the ABIF header
  header <- extract_header(raw_data)

  if (header$file_format != "ABIF") {
    stop("File is not in ABIF format: ", filepath)
  }

  # Extract the ABIF directory
  directory <- extract_directory(
    raw_data,
    header$directory_offset,
    header$num_elements
  )

  # Parse the data for each entry
  entry_data <- lapply(directory, function(dir_entry) {
    parse_data(dir_entry$raw_data, dir_entry$type, dir_entry$num_elements)
  })

  # Make a data.frame of the Peak entry data
  peaks <- peaks_to_df(
    entry_data,
    friendly_names = friendly_peak_names,
    dye_names = dye_names
  )

  # Remove the raw data from directory entries if user doesn't want it
  if (!raw) {
    directory <- lapply(directory, function(dir_entry) {
      dir_entry$raw_data <- NULL
      dir_entry
    })
  }

  # Create the list that will become the hid object
  out <- list(
    file = filepath,
    header = header,
    directory = directory,
    data = entry_data,
    peaks = peaks
  )
  if (raw) out$raw_data <- raw_data

  structure(out, class = "hid")
}

#' Creates an hid object with the data from an .fsa or .hid file.
#'
#' @param filepath character vector of length 1. Path to the .fsa or .hid file.
#' @param raw logical. When TRUE, retains the raw_data from the file and each directory entry.
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
hid <- function(filepath, raw = FALSE, friendly_peak_names = TRUE,
                dye_names = TRUE, ...) {
  new_hid(filepath, raw, friendly_peak_names, dye_names, ...)
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

#' Extracts the ABIF header
#'
#' @param raw_data Raw vector with contents of .fsa or .hid file.
#'
#' @returns List with elements corresponding to the ABIF header information.
#'
extract_header <- function(raw_data) {
  list(
    file_format = rtc(raw_data[1:4]),
    file_format_version = int16(raw_data[5:6]),
    name = rtc(raw_data[7:10]),
    num = int32(raw_data[11:14]),
    type = int16(raw_data[15:16]),
    element_size = int16(raw_data[17:18]),
    num_elements = int32(raw_data[19:22]),
    directory_size = int32(raw_data[23:26]),
    directory_offset = int32(raw_data[27:30])
    # data_handle = int32(raw_data[31:34]),
    # unused_bytes = int16(raw_data[35:128], n = 47)
  )
}

#' Extracts the ABIF directory.
#'
#' @param raw_data Raw vector containing the contents of a .fsa or .hid file.
#' @param directory_offset The 0-based offset of the directory in the file. This is not the offset in the raw_data vector, which would larger by 1 due to R's 1-based indexing.
#' @param num_elements Integer containing the number of directory entries.
#'
#' @returns List of `abif_dir` objects, one for each directory entry.
#'
extract_directory <- function(raw_data, directory_offset, num_elements) {
  stopifnot(is.raw(raw_data))
  stopifnot(is.integer(directory_offset))
  stopifnot(is.integer(num_elements))

  entry_offsets <- seq(directory_offset,
    directory_offset + 28L * (num_elements - 1),
    by = 28
  )

  # Extract the directory entries
  dir_list <- lapply(entry_offsets, function(x) abif_dir(x, raw_data))

  names(dir_list) <- sapply(dir_list, function(x) paste0(x$name, ".", x$num))

  dir_list
}

#' Extracts the data associated with all directory entries in a .fsa or .hid file.
#'
#' @param raw_data Raw vector from .fsa or .hid file.
#' @param directory List representing the ABIF directory.
#' @param raw logical indicating whether to return raw data or parsed data
#'
#' @returns List containing an element with the data for each directory entry.
#' Returns the raw data when `raw = TRUE`
#'
extract_data <- function(raw_data, directory, raw = FALSE) {
  lapply(directory, function(x) {
    out_raw <- extract_raw_data(raw_data, x$data_offset, x$data_size)
    out_parsed <- parse_data(out_raw, x$type, x$num_elements)

    if (raw) {
      out_raw
    } else {
      out_parsed
    }
  })
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

#' Extracts the `data` element from an hid object. If `pattern` is specified, returns only the elements of `data` that match pattern using `grep()`.
#'
#' @param x hid object
#' @param pattern character. A string
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
hid_data <- function(x, pattern = NULL, ...) {
  stopifnot(class(x) == "hid")
  stopifnot(is.null(pattern) || is.character(pattern))
  if (is.character(pattern) && length(pattern) > 1) {
    stop("pattern must be length 1")
  }

  out <- NULL

  if (!"data" %in% names(x)) {
    warning("No data in this hid object.")
  } else {
    if (is.null(pattern)) {
      out <- x$data
    } else {
      out <- x$data[grep(pattern, names(x$data), ...)]
    }
  }
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

  out <- NULL

  if (!"peaks" %in% names(x)) {
    warning("No peak data in this hid object.")
  } else {
    out <- x$peaks
  }
  invisible(out)
}
