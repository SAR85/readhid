new_hid <- function(filepath,
                    raw = FALSE,
                    friendly_peak_names = TRUE,
                    dye_names = TRUE) {
  x <- list(
    file = filepath,
    raw_data = read_hid(filepath)
  )

  x$header <- extract_header(x$raw_data)

  if (x$header$file_format != "ABIF") {
    stop("File is not in ABIF format: ", filepath)
  }

  x$directory <- extract_directory(
    x$raw_data,
    x$header$directory_offset,
    x$header$num_elements
  )
  x$data <- extract_data(x$raw_data, x$directory, raw = raw)

  if (!raw) {
    x$hid_peaks <- peaks_to_df(
      x$data,
      friendly_names = friendly_peak_names,
      dye_names = dye_names
    )
  }

  structure(x, class = "hid")
}

# Placeholder for hid class validator
validate_hid <- function() {

}

#' Creates an hid object with the data from an .fsa or .hid file.
#'
#' @param filepath character vector of length 1. Path to the .fsa or .hid file.
#' @param raw logical. When TRUE, returns the raw data for each directory entry instead of the parsed data.
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
#' @returns List containing a list for each directory entry.
#'
extract_directory <- function(raw_data, directory_offset, num_elements) {
  stopifnot(is.raw(raw_data))
  stopifnot(is.integer(directory_offset))
  stopifnot(is.integer(num_elements))

  entry_offsets <- seq(directory_offset + 1,
    directory_offset + 28 * num_elements,
    by = 28
  )

  # Extract the standard values based on the ABIF specification
  dir_list <- lapply(entry_offsets, function(x) {
    entry <- extract_directory_entry(raw_data[x:(x + 27)])

    if (entry$type %in% special_types()) {
      entry$data_offset <- special_data_offset(
        raw_data,
        entry$name,
        entry$num
      )
      # Substitute the normal element_type for later parsing
      entry$type <- element_types(entry$type, get = "sub")

      # Get the number of elements (array length)
      file_offset <- seek_raw(entry$name, entry$num, raw_data)
      entry$num_elements <- special_array_length(
        file_offset,
        raw_data
      )

      # Get the element size
      entry$element_size <- element_types(entry$type, get = "size")

      # Set the data size
      entry$data_size <- entry$num_elements * entry$element_size
    }
    entry
  })

  names(dir_list) <- sapply(dir_list, function(x) {
    paste0(x$name, ".", x$num)
  })

  dir_list
}

#' Extracts the information for a single ABIF directory entry. This function returns
#' correct values for standard directory entries but will return incorrect information
#' for special data types.
#'
#' @param raw_data Raw vector of length 28 representing the directory entry.
#'
#' @returns List with elements corresponding to the ABIF directory entry information.
#'
extract_directory_entry <- function(raw_data) {
  stopifnot(is.raw(raw_data))
  stopifnot(length(raw_data) == 28)

  out <- list(
    name = rtc(raw_data[1:4]),
    num = int32(raw_data[5:8]),
    type = int16(raw_data[9:10]),
    element_size = int16(raw_data[11:12]),
    num_elements = int32(raw_data[13:16]),
    data_size = int32(raw_data[17:20]),
    data_offset = int32(raw_data[21:24])
    # data_handle = int32(raw_data[25:28])
  )
  out
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
