new_hid <- function(filepath, keep_data = c("parsed", "raw", "both", "none")) {
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
      abif_dir(x, raw_data, keep_data = ifelse(both, "both", keep_data))
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

#' Creates an `hid` object
#'
#' `hid` objects represent the data in an Applied Biosystems Inc. Format (ABIF)
#' file. These files contain DNA sequencing or fragment analysis data produced
#' by Genetic Analyzers and commonly have file extensions such as .fsa or .hid.
#'
#' @param filepath character. Path to the .fsa or .hid file.
#' @param keep_data character. Specifies what directory entry data to keep
#' (raw, parsed, both, or none). If "raw" or "both", also keeps the entire file
#' raw data.
#'
#' @returns hid object
#' @export hid
#'
#' @examples
#' \dontrun{
#' my_hid_file <- hid("/path/to/file.hid", keep_data = "parsed")
#' }
#'
hid <- function(filepath, keep_data = c("parsed", "raw", "both", "none")) {
  new_hid(filepath, keep_data)
}

#' Read an ABIF file
#'
#' This function reads the raw data from an ABIF file.
#'
#' @param filepath character. Path to the ABIF file.
#' @param ... Additional arguments passed to `readBin()`
#'
#' @returns raw. The raw contents of the file.
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
