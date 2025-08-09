new_abif_peaks <- function(x, friendly_names = TRUE, dye_names = TRUE) {
  if (is(x, "hid")) x <- x$directory
  if (is.null(x)) stop("No directory found in x.")

  # Extract the Peak entries from the directory
  peaks <- x[grep("Peak\\.[1-9]+", names(x))]

  # Check for Peak entries and exit if none found
  if (length(peaks) == 0) {
    message("No peak data in directory.")
    return(data.frame())
  }

  # Extract data from the Peak directory entries, parsing if needed
  peaks <- lapply(peaks, function(dir_entry) {
    is_parsed <- attr(dir_entry, "parsed")
    is_raw <- attr(dir_entry, "raw")

    out <- NULL

    # Parse raw data if present and not already parsed
    if (!is_parsed && is_raw) out <- parse_dat(dir_entry$raw_data)
    # Return parsed data if present
    if (is_parsed) out <- dir_entry$parsed_data
    # Character type for peak data is a comma-separated list.
    # Need to split the strings to make same length as other vectors
    if (is.character(out)) out <- unlist(strsplit(out, ","))

    out
  })

  missing_data <- sapply(peaks, is.null)
  if (sum(missing_data) == length(peaks)) {
    message(paste(
      "Peak entries found but do not contain data.\n",
      "Try creating the directory object using keep_data = 'parsed', 'raw',",
      "or 'both'."
    ))
    return(data.frame())
  }

  # Convert to data.frame
  peaks <- as.data.frame(peaks)

  # Rename columns with more friendly names according to friendly_names
  if (friendly_names) {
    peak_names <- friendly_peak_names()

    names(peaks) <- sapply(names(peaks), function(name) {
      if (name %in% names(peak_names)) {
        out <- peak_names[[name]]
      } else {
        out <- name
      }
      out
    })
  }

  # Add dye names according to dye_names
  if (dye_names) peaks <- merge(peaks, dye_names(x), all.x = TRUE)

  structure(peaks, class = c("abif_peaks", class(peaks)))
}

#' Creates a data.frame from the `Peak` data in an ABIF file.
#'
#' @param x hid or abif_dir object.
#' @param friendly_names logical. Whether to rename columns with more
#' meaningful names or use the directory entry names.
#' @param dye_names logical. Whether to add dye names to the output data.frame.
#'
#' @returns data.frame created from the `Peak` entries in x.
#' @export
#'
abif_peaks <- function(x, friendly_names = TRUE, dye_names = TRUE) {
  new_abif_peaks(x, friendly_names, dye_names)
}

friendly_peak_names <- function() {
  list(
    Peak.1 = "dye_index",
    Peak.2 = "data_point",
    Peak.3 = "begin_data_point",
    Peak.4 = "end_data_point",
    Peak.5 = "fwhm",
    Peak.6 = "corrected_fwhm",
    Peak.7 = "height",
    Peak.8 = "begin_height",
    Peak.9 = "end_height",
    Peak.10 = "area",
    Peak.11 = "corrected_area",
    Peak.12 = "size",
    Peak.13 = "begin_size",
    Peak.14 = "end_size",
    Peak.15 = "fwhm_bp",
    Peak.16 = "corrected_fwhm_bp",
    Peak.17 = "area_bp",
    Peak.18 = "corrected_area_bp",
    Peak.19 = "label",
    Peak.20 = "size_matched",
    Peak.21 = "bp_size_match",
    Peak.22 = "offscale",
    Peak.23 = "user_created",
    Peak.24 = "broad",
    Peak.25 = "pullup"
  )
}

dye_names <- function(directory) {
  num_dyes <- dir_data(directory$`Dye#.1`, "parsed")
  dyes <- directory[grep("DyeN\\.[0-9]+", names(directory))]
  list(
    dye_index = seq(num_dyes),
    dye_name = sapply(dyes, `[[`, "parsed_data")
  )
}
