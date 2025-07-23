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

dye_names <- function(data) {
  list(
    dye_index = seq(data$`Dye#.1`),
    dye_name = unlist(data[grep("DyeN\\.[0-9]+", names(data))])
  )
}

peaks_to_df <- function(data, friendly_names = TRUE, dye_names = TRUE) {
  # Extract the Peak entries
  peaks <- data[grep("Peak\\.[1-9]+", names(data))]

  # Split the character data types to make same length as other vectors
  peaks <- lapply(peaks, function(x) {
    if (is.character(x)) {
      out <- unlist(strsplit(x, ","))
    } else {
      out <- x
    }
    out
  })

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
  if (dye_names) {
    peaks <- merge(peaks, dye_names(data), all.x = TRUE)
  }
  peaks
}
