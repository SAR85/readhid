new_abif_head <- function(dat) {
  if (length(dat) < 30 || !setequal(dat[1:4], charToRaw("ABIF"))) {
    stop("File is not in ABIF format.")
  }

  dir_entry <- abif_dir(6L, dat, keep_data = "none")

  out <- list(
    file_format = rtc(dat[1:4]),
    file_format_version = int16(dat[5:6])
    # name = rtc(dat[7:10]),
    # num = int32(dat[11:14]),
    # type = int16(dat[15:16]),
    # element_size = int16(dat[17:18]),
    # num_elements = int32(dat[19:22]),
    # directory_size = int32(dat[23:26]),
    # directory_offset = int32(dat[27:30])
    # data_handle = int32(raw_data[31:34]),
    # unused_bytes = int16(raw_data[35:128], n = 47)
  )
  out <- c(out, dir_entry)

  structure(out, class = c("abif_head", "abif_dir"))
}

#' Creates a new `abif_head` object, which is used to represent the header of
#' an ABIF file, such as .fsa or .hid.
#'
#' @param dat raw. The data containing the ABIF file header. Must be at least
#' the first 30 bytes of the file.
#'
#' @return abif_head object containing the parsed header data.
#' @export
abif_head <- function(dat) {
  new_abif_head(dat)
}

#' @export
print.abif_head <- function(x, ...) {
  cat(
    "ABIF header\n",
    "File format version:", x$file_format_version, "\n",
    "Directory elements:", x$num_elements, "\n",
    "Directory offset:", x$directory_offset
  )
  invisible(x)
}
