new_hid <- function(filepath) {
  x <- list(
    file = filepath,
    raw_data = read_hid(filepath)
  )

  x$header <- extract_header(x$raw_data)
  x$directory <- extract_directory(
    x$raw_data,
    x$header$directory_offset,
    x$header$num_elements
  )
  x$fsa_data <- NULL # Todo: extract the abif data for each directory entry
  x$hid_data <- NULL # Todo: extract the data for "DATA" fields in hid files
  x$hid_peaks <- NULL # Todo: extract the data for "Peak" fields in hid files

  structure(x, class = "hid")
}

# Placeholder for hid class validator
validate_hid <- function() {

}

# Placeholder for hid class helper
hid <- function() {

}

read_hid <- function(filepath, ...) {
  stopifnot(file.exists(filepath))

  hid_file <- file(filepath, open = "rb")

  out <- readBin(hid_file, what = "raw", n = 1.2 * file.info(filepath)$size, ...)

  close(hid_file)

  out
}

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

extract_directory <- function(raw_data, directory_offset, num_elements) {
  stopifnot(is.raw(raw_data))
  stopifnot(is.integer(directory_offset))
  stopifnot(is.integer(num_elements))

  entry_offsets <- seq(directory_offset + 1,
    directory_offset + 28 * num_elements,
    by = 28
  )

  lapply(entry_offsets, function(x) {
    extract_directory_entry(raw_data[x:(x + 27)])
  })
}

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
    data_offset = int32(raw_data[21:24]),
    data_handle = int32(raw_data[25:28])
  )
  out
}
