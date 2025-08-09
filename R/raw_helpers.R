#' Parse signed 32 bit integers from a raw vector
#'
#' @param f Raw vector
#' @param n The maximum number of records to be read. Passed to `readBin()`.
#' @param ... Other arguments to pass to `readBin()`
#'
#' @returns Integer vector
#' @export
#'
int32 <- function(f, n = 1L, ...) {
  readBin(
    f,
    what = "integer",
    signed = TRUE,
    endian = "big",
    size = 4,
    n = n,
    ...
  )
}

#' Parse signed 16 bit integers from a raw vector
#'
#' @param f Raw vector
#' @param n The maximum number of records to be read. Passed to `readBin()`.
#' @param ... Other arguments to pass to `readBin()`
#'
#' @returns Integer vector
#' @export
#'
int16 <- function(f, n = 1L, ...) {
  readBin(
    f,
    what = "integer",
    signed = TRUE,
    endian = "big",
    size = 2,
    n = n,
    ...
  )
}

#' Parse signed 8 bit integers from a raw vector
#'
#' @param f Raw vector#'
#' @param n The maximum number of records to be read. Passed to `readBin()`.
#' @param ... Other arguments to pass to `readBin()`
#'
#' @returns Integer vector
#' @export
#'
int8 <- function(f, n = 1L, ...) {
  readBin(
    f,
    what = "integer",
    signed = TRUE,
    endian = "big",
    size = 1,
    n = n,
    ...
  )
}

#' Parses unsigned 16 bit integers from a raw vector
#'
#' @param f Raw vector
#' @param n The maximum number of records to be read. Passed to `readBin()`.
#' @param ... Other arguments to pass to `readBin()`
#'
#' @returns Integer vector
#' @export
#'
uint16 <- function(f, n = 1L, ...) {
  readBin(
    f,
    what = "integer",
    signed = FALSE,
    endian = "big",
    size = 2,
    n = n,
    ...
  )
}

#' Parses unsigned 8 bit integers from a raw vector
#'
#' @param f Raw vector
#' @param n The maximum number of records to be read. Passed to `readBin()`.
#' @param ... Other arguments to pass to `readBin()`
#'
#' @returns Integer vector
#' @export
#'
uint8 <- function(f, n = 1L,...) {
  readBin(
    f,
    what = "integer",
    signed = FALSE,
    endian = "big",
    size = 1,
    n = n,
    ...
  )
}

#' Parses 32 bit floating numbers from a raw vector
#'
#' @param f Raw vector
#' @param n The maximum number of records to be read. Passed to `readBin()`.
#' @param ... Other arguments to pass to `readBin()`
#'
#' @returns Double vector
#' @export
#'
float32 <- function(f, n = 1L, ...) {
  readBin(f, what = "numeric", endian = "big", size = 4, n = n, ...)
}

#' Parses 64 bit floating numbers from a raw vector
#'
#' @param f Raw vector
#' @param n The maximum number of records to be read. Passed to `readBin()`.
#' @param ... Other arguments to pass to `readBin()`
#'
#' @returns Double vector
#' @export
#'
float64 <- function(f, n = 1L, ...) {
  readBin(f, what = "numeric", endian = "big", size = 8, n = n, ...)
}

#' Parses strings from raw vector
#'
#' @param x raw vector
#' @param ... Other arguments to pass to `rawToChar()`
#'
#' @returns Character vector
#' @export
#'
rtc <- function(x, ...) {
  suppressWarnings(rawToChar(x, ...))
}

#' Parses a date from a raw vector
#'
#' @param f Raw vector
#' @param ... Other arguments passed to `as.Date()`
#'
#' @returns Date vector of length 1
#' @export
#'
hidDate <- function(f, ...) {
  year <- int16(f, n = 1)
  month  <-  uint8(f[-(1:2)], n = 1)
  day <-  uint8(f[-(1:3)], n = 1)

  as.Date(paste0(year, "-", month, "-", day), ...)
}

#' Parses time field from raw data.
#'
#' @param f Raw vector
#'
#' @returns List of the hour, minute, second, and hsecond data
#' @export
#'
hidTime <- function(f) {
  list(
    hour = uint8(f, n = 1),
    minute = uint8(f[-1], n = 1),
    second = uint8(f[-(1:2)], n = 1),
    hsecond = uint8(f[-(1:3)], n = 1)
  )
}

#' Parses the char element type of ABIF files from raw data.
#'
#' @param f Raw vector
#'
#' @returns Character vector
#' @export
#'
char <- function(f) {
  tryCatch(
    rtc(f),
    finally = paste(rawToChar(f, multiple = TRUE), collapse = "")
  )
}

#' Parses the pString element type of ABIF files from raw data.
#'
#' @param f Raw vector
#'
#' @returns Character vector
#' @export
#'
pString <- function(f) {
  n <- int8(f[1])
  rtc(f[2:(2 + n)])
}

#' Searches a raw vector containing a .fsa or .hid file for a directory entry
#' name and number, returning the index where found.
#'
#' @param name Character vector of length 1 with the directory name.
#' @param number Integer vector of length 1 with the directory number.
#' @param raw_data Raw vector containing the data from a .fsa or .hid file
#'
#' @returns integer representing the matching index in `raw_data`
#'
seek_raw <- function(name, number, raw_data) {
  stopifnot(is.integer(number))

  name_raw <- charToRaw(name)
  number_raw <- writeBin(number, raw(), size = 4, endian = "big")
  search_raw <- c(name_raw, number_raw)

  grepRaw(search_raw, raw_data)
}

#' Extracts the array length for special element types.
#'
#' @param offset Integer vector of length 1 representing the file offset for the #' start of the directory entry of interest. This can be found using `seek_raw()`.
#' @param raw_data Raw vector containing data from a .fsa or .hid file.
#'
#' @returns Integer vector of length 1 representing the array length.
#'
special_array_length <- function(offset, raw_data) {
  stopifnot(is.integer(offset))
  stopifnot(is.raw(raw_data))
  stopifnot(offset < length(raw_data) - 15)

  int32(raw_data[(offset + 12):(offset + 15)])
}

#' Extracts the data offset for special element types.
#'
#' @param raw_data Raw vector containing data from a .fsa or .hid file.
#' @param entry_name Character vector of length 1. The name of the directory entry.
#' @param entry_num Integer vector of length 1. The number of the directory entry.
#'
#' @returns Integer vector of length one. The file offset of the data for the directory entry
#'
special_data_offset <- function(raw_data, entry_name, entry_num) {
  name_location <- seek_raw(entry_name, entry_num, raw_data)
  data_location_offset <- name_location + 20

  int32(raw_data[data_location_offset:(data_location_offset + 3)])
}

#' Extracts the raw data associated with an ABIF directory entry.
#'
#' @param raw_data Raw vector representing the .fsa or .hid file.
#' @param data_offset Integer vector of length 1. The file offset for the data of the directory entry.
#' @param data_size Integer vector of length 1. The data size of the directory entry.
#'
#' @returns Raw vector containing the data for the directory entry.
#'
extract_raw_data <- function(raw_data, data_offset, data_size) {
  if (data_size < 5) {
    out <- writeBin(data_offset, raw(), size = 4, endian = "big")
    out <- out[1:data_size]
  } else {
    data_start <- data_offset + 1
    data_end <- data_offset + data_size
    out <- raw_data[data_start:data_end]
  }
  out
}
