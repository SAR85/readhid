#' Returns vector of the "special" element types found in .hid files
#'
#' @returns Vector of integers
#'
special_types <- function() {
  c(28L, 30L:34L)
}

#' Returns the name, element type, element size, or substitute element type for
#' a given element type.
#'
#' @param element_type Integer vector of length 1 to look up
#' @param get Character vector of length 1 specifying the data to return
#'
#' @returns Character vector or integer vector of length 1. Depends on value of `get`
#'
element_types <- function(element_type, get = c("name", "type", "size", "sub")) {
  df <- data.frame(
    name = c(
      "byte", # 1
      "char", # 2
      "word", # 3
      "short", # 4
      "long", # 5
      "float", # 7
      "double", # 8
      "date", # 10
      "time", # 11
      "logical", # 13
      "pString", # 18
      "cString", # 19
      "short", # 28
      "char", # 30
      "long", # 31
      "double", # 32
      "cString", # 33
      "pString" # 34
    ),
    type = c(
      1L, 2L, 3L, 4L, 5L,
      7L, 8L, 10L, 11L, 13L,
      18L, 19L, 28L, 30L, 31L,
      32L, 33L, 34L
    ),
    size = c(
      1L, 1L, 2L, 2L, 4L,
      4L, 8L, 4L, 4L, 1L,
      1L, 1L, 2L, 1L, 4L,
      8L, 1L, 1L
    ),
    sub = c(
      1L, 2L, 3L, 4L, 5L,
      7L, 8L, 10L, 11L, 13L,
      18L, 19L, 4L, 2L, 5L,
      8L, 19L, 18L
    )
  )

  df[df$type == element_type, get]
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

#' Parses the raw data associated with an ABIF directory entry.
#'
#' @param raw_data Raw vector of data from the directory entry.
#' @param element_type Integer vector of length 1. The element type of the directory entry.
#' @param num_elements Integer vector of length 1. The number of elements of the directory entry.
#'
#' @returns List, character, or numeric vector, depending on element type. The parsed data from the directory entry.
#'
parse_data <- function(raw_data, element_type, num_elements) {
  out <- NULL

  if (element_type == 1L) {
    out <- int8(raw_data, n = num_elements)
  }
  if (element_type == 2L) {
    out <- char(raw_data)
  }
  if (element_type == 3L) {
    out <- uint16(raw_data, n = num_elements)
  }
  if (element_type == 4L) {
    out <- int16(raw_data, n = num_elements)
  }
  if (element_type == 5L) {
    out <- int32(raw_data, n = num_elements)
  }
  if (element_type == 7L) {
    out <- float32(raw_data, n = num_elements)
  }
  if (element_type == 8L) {
    out <- float64(raw_data, n = num_elements)
  }
  if (element_type == 10L) {
    out <- hidDate(raw_data)
  }
  if (element_type == 11L) {
    out <- hidTime(raw_data)
  }
  if (element_type == 13L) {
    out <- as.logical(raw_data, n = num_elements)
  }
  if (element_type == 18L) {
    out <- pString(raw_data)
  }
  if (element_type == 19L) {
    out <- rtc(raw_data[-length(raw_data)])
  }

  out
}
