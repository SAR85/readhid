#' Returns vector of the "special" element types found in newer version of
#' ABIF files, such as .hid files produced by the 3500 Genetic Analyzer.
#'
#' @returns integer vector representing the special element types.
#' @export
#'
#' @examples
#' ifelse(32L %in% special_types(), print("32 is special"), print("32 isn't special"))
#'
special_types <- function() {
  c(28L, 30L:34L)
}

#' Returns the name, element size, substitute element type, or
#' the name of the function used to parse the element type.
#'
#' Return value depends on the `get` parameter:
#' "name" returns a string with the name of the element type
#'
#' "size" returns an integer with the element size for the element type
#'
#' "sub" returns an integer with the standard element type substitute. For
#' special element types, this will be the standard element type used to parse
#' the data. For normal element types, this will be the same as the element type.
#'
#' "parse_fun" returns a string with the name of the function used to parse the
#' element type.
#'
#' @param element_type Integer vector of length 1 to look up
#' @param get Character vector of length 1 specifying the data to return
#'
#' @returns Character vector or integer vector of length 1. Depends on value of `get`
#' @export
#'
#' @seealso [special_types()]
#' @examples
#' # Element type 33 is a special type. The equivalent standard type can be found
#' # using "sub":
#' element_types(33L, "sub")
#'
#' # Retrieve the parsing function for an element type:
#' parse_fun <- match.fun(element_types(13L, "parse_fun"))
#' identical(parse_fun, as.logical)
#' parse_fun(as.raw(0x01), n = 1)
#'
element_types <- function(
    element_type,
    get = c("name", "size", "sub", "parse_fun")) {
  stopifnot(is.integer(element_type))

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
    ),
    parse_fun = c(
      "uint8", "char", "uint16", "int16", "int32",
      "float32", "float64", "hidDate", "hidTime", "as.logical",
      "pString", "rtc", "int16", "char", "int32",
      "float64", "rtc", "pString"
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
