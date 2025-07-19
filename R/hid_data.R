special_types <- function() {
  c(28, 30:34)
}

element_types <- function(element_type, get = c("name", "size", "sub")) {
  df <- data.frame(
    name = c(
      "byte", "char", "word", "short", "long", "float", "double",
      "date", "time", "pString", "cString", "short", "char", "long", "double",
      "cString", "pString"
    ),
    type = c(1L, 2L, 3L, 4L, 5L, 7L, 8L, 10L, 11L, 18L, 19L, 28L, 30:34L),
    size = c(1L, 1L, 2L, 2L, 4L, 4L, 8L, 4L, 4L, 1L, 1L, 2L, 1L, 4L, 8L, 1L, 1L),
    sub = c(1L, 2L, 3L, 4L, 5L, 7L, 8L, 10L, 11L, 18L, 19L, 4L, 2L, 5L, 8L, 19L, 18L)
  )

  df[df$type == element_type, get]
}

seek_raw <- function(name, number, rawdata) {
  name_raw <- charToRaw(name)
  number_raw <- writeBin(number, raw(), size = 4, endian = "big")
  search_raw <- c(name_raw, number_raw)

  grepRaw(search_raw, rawdata)
}

special_array_length <- function(offset, raw_data) {
  stopifnot(is.integer(offset))
  stopifnot(is.raw(raw_data))
  stopifnot(offset < length(raw_data) - 15)

  int32(raw_data[(offset + 12):(offset + 15)])
}

special_data_offset <- function(raw_data, directory_entry) {
  data_size <- directory_entry$data_size

  name <- directory_entry$name
  number <- directory_entry$num
  name_location <- seek_raw(name, number, raw_data)
  data_location_offset <- name_location + 20
  data_location <- int32(raw_data[data_location_offset:(data_location_offset + 3)])

  ifelse(data_size < 5, data_location_offset, data_location)
}
