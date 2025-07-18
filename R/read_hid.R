read_hid <- function(filepath, ...){
  stopifnot(file.exists(filepath))

  hid_file <- read(filepath, open = "rb")

  readBin(hid_file, what = "raw", n = 1.2 * file.info(hid_file)$size, ...)
}

extract_header <- function(raw_data = raw()) {
  list(
    file_format = rtc(raw_data[1:4]),
    file_format_version = int16(raw_data[5:6]),
    name = rtc(raw_data[7:10]),
    num = int32(raw_data[11:14]),
    type = int16(raw_data[15:16]),
    element_size = int16(raw_data[17:18]),
    num_elements = int32(raw_data[19:22]),
    data_size = int32(raw_data[23:26]),
    data_offset = int32(raw_data[27:30]),
    data_handle = int32(raw_data[31:34]),
    unused_bytes = int16(raw_data[35:128], n = 47)
  )
}

extract_directory <- function(raw_data = raw()) {
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

parse_directory_entry <- function(rawdata = raw()) {
  offset <- hid_object$header$directory_dataoffset
  numelements <- hid_object$header$directory_numelements
  entry_offsets <- seq(offset + 1, offset + 28 * numelements, by = 28)

  dir <- lapply(entry_offsets, function(x) {
    parse_directory_entry(rawdata[x:(x + 28)])
  })

  hid_object$directory <- dir

  hid_object
}
