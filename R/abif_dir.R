new_abif_dir <- function(dir_offset, file_raw) {
  stopifnot(dir_offset + 28 <= length(file_raw))

  dir_raw <- file_raw[(dir_offset + 1):(dir_offset + 28)]

  out <- list(
    name = rtc(dir_raw[1:4]),
    num = int32(dir_raw[5:8]),
    type = int16(dir_raw[9:10]),
    element_size = int16(dir_raw[11:12]),
    num_elements = int32(dir_raw[13:16]),
    data_size = int32(dir_raw[17:20]),
    data_offset = int32(dir_raw[21:24])
    # data_handle = int32(raw_data[25:28])
  )

  # This logic replaces the above standard values as applicable
  # when the element type is a special type
  if (out$type %in% special_types()) {
    # Store the special type for future reference
    attr(out, "special_type") <- out$type

    # Update the data_offset
    out$data_offset <- special_data_offset(
      file_raw,
      out$name,
      out$num
    )
    # Substitute the normal element_type for later parsing
    out$type <- element_types(out$type, get = "sub")

    # Get the number of elements (array length)
    file_offset <- seek_raw(out$name, out$num, file_raw)
    out$num_elements <- special_array_length(
      file_offset,
      file_raw
    )

    # Get the element size
    out$element_size <- element_types(out$type, get = "size")

    # Set the data size
    out$data_size <- out$num_elements * out$element_size
  }

  out$raw_data <- extract_raw_data(file_raw, out$data_offset, out$data_size)

  structure(out, class = "abif_dir")
}

abif_dir <- function(dir_raw, file_raw) {
  new_abif_dir(dir_raw, file_raw)
}

#' @export
print.abif_dir <- function(dir_entry) {
  stopifnot(class(dir_entry) == "abif_dir")

  cat(
    "ABIF directory entry\n",
    "Name:", dir_entry$name,"\n",
    "Number:", dir_entry$num, "\n",
    "Type:", dir_entry$type,
    paste0("(", element_types(dir_entry$type, "name"), ")"), "\n",
    "Element size:", dir_entry$element_size, "bytes\n",
    "Num. elements:", format(dir_entry$num_elements, big.mark = ","), "\n",
    "Data size:", format(dir_entry$data_size, big.mark = ","), "bytes\n",
    "Data offset:", dir_entry$data_offset, "\n"
  )
  if(!is.null(attr(dir_entry, "special_type"))) {
    cat(
      "Special type:", attr(dir_entry, "special_type")
    )
  }
  invisible(dir_entry)
}
