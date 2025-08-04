new_abif_dir <- function(dir_offset,
                         file_raw,
                         keep_data = c("parsed", "raw", "both", "none")) {
  stopifnot(dir_offset + 28 <= length(file_raw))

  keep_data <- match.arg(keep_data)
  if (keep_data == "both") keep_data <- c("parsed", "raw")

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

  if (!("none" %in% keep_data)) {
    raw_data <- abif_dat(
      extract_raw_data(
        file_raw,
        out$data_offset,
        out$data_size
      ),
      out$type
    )
  }
  if ("raw" %in% keep_data) out$raw_data <- raw_data
  if ("parsed" %in% keep_data) out$parsed_data <- parse_dat(raw_data)

  structure(out, class = "abif_dir")
}

#' Create a new ABIF directory object from raw data.
#'
#' @param dir_offset integer. Offset of the directory entry in file_raw.
#' @param file_raw  raw. Data containing the directory entry.
#' @param keep_data character. Specifies what directory entry data to keep
#' (raw, parsed, both, or none).
#'
#' @returns abif_dir object containing the directory entry information.
#' @export
#'
abif_dir <- function(dir_offset,
                     file_raw,
                     keep_data = c("parsed", "raw", "both", "none")) {
  new_abif_dir(dir_offset, file_raw, keep_data)
}

#' @export
print.abif_dir <- function(x, ...) {
  stopifnot(class(x) == "abif_dir")

  cat(
    "ABIF directory entry\n",
    "Name:", x$name, "\n",
    "Number:", x$num, "\n",
    "Type:", x$type,
    paste0("(", element_types(x$type, "name"), ")"), "\n",
    "Element size:", x$element_size, "bytes\n",
    "Num. elements:", format(x$num_elements, big.mark = ","), "\n",
    "Data size:", format(x$data_size, big.mark = ","), "bytes\n",
    "Data offset:", x$data_offset, "\n"
  )
  if (!is.null(attr(x, "special_type"))) {
    cat(
      "Special type:", attr(x, "special_type")
    )
  }
  invisible(x)
}

#' Extracts the name from an ABIF directory object.
#'
#' When include_num == TRUE, includes the directory entry number in the output
#' in the following format: "Name.Num". When FALSE (the default), only the name
#' is returned.
#'
#' @param directory abif_dir. The ABIF directory object.
#'
#' @param include_num logical. Whether to include the directory entry number.
#'
#' @returns character. The directory entry name.
#' @export
#'
dir_name <- function(directory, include_num = FALSE) {
  UseMethod("dir_name")
}

#' @export
dir_name.abif_dir <- function(directory, include_num) {
  out <- directory$name
  if (include_num) out <- paste0(out, ".", dir_num(directory))
  out
}

#' Extracts the number of an ABIF directory object.
#'
#' @param directory abif_dir. The ABIF directory object.
#'
#' @returns integer. The directory entry number.
#' @export
#'
dir_num <- function(directory) {
  UseMethod("dir_num")
}

#' @export
dir_num.abif_dir <- function(directory) {
  directory$num
}

#' Extracts the element type of an ABIF directory object.
#'
#' @param directory abif_dir. The ABIF directory object.
#'
#' @returns integer. The directory entry element type.
#' @export
#'
#' @seealso [element_types()]
#'
dir_type <- function(directory) {
  UseMethod("dir_type")
}

#' @export
dir_type.abif_dir <- function(directory) {
  directory$type
}

#' Extracts the element size of an ABIF directory object.
#'
#' @param directory abif_dir. The ABIF directory object.
#'
#' @returns integer. The directory entry element size in bytes.
#' @export
#'
dir_element_size <- function(directory) {
  UseMethod("dir_element_size")
}

#' @export
dir_element_size.abif_dir <- function(directory) {
  directory$element_size
}

#' Extracts the number of elements from an ABIF directory object.
#'
#' @param directory abif_dir. The ABIF directory object.
#'
#' @returns integer. The directory entry's number of elements.
#' @export
#'
dir_n <- function(directory) {
  UseMethod("dir_n")
}

#' @export
dir_n.abif_dir <- function(directory) {
  directory$num_elements
}

#' Extracts the data size of an ABIF directory object.
#'
#' @param directory abif_dir. The ABIF directory object.
#'
#' @returns integer. The directory entry's data size in bytes.
#' @export
#'
dir_size <- function(directory) {
  UseMethod("dir_size")
}

#' @export
dir_size.abif_dir <- function(directory) {
  directory$data_size
}

#' Extracts the file offset of an ABIF directory object.
#'
#' @param directory abif_dir. The ABIF directory object.
#'
#' @returns integer. The directory entry's file offset.
#' @export
#'
dir_offset <- function(directory) {
  UseMethod("dir_offset")
}

#' @export
dir_offset.abif_dir <- function(directory) {
  directory$data_offset
}

#' Extracts the data of an ABIF directory object.
#'
#' @param directory abif_dir. The ABIF directory object.
#' @param what character. Specifies what directory entry data to keep.
#' (raw, parsed, both).
#'
#' @returns list containing the directory entry's data
#' @export
#'
dir_data <- function(directory, what = c("parsed", "raw", "both")) {
  UseMethod("dir_data")
}

#' @export
dir_data.abif_dir <- function(directory,
                              what = c("parsed", "raw", "both")) {
  if ("both" %in% what) what <- c("parsed", "raw")

  out <- directory[paste0(what, "_data")]

  invisible(out)
}
