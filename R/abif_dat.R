new_abif_dat <- function(data, type) {
  stopifnot(is.integer(type))

  attr(data, "type") <- type
  attr(data, "is_raw") <- is.raw(data)

  structure(data, class = c("abif_dat", class(data)))
}

#' Creates an `abif_dat` object, which represents a data element from an ABIF
#' file.
#'
#' @param data atomic vector containing the data.
#' @param type integer. The ABIF element type for the data.
#'
#' @returns abif_dat object.
#' @export
#'
#' @examples
#' # Create an abif_dat object containing raw data representing signed 16-bit
#' # integer data (element type 4)
#' raw_dat <- abif_dat(as.raw(c(0x00, 0x01)), type = 4L)
#'
#' # An atomic vector of any type can be used for the data:
#' dat <- abif_dat(c(1L:5L), type = 5L)
#'
abif_dat <- function(data, type) {
  new_abif_dat(data, type)
}

#' @export
summary.abif_dat <- function(object, ...) {
  type_num <- attr(object, "type")
  type_name <- element_types(type_num, "name")
  is_raw <- attr(object, "is_raw")

  num_elements <- ifelse(
    is_raw,
    length(object) / element_types(type_num, "size"),
    length(object)
  )

  cat(
    "ABIF data object\n",
    "Data type:", type_num, paste0("(", type_name, ")\n"),
    "Num. elements:", num_elements, "\n",
    "Raw data:", is_raw, "\n"
  )
  NextMethod(object)
}

#' Parses the raw data associated with an ABIF data object (`abif_dat`).
#'
#' Times (element type 11) are returned as a list and dates (element
#' type 10) are returned as a `Date`. Other element types are returned as
#' numeric or character vectors. If x does not contain raw data, the data is
#' returned unchanged.
#'
#' @param x abif_dat object containing raw data.
#'
#' @returns atomic vector, object, or list, containing the parsed data from the
#' directory entry.
#' @export
#'
#' @examples
#' my_dat <- abif_dat(as.raw(0x01), type = 13L)
#' x <- parse_dat(my_dat)
#'
parse_dat <- function(x) {
  UseMethod("parse_dat")
}

#' @export
parse_dat.abif_dat <- function(x) {
  # Only try to parse raw data
  if (!attr(x, "is_raw")) {
    return(x)
  }

  element_type <- attr(x, "type")
  element_size <- element_types(element_type, "size")
  num_elements <- length(x) / element_size

  out <- NULL

  if (element_type == 1L) {
    out <- uint8(x, n = num_elements)
  }
  if (element_type == 2L) {
    out <- char(x)
  }
  if (element_type == 3L) {
    out <- uint16(x, n = num_elements)
  }
  if (element_type == 4L) {
    out <- int16(x, n = num_elements)
  }
  if (element_type == 5L) {
    out <- int32(x, n = num_elements)
  }
  if (element_type == 7L) {
    out <- float32(x, n = num_elements)
  }
  if (element_type == 8L) {
    out <- float64(x, n = num_elements)
  }
  if (element_type == 10L) {
    out <- hidDate(x)
  }
  if (element_type == 11L) {
    out <- hidTime(x)
  }
  if (element_type == 13L) {
    out <- as.logical(x, n = num_elements)
  }
  if (element_type == 18L) {
    out <- pString(x)
  }
  if (element_type == 19L) {
    out <- rtc(x[-length(x)])
  }

  if (!is.null(out)) out <- abif_dat(out, element_type)

  invisible(out)
}

#' Returns vector of the "special" element types found in newer versions of
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

#' Returns the name, element size, substitute element type, or the name of the
#' function used to parse the element type.
#'
#' Return value depends on the `get` parameter:
#'  "name" returns a string with the name of the element type
#'  "size" returns an integer with the element size for the element type\
#'  "sub" returns an integer with the standard element type substitute. For
#'    special element types, this will be the standard element type used to
#'    parse the data. For normal element types, this will be the same as the
#'    element type.
#'  "parse_fun" returns a string with the name of the function used to parse the
#'    element type.
#'
#' @param element_type integer representing the element type to look up.
#' @param get character specifying the data to return.
#'
#' @returns character or integer, depending on value of `get`
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
