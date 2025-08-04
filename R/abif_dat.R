new_abif_dat <- function(data, type) {
  stopifnot(is.integer(type))

  attr(data, "type") <- type
  attr(data, "is_raw") <- is.raw(data)

  structure(data, class = c("abif_dat", class(data)))
}

#' Creates an `abif_dat` object, which represents a data element from an ABIF
#' file.
#'
#' @param data atomic vector containing the data
#' @param type integer. The element type of the data
#'
#' @returns abif_dat object.
#' @export
#'
abif_dat <- function(data, type) {
  new_abif_dat(data, type)
}

#' @export
print.abif_dat <- function(x, ...) {
  type_num <- attr(x, "type")
  type_name <- element_types(type_num, "name")
  is_raw <- attr(x, "is_raw")

  cat(
    "ABIF data object\n",
    "Data type:", type_num, paste0("(", type_name, ")\n"),
    "Num. elements:", length(x), "\n",
    "Raw data:", is_raw
  )

  invisible(x)
}

#' Parses the raw data associated with an ABIF data object (`abif_dat`).
#'
#' @param x abif_dat object containing raw data.
#'
#' @returns list, character, or numeric vector, depending on element type.
#' The parsed data from the directory entry.
#' @export
#'
parse_dat <- function(x) {
  UseMethod("parse_dat")
}

#' @export
parse_dat.abif_dat <- function(x) {

  # Only try to parse raw data
  if (!attr(x, "is_raw")) return(x)


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
