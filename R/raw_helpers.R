#' Parse signed 32 bit integers from a raw vector
#'
#' @param f Raw vector
#' @param n The maximum number of records to be read. Passed to `readBin()`.
#' @param ... Other arguments to pass to `readBin()`
#'
#' @returns Integer vector
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

#' Parses unsigned 32 bit integers from a raw vector
#'
#' @param f Raw vector
#' @param n The maximum number of records to be read. Passed to `readBin()`.
#' @param ... Other arguments to pass to `readBin()`
#'
#' @returns Integer vector
#'
uint32 <- function(f, n = 1L, ...) {
  readBin(
    f,
    what = "integer",
    signed = FALSE,
    endian = "big",
    size = 4,
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
#'
float64 <- function(f, n = 1L, ...) {
  readBin(f, what = "numeric", endian = "big", size = 8, n = 1L, ...)
}

#' Parses strings from raw vector
#'
#' @param x raw vector
#' @param ... Other arguments to pass to `rawToChar()`
#'
#' @returns Character vector
#'
rtc <- function(x, ...) {
  suppressWarnings(rawToChar(x, ...))
}


#' Parses a date from a raw vector
#'
#' @param f Raw vector
#'
#' @returns Date vector of length 1
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
#'
hidTime <- function(f) {
  list(
    hour = uint8(data, n = 1),
    minute = uint8(data[-1], n = 1),
    second = uint8(data[-(1:2)], n = 1),
    hsecond = uint8(data[-(1:3)], n = 1)
  )
}
