example_hid <- function(raw = FALSE) {
  path <- paste0(
    "fixtures/",
    ifelse(raw, "example_hidraw.RDS", "example_hid.RDS")
  )
  readRDS(test_path(path))
}
