make_example_hids <- function() {
  ex <- hid(test_path("fixtures/A02_RD14-0003-15d2U60-0.25GF-Q4.5_01.15sec.hid"))
  ex_raw <- hid(test_path("fixtures/A02_RD14-0003-15d2U60-0.25GF-Q4.5_01.15sec.hid"),
                raw = TRUE)
  saveRDS(ex, test_path("fixtures/example_hid.RDS"))
  saveRDS(ex_raw, test_path("fixtures/example_hidraw.RDS"))
}

example_hid <- function(raw = FALSE) {
  path <- paste0(
    "fixtures/",
    ifelse(raw, "example_hidraw.RDS", "example_hid.RDS")
  )
  readRDS(test_path(path))
}
