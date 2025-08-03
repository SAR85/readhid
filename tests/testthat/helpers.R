make_example_hids <- function() {
  ex <- hid(
    test_path("fixtures/A02_RD14-0003-15d2U60-0.25GF-Q4.5_01.15sec.hid"),
    keep_data = "both"
  )
  ex_noraw <- hid(
    test_path("fixtures/A02_RD14-0003-15d2U60-0.25GF-Q4.5_01.15sec.hid"),
    keep_data = "parsed"
  )
  saveRDS(ex, test_path("fixtures/example_hid.RDS"))
  saveRDS(ex_noraw, test_path("fixtures/example_noraw.RDS"))
}

example_hid <- function(raw = TRUE) {
  path <- paste0(
    "fixtures/",
    ifelse(raw, "example_hid.RDS", "example_noraw.RDS")
  )
  readRDS(test_path(path))
}
