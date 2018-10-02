context("scraping")

test_that("get_all_articles_tbl works with less than 100 entries", {
  tbl <- get_all_articles_tbl(101623795, 50)
  expect_equal(nrow(tbl), 50)
})

tbl <- get_all_articles_tbl(101623795, 250)

test_that("get_all_articles_tbl works with more than 100 entries", {
  expect_equal(nrow(tbl), 250)
})
test_that("get_all_articles_tbl returns 5 columns",{
  expect_length(tbl, 5)
})
test_that("get_all_articles_tbl returns 3 date columns ",{
  stringr::str_detect(c(tbl$received, tbl$accepted, tbl$pubmed), "^[12][90][0-9]{2}-[01][0-9]-[0-3][0-9]$") %>%
    all(na.rm = TRUE) %>%
    expect_true()
})
