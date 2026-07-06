context("strings processing")

# ots_sector_code ----

test_that("ots_sector_code works for a partial sector name match", {
  d <- ots_sector_code("agri")
  expect_is(d, "data.frame")
  expect_equal(nrow(d), 1L)
  expect_equal(ncol(d), 3L)
})

test_that("ots_sector_code returns 0 rows for non-existing sector", {
  d <- ots_sector_code("adamantium")
  expect_is(d, "data.frame")
  expect_equal(nrow(d), 0L)
})

test_that("ots_sector_code returns error for empty string", {
  expect_error(ots_sector_code(""))
})

test_that("ots_sector_code returns error for NULL", {
  expect_error(ots_sector_code(NULL))
})

# ots_industry_code ----

test_that("ots_industry_code works for a partial industry name match", {
  d <- ots_industry_code("cereal")
  expect_is(d, "data.frame")
  expect_equal(nrow(d), 2L)
  expect_equal(ncol(d), 2L)
})

test_that("ots_industry_code works for an exact industry name match", {
  d <- ots_industry_code("wheat")
  expect_is(d, "data.frame")
  expect_equal(nrow(d), 1L)
})

test_that("ots_industry_code returns 0 rows for non-existing industry", {
  d <- ots_industry_code("kriptonite")
  expect_is(d, "data.frame")
  expect_equal(nrow(d), 0L)
})

test_that("ots_industry_code returns error for empty string", {
  expect_error(ots_industry_code(""))
})

test_that("ots_industry_code returns error for NULL", {
  expect_error(ots_industry_code(NULL))
})
