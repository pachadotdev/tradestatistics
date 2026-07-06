context("create tidy data")

# ots_create_tidy_data connects to the API and returns valid tables with a valid input ----

# Mock countries test inside ots_create_tidy_data

test_that("without cache", {
  skip_on_cran()
  vcr::use_cassette(name = "chl_2002_1", {
    # Bilateral trade Chile-Argentina at industry level (2002)
    test_data <- ots_create_tidy_data(years = 2002, importers = "chl", exporters = "arg", sectors = 1L, industries = 1L, table = "itpde")
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 10)
    
    # Bilateral trade Chile-Argentina at aggregated level (2002)
    test_data <- ots_create_tidy_data(years = 2002, importers = "chl", exporters = "arg", table = "itpde_imp_exp")
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 6)
    
    # Chilean trade at aggregated level (2002)
    test_data <- ots_create_tidy_data(years = 2002, importers = "chl", table = "itpde_imp")
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 4)
    
    # Chilean trade at sector level (2002)
    test_data <- ots_create_tidy_data(years = 2002, importers = "chl", sectors = 1L, table = "itpde_imp_sec")
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 5)
    
    # Chilean trade at industry level (2002)
    test_data <- ots_create_tidy_data(years = 2002, importers = "chl", industries = 1L, table = "itpde_imp_ind")
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 5)
  })
})

test_that("with cache", {
  skip_on_cran()
  vcr::use_cassette(name = "chl_2002_2", {
    # test in memory cache
    test_data <- ots_create_tidy_data(
      years = 2002, importers = "chl", exporters = "arg", sectors = 1L,
      industries = 1L, table = "itpde", use_cache = TRUE)
    # test file cache
    test_data <- ots_create_tidy_data(
      years = 2002, importers = "chl", exporters = "arg", sectors = 1L,
      industries = 1L, table = "itpde", use_cache = TRUE, file = tempfile("data"))
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 10)
  })
})

test_that("unused commodities argument = yr table + warning", {
  skip_on_cran()
  vcr::use_cassette(name = "chl_2002_3", {
    test_data <- expect_warning(ots_create_tidy_data(
      years = 2002, importers = "chl", exporters = "arg", sectors = 1L,
      industries = 1L, table = "itpde_imp", use_cache = TRUE))
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 4)
  })
})

test_that("no API data = warning", {
  skip_on_cran()
  vcr::use_cassette(name = "chl_2002_4", {
    expect_warning(
      ots_create_tidy_data(
        years = 2002, importers = 'chl', exporters = 'yug', table = "itpde_imp_exp"
      )
    )
  })
})

test_that("mix country name + country code", {
  skip_on_cran()
  vcr::use_cassette(name = "chl_2002_5", {
    expect_equal(
      nrow(ots_create_tidy_data(years = 2002, importers = c("Argentina","chl"), table = "itpde_imp")),
      2L
    )
  })
})

test_that("no country match", {  
  expect_error(
    ots_create_tidy_data(years = 2002, importers = "Wakanda", table = "itpde_imp")
  )

  expect_error(
    ots_create_tidy_data(years = 2002, importers = "Wakanda", table = "itpde")
  )
})

test_that("wrong optional parameters = error", {  
  # Incorrect parameters
  expect_error(
    ots_create_tidy_data(
      years = 2002, importers = "arg", exporters = "chl",
      max_attempts = 0
    )
  )
  
  expect_error(
    ots_create_tidy_data(
      years = 2002, importers = "arg", exporters = "chl",
      use_localhost = 0
    )
  )
})
