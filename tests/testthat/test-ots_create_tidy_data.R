context("create tidy data")

# ots_create_tidy_data connects to the API and returns valid tables with a valid input ----

# Mock countries test inside ots_create_tidy_data

test_that("valid input + no cache = yr(p)(c) table", {
  skip_on_cran()
  vcr::use_cassette(name = "chl_arg_2002_yrpc", {
    # Bilateral trade Chile-Argentina at commodity level (2002)
    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", partners = "arg", table = "yrpc"
    )
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 14)
    
    # Bilateral trade Chile-Argentina at aggregated level (2002)
    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", partners = "arg", table = "yrp"
    )
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 7)
    
    # Chilean trade at commodity level (2002)
    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", table = "yrc"
    )
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 12)
    
    # Chilean trade at aggregated level (2002)
    test_data <- ots_create_tidy_data(years = 2002, reporters = "chl", 
                                      table = "yr")
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 5)
    
    # Commodity trade at aggregated level (2002)
    test_data <- ots_create_tidy_data(years = 2002, table = "yc")
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 10)
  })
})

test_that("valid input + cache = yrpc table", {
  skip_on_cran()
  vcr::use_cassette(name = "chl_arg_2002_yrpc_cache", {
    # test in memory cache
    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", partners = "arg", table = "yrpc",
      use_cache = TRUE
    )
    # test file cache
    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", partners = "arg", table = "yrpc",
      use_cache = TRUE, file = tempfile("data")
    )
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 14)
  })
})

test_that("valid input + no cache + commodity filter = yrpc table", {
  skip_on_cran()
  vcr::use_cassette(name = "chl_arg_2002_yrpc_wheat", {
    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", partners = "arg", table = "yrpc",
      commodities = "110100"
    )
    
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 14)
  })
})

test_that("valid input + no cache + group filter = yrpc table", {
  skip_on_cran()
  vcr::use_cassette(name = "chl_arg_2002_yrpc_fish", {
    # filter group 03 = fish and crustaceans

    # test_data <- ots_create_tidy_data(
    #   years = 2002, reporters = "chl", partners = "arg", table = "yrpc"
    # )

    # str(test_data)

    # library(dplyr)
    
    # test_data %>%
    #  distinct(chapter_code)

    # test_data %>%
    #   mutate(
    #     x = substr(commodity_code, 1, 2)
    #   ) %>%
    #   filter(x == "03")

    # 03031 just to make the test faster
    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", partners = "arg", table = "yrpc",
      commodities = "03031"
    )

    # str(test_data)
    
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 14)
  })
})

test_that("valid input + no cache + group filter = yrpc table", {
  skip_on_cran()
  vcr::use_cassette(name = "chl_arg_2002_yrpc_fish_3codes", {
    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", partners = "arg", table = "yrpc",
      commodities = c("030311", "030312", "030319")
    )
    
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 14)
    expect_equal(nrow(test_data), 3)
  })
})

test_that("valid input + no cache + group filter = yrpc table", {
  skip_on_cran()
  vcr::use_cassette(name = "chl_arg_2002_yrpc_fish_chapter", {
    # load_all()

    # library(dplyr)

    # test_data_2 <- ots_create_tidy_data(
    #   years = 2002, reporters = "chl", partners = "arg", table = "yrpc"
    # ) %>%
    #   filter(chapter_code == "03")

    # dim(test_data_2)

    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", partners = "arg", table = "yrpc",
      chapters = "03"
    )

    # dim(test_data)

    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 14)

    # sort(unique(ots_commodities$chapter_code))
    # sort(unique(ots_commodities$section_code))

    # unique(substr(test_data$commodity_code, 1, 2))

    expect_equal(unique(substr(test_data$commodity_code, 1, 2)), "03")
  })
})

test_that("valid input + no cache + group filter = yrpc table", {
  skip_on_cran()
  vcr::use_cassette(name = "chl_arg_2002_yrpc_vegetables_chapter", {
    # load_all()

    # library(dplyr)

    # test_data_2 <- ots_create_tidy_data(
    #   years = 2002, reporters = "chl", partners = "arg", table = "yrpc"
    # ) %>%
    #   filter(section_code == "02")

    # dim(test_data_2)

    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", partners = "arg", table = "yrpc",
       sections = "02"
    )

    # dim(test_data)

    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 14)

    # sort(unique(ots_commodities$chapter_code))
    # sort(unique(ots_commodities$section_code))

    # unique(substr(test_data$commodity_code, 1, 2))
  })
})

test_that("unused commodities argument = yr table + warning", {
  skip_on_cran()
  vcr::use_cassette(name = "chl_arg_2002_yr_apple", {
    test_data <- expect_warning(
      ots_create_tidy_data(years = 2002, table = "yr", commodities = "apple")
    )
    
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 5)
  })
})

test_that("no API data = warning", {
  skip_on_cran()
  vcr::use_cassette(name = "chl_yug_2002_yrp", {
    expect_warning(
      ots_create_tidy_data(
        years = 2002, reporters = 'chl', partners = 'yug', table = "yrp"
      )
    )
  })
})

test_that("valid mixed country ISO/string = yrp table", {
  skip_on_cran()
  vcr::use_cassette(name = "chl_arg_2002_yr", {
    expect_s3_class(
      ots_create_tidy_data(
        years = 2002, reporters = c("Argentina","chl"), table = "yr"
      ),
      "data.frame"
    )
    
    expect_s3_class(
      ots_create_tidy_data(
        years = 2002, reporters = "mex", partners = c("Canada","usa"),
        table = "yrp"
      ),
      "data.frame"
    )
  })
})

test_that("wrong YR input = error + warning", {
  skip_on_cran()
  
  # Bilateral trade ABC-ARG fake ISO codes (2002) - Error message
  expect_error(
    expect_warning(
      ots_create_tidy_data(years = 2002, reporters = "abc", partners = "arg"),
      "After ignoring the unmatched reporter strings"
    )
  )
  
  # Bilateral trade CHL-ABC fake ISO code (2002) - Error message
  expect_error(
    expect_warning(
      ots_create_tidy_data(years = 2002, reporters = "chl", partners = "abc"),
      "After ignoring the unmatched partner strings"
    )
  )
  
  # Bilateral trade USA (1776) - Error message
  expect_error(
    ots_create_tidy_data(years = 1776, reporters = "usa", partners = "all"),
    "Provided that the table you requested contains a 'year' field"
  )
  
  # Bilateral trade Chile-Argentina with fake table (2002) - Error message
  expect_error(
    ots_create_tidy_data(years = 2002, reporters = "chl", partners = "arg", 
                         table = "abc"),
    "requested table does not exist"
  )
})

test_that("invalid cache/file input = error + warning", {
  skip_on_cran()
  
  # Incorrect parameters
  expect_error(
    expect_warning(
      ots_create_tidy_data(
        years = 2002, reporters = "arg", partners = "chl",
        use_cache = 200100,
        file = "foo.bar"
      ),
      "After ignoring the unmatched reporter strings"
    )
  )
  
  expect_error(
    expect_warning(
      ots_create_tidy_data(
        years = 2002, reporters = "arg", partners = "chl",
        use_cache = TRUE,
        file = 200100
      ),
      "After ignoring the unmatched reporter strings"
    )
  )
})

test_that("non-existing product code = error", {
  skip_on_cran()
  
  expect_error(
    ots_create_tidy_data(
      years = 2002, reporters = "chl", partners = "arg", table = "yrpc",
      commodities = "0000"
    )
  )
})

test_that("non-existing product string = error + warning", {
  skip_on_cran()
  
  vcr::use_cassette(name = "chl_arg_2002_yrpc", {
    expect_error(
      expect_warning(
        ots_create_tidy_data(
          years = 2002, reporters = "chl", partners = "arg", table = "yrpc",
          commodities = "kriptonite"
        )
      )
    )
  })
})

test_that("no country match = error", {
  skip_on_cran()
  
  expect_error(
    ots_create_tidy_data(
      years = 2002, reporters = "Wakanda", table = "yr"
    )
  )
  
  expect_error(
    ots_create_tidy_data(
      years = 2002, reporters = "usa", partners = "Wakanda", table = "yrp"
    )
  )
  
  expect_error(
    ots_create_tidy_data(
      years = 2002, reporters = "", table = "yr"
    )
  )
})

test_that("wrong optional parameters = error", {
  skip_on_cran()
  
  # Incorrect parameters
  expect_error(
    ots_create_tidy_data(
      years = 2002, reporters = "arg", partners = "chl",
      max_attempts = 0
    )
  )
  
  expect_error(
    ots_create_tidy_data(
      years = 2002, reporters = "arg", partners = "chl",
      use_localhost = 0
    )
  )
})
