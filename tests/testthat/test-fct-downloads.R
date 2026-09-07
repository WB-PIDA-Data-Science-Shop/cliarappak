# Unit tests for the pure helper functions in R/fct_downloads.R,
# R/fct_family.R and R/fct_remove_avg.R.

test_that("check_quantiles() flags zero interquartile spread", {
  expect_true(check_quantiles(rep(1, 10)))
  expect_true(check_quantiles(c(2, 2, 2, 2, 5)))   # q25 == q75 == 2
  expect_false(check_quantiles(1:100))
  expect_false(check_quantiles(c(NA_real_, NA_real_)))
})

test_that("remove_average_items() drops only the 'Average' entries", {
  x <- c("Political institutions",
         "Political institutions - Average",
         "Justice institutions")
  expect_identical(
    remove_average_items(x),
    c("Political institutions", "Justice institutions")
  )
  expect_identical(remove_average_items(character(0)), character(0))
})

test_that("make_colnames_unique() de-duplicates column names", {
  df <- data.frame(a = 1, b = 2)
  names(df) <- c("x", "x")
  out <- make_colnames_unique(df)
  expect_identical(names(out), c("x", "x.1"))
  expect_equal(unname(unlist(out)), c(1, 2))
})

test_that("trends_check_data() detects gaps in a country's series", {
  raw <- data.frame(
    country_name = rep(c("X", "Y"), each = 3),
    Year = rep(2000:2002, 2),
    foo = c(1, NA, 3, 4, 5, 6)
  )
  expect_true(trends_check_data(2000, 2002, "X", "foo", raw))   # NA in 2001
  expect_false(trends_check_data(2000, 2000, "X", "foo", raw))  # 2000 only
  expect_false(trends_check_data(2000, 2002, "Y", "foo", raw))  # complete
})

test_that("check_spatial_data() reports fully-missing indicators", {
  db <- data.frame(var_name = "Indicator One", variable = "ind1")
  expect_true(
    check_spatial_data(data.frame(value_ind1 = c(NA, NA)), "Indicator One", db)
  )
  expect_false(
    check_spatial_data(data.frame(value_ind1 = c(NA, 0.5)), "Indicator One", db)
  )
})

test_that("check_data() single-indicator mode reports base-country gaps", {
  db <- data.frame(var_name = c("Ind A", "Ind B"), variable = c("a", "b"))
  data <- data.frame(country_name = c("X", "Y"), a = c(NA, 1), b = c(2, 3))
  expect_true(check_data(data, "X", "Ind A", db_variables = db))
  expect_false(check_data(data, "X", "Ind B", db_variables = db))
})
