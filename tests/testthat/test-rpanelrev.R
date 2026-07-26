make_panel_data <- function() {
  data.frame(
    country = rep(c("A", "B"), each=3L),
    year    = rep(2020:2022, 2L),
    y       = c(10, 20, 30, 40, 50, 60),
    x       = c(1, 2, 3, 4, 5, 6)
  )
}


test_that("panelrev creates correctly reversed prefixed variables", {
  data <- make_panel_data()
  
  result <- panelrev(
    data       = data,
    vars       = c("y", "x"),
    dimensions = c("country", "year"),
    prefix     = "rev_"
  )
  
  expect_type(result, "list")
  expect_named(result, c("data", "model"))
  expect_s3_class(result$data, "data.frame")
  expect_null(result$model)
  
  expect_named(
    result$data,
    c("country", "year", "y", "x", "rev_y", "rev_x")
  )
  
  expect_equal(
    result$data$rev_y,
    c(30, 20, 10, 60, 50, 40)
  )
  
  expect_equal(
    result$data$rev_x,
    c(3, 2, 1, 6, 5, 4)
  )
  
  expect_identical(result$data$y, data$y)
  expect_identical(result$data$x, data$x)
  expect_identical(result$data$country, data$country)
  expect_identical(result$data$year, data$year)
})

test_that("panelrev replaces original variables when requested", {
  data <- make_panel_data()
  
  result <- panelrev(
    data       = data,
    vars       = c("y", "x"),
    dimensions = c("country", "year"),
    replace    = TRUE
  )
  
  expect_type(result, "list")
  expect_named(result, c("data", "model"))
  expect_s3_class(result$data, "data.frame")
  expect_null(result$model)
  
  expect_equal(
    result$data$y,
    c(30, 20, 10, 60, 50, 40)
  )
  
  expect_equal(
    result$data$x,
    c(3, 2, 1, 6, 5, 4)
  )
  
  expect_false("rev_y" %in% names(result$data))
  expect_false("rev_x" %in% names(result$data))
  
  expect_identical(result$data$country, data$country)
  expect_identical(result$data$year, data$year)
})

test_that("panelrev reverses values by the final declared dimension", {
  data <- data.frame(
    country = c("A", "A", "A", "B", "B", "B"),
    year    = c(2022, 2020, 2021, 2021, 2022, 2020),
    y       = c(30, 10, 20, 50, 60, 40)
  )
  
  result <- panelrev(
    data       = data,
    vars       = "y",
    dimensions = c("country", "year"),
    prefix     = "rev_"
  )
  
  expect_identical(result$data$country, data$country)
  expect_identical(result$data$year, data$year)
  
  expect_equal(
    result$data$rev_y,
    c(10, 30, 20, 50, 40, 60)
  )
})

test_that("panelrev handles a single reversal dimension", {
  data <- data.frame(
    year = 2020:2023,
    y    = c(10, 20, 30, 40)
  )
  
  result <- panelrev(
    data       = data,
    vars       = "y",
    dimensions = "year"
  )
  
  expect_equal(
    result$data$rev_y,
    c(40, 30, 20, 10)
  )
  
  expect_identical(result$data$year, data$year)
  expect_null(result$model)
})

test_that("panelrev reverses missing values as ordinary values", {
  data <- data.frame(
    country = rep(c("A", "B"), each=3L),
    year    = rep(2020:2022, 2L),
    y       = c(10, NA_real_, 30, 40, 50, NA_real_)
  )
  
  result <- panelrev(
    data       = data,
    vars       = "y",
    dimensions = c("country", "year")
  )
  
  expect_equal(
    result$data$rev_y,
    c(30, NA_real_, 10, NA_real_, 50, 40)
  )
})

test_that("panelrev runs an estimation function after reversal", {
  data <- make_panel_data()
  
  result <- panelrev(
    data       = data,
    vars       = c("y", "x"),
    dimensions = c("country", "year"),
    replace    = TRUE,
    estimate   = stats::lm,
    formula    = y ~ x
  )
  
  expect_type(result, "list")
  expect_named(result, c("data", "model"))
  expect_s3_class(result$data, "data.frame")
  expect_s3_class(result$model, "lm")
  
  expect_equal(
    result$data$y,
    c(30, 20, 10, 60, 50, 40)
  )
  
  expect_equal(
    result$data$x,
    c(3, 2, 1, 6, 5, 4)
  )
  
  expect_true(all(is.finite(stats::coef(result$model))))
  
  expect_equal(
    unname(stats::coef(result$model)),
    c(0, 10),
    tolerance = 1e-10
  )
  
  expect_equal(
    as.numeric(stats::fitted(result$model)),
    result$data$y,
    tolerance = 1e-10
  )
})

test_that("panelrev estimates using prefixed variables", {
  data <- make_panel_data()
  
  result <- panelrev(
    data       = data,
    vars       = c("y", "x"),
    dimensions = c("country", "year"),
    prefix     = "rev_",
    estimate   = stats::lm,
    formula    = rev_y ~ rev_x
  )
  
  expect_s3_class(result$model, "lm")
  expect_true(all(c("rev_y", "rev_x") %in% names(result$data)))
  
  expect_equal(
    as.numeric(stats::fitted(result$model)),
    result$data$rev_y,
    tolerance = 1e-10
  )
})

test_that("panelrev applies pre-estimation before model estimation", {
  data <- make_panel_data()
  
  result <- panelrev(
    data       = data,
    vars       = c("y", "x"),
    dimensions = c("country", "year"),
    replace    = TRUE,
    preestimation = function(data) {
      data$x_squared <- data$x^2
      data
    },
    estimate = stats::lm,
    formula  = y ~ x_squared
  )
  
  expect_s3_class(result$data, "data.frame")
  expect_s3_class(result$model, "lm")
  
  expect_true("x_squared" %in% names(result$data))
  expect_equal(result$data$x_squared, result$data$x^2)
  
  model_terms <- attr(stats::terms(result$model), "term.labels")
  expect_true("x_squared" %in% model_terms)
})

test_that("panelrev applies post-estimation and returns predictions", {
  data <- make_panel_data()
  
  result <- panelrev(
    data       = data,
    vars       = c("y", "x"),
    dimensions = c("country", "year"),
    replace    = TRUE,
    estimate   = stats::lm,
    formula    = y ~ x,
    na.action  = stats::na.exclude,
    postestimation = function(data, model) {
      prediction <- stats::predict(
        model,
        newdata = data,
        se.fit  = TRUE
      )
      
      data$prediction <- as.numeric(prediction$fit)
      data$stdp       <- as.numeric(prediction$se.fit)
      data$residual   <- as.numeric(stats::residuals(model))
      
      list(
        data  = data,
        model = model
      )
    }
  )
  
  expect_type(result, "list")
  expect_named(result, c("data", "model"))
  expect_s3_class(result$data, "data.frame")
  expect_s3_class(result$model, "lm")
  
  expect_true(
    all(c("prediction", "stdp", "residual") %in% names(result$data))
  )
  
  expect_length(result$data$prediction, nrow(data))
  expect_length(result$data$stdp, nrow(data))
  expect_length(result$data$residual, nrow(data))
  
  expect_true(all(is.finite(result$data$prediction)))
  expect_true(all(is.finite(result$data$stdp)))
  expect_true(all(is.finite(result$data$residual)))
  
  expect_equal(
    result$data$prediction,
    result$data$y,
    tolerance = 1e-10
  )
  
  expect_equal(
    result$data$residual,
    rep(0, nrow(data)),
    tolerance = 1e-10
  )
})

test_that("panelrev permits additional post-estimation components", {
  data <- make_panel_data()
  
  result <- panelrev(
    data       = data,
    vars       = c("y", "x"),
    dimensions = c("country", "year"),
    replace    = TRUE,
    estimate   = stats::lm,
    formula    = y ~ x,
    postestimation = function(data, model) {
      list(
        data         = data,
        model        = model,
        coefficients = stats::coef(model)
      )
    }
  )
  
  expect_type(result, "list")
  expect_true(
    all(c("data", "model", "coefficients") %in% names(result))
  )
  
  expect_equal(
    result$coefficients,
    stats::coef(result$model)
  )
})

test_that("panelrev rejects invalid data inputs", {
  expect_error(
    panelrev(
      data       = matrix(1:4, nrow = 2L),
      vars       = "y",
      dimensions = "time"
    ),
    regexp = "data.*data frame"
  )
  
  empty_data <- data.frame(
    year = integer(),
    y    = numeric()
  )
  
  expect_error(
    panelrev(
      data       = empty_data,
      vars       = "y",
      dimensions = "year"
    ),
    regexp = "at least one observation"
  )
})

test_that("panelrev rejects invalid variable specifications", {
  data <- make_panel_data()
  
  expect_error(
    panelrev(
      data       = data,
      dimensions = c("country", "year")
    ),
    regexp = "vars.*non-empty character vector"
  )
  
  expect_error(
    panelrev(
      data       = data,
      vars       = character(),
      dimensions = c("country", "year")
    ),
    regexp = "vars.*non-empty character vector"
  )
  
  expect_error(
    panelrev(
      data       = data,
      vars       = c("y", "y"),
      dimensions = c("country", "year")
    ),
    regexp = "vars.*duplicated"
  )
  
  expect_error(
    panelrev(
      data       = data,
      vars       = "unknown",
      dimensions = c("country", "year")
    ),
    regexp = "Variables not found"
  )
  
  expect_error(
    panelrev(
      data       = data,
      vars       = "year",
      dimensions = c("country", "year")
    ),
    regexp = "cannot also be dimension"
  )
})

test_that("panelrev rejects invalid dimension specifications", {
  data <- make_panel_data()
  
  expect_error(
    panelrev(
      data = data,
      vars = "y"
    ),
    regexp = "dimensions.*non-empty character vector"
  )
  
  expect_error(
    panelrev(
      data       = data,
      vars       = "y",
      dimensions = c("country", "country", "year")
    ),
    regexp = "dimensions.*duplicated"
  )
  
  expect_error(
    panelrev(
      data       = data,
      vars       = "y",
      dimensions = c("country", "unknown")
    ),
    regexp = "Dimension variables not found"
  )
  
  missing_dimension_data <- data
  missing_dimension_data$year[1L] <- NA_integer_
  
  expect_error(
    panelrev(
      data       = missing_dimension_data,
      vars       = "y",
      dimensions = c("country", "year")
    ),
    regexp = "Missing values.*dimension"
  )
  
  duplicated_data <- rbind(data, data[1L, ])
  
  expect_error(
    panelrev(
      data       = duplicated_data,
      vars       = "y",
      dimensions = c("country", "year")
    ),
    regexp = "unique observation"
  )
})

test_that("panelrev validates replacement arguments", {
  data <- make_panel_data()
  
  expect_error(
    panelrev(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      replace    = NA
    ),
    regexp = "replace.*TRUE or FALSE"
  )
  
  expect_error(
    panelrev(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      prefix     = ""
    ),
    regexp = "prefix.*non-empty character string"
  )
  
  data$rev_y <- NA_real_
  
  expect_error(
    panelrev(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      prefix     = "rev_"
    ),
    regexp = "output variables already exist"
  )
})

test_that("panelrev validates estimation arguments", {
  data <- make_panel_data()
  
  expect_error(
    panelrev(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      preestimation = "not a function"
    ),
    regexp = "preestimation.*NULL or a function"
  )
  
  expect_error(
    panelrev(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      estimate   = "not a function",
      formula    = y ~ x
    ),
    regexp = "estimate.*NULL or a function"
  )
  
  expect_error(
    panelrev(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      formula    = y ~ x
    ),
    regexp = "formula.*estimate.*NULL"
  )
  
  expect_error(
    panelrev(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      estimate   = stats::lm
    ),
    regexp = "formula.*must be supplied"
  )
  
  expect_error(
    panelrev(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      estimate   = stats::lm,
      formula    = "y ~ x"
    ),
    regexp = "formula.*must be a formula"
  )
  
  expect_error(
    panelrev(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      postestimation = function(data, model) {
        list(data = data, model = model)
      }
    ),
    regexp = "postestimation.*estimate.*NULL"
  )
})

test_that("panelrev validates pre-estimation return values", {
  data <- make_panel_data()
  
  condition <- tryCatch(
    panelrev(
      data       = data,
      vars       = "y",
      dimensions = c("country", "year"),
      preestimation = function(data) {
        data$y
      }
    ),
    error = identity
  )
  
  expect_s3_class(condition, "error")
  
  expect_match(
    conditionMessage(condition),
    "preestimation.*data frame"
  )
})

test_that("panelrev validates post-estimation return structure", {
  data <- make_panel_data()
  
  condition <- tryCatch(
    panelrev(
      data       = data,
      vars       = c("y", "x"),
      dimensions = c("country", "year"),
      replace    = TRUE,
      estimate   = stats::lm,
      formula    = y ~ x,
      postestimation = function(data, model) {
        data
      }
    ),
    error = identity
  )
  
  expect_s3_class(condition, "error")
  
  expect_match(
    conditionMessage(condition),
    "postestimation.*named list.*data.*model"
  )
})

test_that("panelrev requires data and model post-estimation components", {
  data <- make_panel_data()
  
  condition <- tryCatch(
    panelrev(
      data       = data,
      vars       = c("y", "x"),
      dimensions = c("country", "year"),
      replace    = TRUE,
      estimate   = stats::lm,
      formula    = y ~ x,
      postestimation = function(data, model) {
        list(data = data)
      }
    ),
    error = identity
  )
  
  expect_s3_class(condition, "error")
  
  error_message <- conditionMessage(condition)
  
  expect_match(error_message, "postestimation")
  expect_match(error_message, "named list")
  expect_match(error_message, "data")
  expect_match(error_message, "model")
})

test_that("panelrev requires post-estimation data to be a data frame", {
  data <- make_panel_data()
  
  condition <- tryCatch(
    panelrev(
      data       = data,
      vars       = c("y", "x"),
      dimensions = c("country", "year"),
      replace    = TRUE,
      estimate   = stats::lm,
      formula    = y ~ x,
      postestimation = function(data, model) {
        list(
          data  = data$y,
          model = model
        )
      }
    ),
    error = identity
  )
  
  expect_s3_class(condition, "error")
  
  expect_match(
    conditionMessage(condition),
    "data.*postestimation.*data frame"
  )
})
