#' Reverse the Order of Values of Time Series and Multidimensional Panels
#'
#' Reverses the values of selected variables along the last dimension of a
#' data frame. Reversal is performed separately for every combination of the
#' preceding dimensions, while all dimension variables remain unchanged.
#'
#' The reversed values can either replace the original variables or be stored
#' in new variables constructed by adding a prefix to the original variable
#' names. Optional pre-estimation, estimation, and post-estimation functions
#' can be evaluated after the reversal.
#'
#' @param data data frame  
#'   Input data containing the dimension variables and the variables to be
#'   reversed.
#'
#' @param vars character vector  
#'   Names of the variables whose values are to be reversed.
#'
#' @param dimensions character vector  
#'   Names of the variables that uniquely identify observations. The last
#'   variable specifies the dimension along which values are reversed, while
#'   all preceding variables define independent groups.
#'
#' @param replace logical scalar, default = \code{FALSE}  
#'   If \code{TRUE}, the original variables are overwritten with their reversed
#'   values. If \code{FALSE}, new variables are created using \code{prefix}.
#'
#' @param prefix character scalar, default = \code{"rev_"}  
#'   Prefix added to the names of reversed variables when
#'   \code{replace = FALSE}.
#'
#' @param preestimation function or \code{NULL}, default = \code{NULL}  
#'   Function evaluated after the selected variables have been reversed and
#'   before estimation. The function must accept the transformed data frame as
#'   its first argument and return a data frame. The returned data frame is
#'   subsequently passed to \code{estimate}.
#'
#' @param estimate function or \code{NULL}, default = \code{NULL}  
#'   Estimation function evaluated after reversal and, when supplied,
#'   \code{preestimation}. The function must accept named arguments
#'   \code{data} and \code{formula}, as well as any additional arguments
#'   supplied through \code{...}. Examples include \code{stats::lm} and
#'   \code{stats::glm}.
#'
#' @param formula formula or \code{NULL}, default = \code{NULL}  
#'   Model formula passed to \code{estimate}. Required when an estimation
#'   function is supplied.
#'
#' @param postestimation function or \code{NULL}, default = \code{NULL}  
#'   Function evaluated after estimation. The function must accept the
#'   transformed data frame as its first argument and the fitted model as its
#'   second argument. It must return the final result as a named list
#'   containing components \code{data} and \code{model}. The function may add
#'   predictions, residuals, standard errors, or other post-estimation
#'   quantities to the transformed data frame.
#'
#' @param ... Optional.  
#'   Additional arguments passed to \code{estimate}.
#'
#' @return
#' A named list containing at least the following components:  
#' \describe{
#'   \item{\code{data}}{
#'     The transformed data frame after reversal and, when supplied,
#'     application of \code{preestimation} and \code{postestimation}.
#'   }
#'   \item{\code{model}}{
#'     The object returned by \code{estimate}, or the model component returned
#'     by \code{postestimation}. If no estimation function is supplied, this
#'     component is \code{NULL}.
#'   }
#' }
#'
#' @details
#' The final variable in \code{dimensions} determines the ordering along which
#' the selected variables are reversed. All preceding dimension variables
#' define independent groups.
#'
#' For example, with
#'
#' \preformatted{
#' dimensions = c("country", "sector", "year")
#' }
#'
#' reversal is performed along \code{year}, separately for each
#' country-sector combination.
#'
#' The dimension variables themselves are not modified, and the original row
#' order of \code{data} is preserved.
#'
#' Missing values in variables listed in \code{vars} are treated as ordinary
#' values and therefore participate in the reversal. Missing values in
#' dimension variables are not permitted.
#'
#' Each combination of the dimension variables must uniquely identify one
#' observation.
#'
#' The order of operations is:
#'
#' \enumerate{
#'   \item Reverse the selected variables.
#'   \item Apply \code{preestimation} to the transformed data.
#'   \item Evaluate \code{estimate}.
#'   \item Apply \code{postestimation} to the transformed data and fitted model.
#' }
#'
#' The \code{preestimation} function must return a data frame. The
#' \code{postestimation} function must return the complete final result as a
#' named list containing \code{data} and \code{model}. This allows predictions,
#' residuals, standard errors, and other post-estimation quantities to be added
#' to the transformed data.
#'
#' When \code{replace = TRUE}, \code{preestimation}, \code{estimate}, and
#' \code{postestimation} use the overwritten variables. When
#' \code{replace = FALSE}, they should refer to the prefixed variable names.
#'
#' @seealso
#' \code{\link[stats]{lm}},
#' \code{\link[stats]{glm}}
#'
#' @examples
#'   ## Example: reversal and linear estimation
#'
#'   data <- data.frame(
#'       country = rep(c("A", "B"), each = 3L),
#'       year    = rep(2020:2022, 2L),
#'       y       = c(10, 20, 30, 40, 50, 60),
#'       x       = c(1, 2, 3, 4, 5, 6)
#'   )
#'
#'   # create prefixed reversed variables
#'   result <- panelrev(
#'       data       = data,
#'       vars       = c("y", "x"),
#'       dimensions = c("country", "year"),
#'       prefix     = "rev_"
#'   )
#'   print(result$data)
#'   print(result$model)                                # NULL
#'
#'   # overwrite variables and estimate a linear model
#'   result <- panelrev(
#'       data       = data,
#'       vars       = c("y", "x"),
#'       dimensions = c("country", "year"),
#'       replace    = TRUE,
#'       estimate   = stats::lm,
#'       formula    = y ~ x
#'   )
#'   print(result$data)
#'   print(result$model)
#'   print(summary(result$model))
#'
#'   # pre-estimation, estimation, and prediction
#'   result <- panelrev(
#'       data       = data,
#'       vars       = c("y", "x"),
#'       dimensions = c("country", "year"),
#'       replace    = TRUE,
#'       preestimation = function(data) {
#'           data$x_squared <- data$x^2
#'           data
#'       },
#'       estimate = stats::lm,
#'       formula  = y ~ x + x_squared,
#'       na.action = stats::na.exclude,
#'       postestimation = function(data, model) {
#'           prediction <- stats::predict(
#'               model,
#'               newdata = data,
#'               se.fit  = TRUE
#'           )
#'
#'           data$prediction <- as.numeric(prediction$fit)
#'           data$stdp       <- as.numeric(prediction$se.fit)
#'           data$residual   <- as.numeric(stats::residuals(model))
#'
#'           list(
#'               data  = data,
#'               model = model
#'           )
#'       }
#'   )
#'   print(result$data)
#'   print(result$model)
#'
#' @export
panelrev <- function(data, vars, dimensions, replace=FALSE, prefix="rev_",
                     preestimation=NULL, estimate=NULL, formula=NULL,
                     postestimation=NULL, ...) {
  # 'data' checks
  if (!is.data.frame(data))
    stop("'data' must be a data frame.",                        call. = FALSE)
  if (nrow(data)                                                      ==   0L)
    stop("'data' must contain at least one observation.",       call. = FALSE)
  # 'vars' checks
  if (missing(vars)            || !is.character(vars)                         ||
      length(vars)       == 0L || anyNA(vars)                                 ||
      any(vars           == ""))
    stop("'vars' must be a non-empty character vector of ",
         "variable names.",                                     call. = FALSE)
  if (anyDuplicated(vars))
    stop("'vars' must not contain duplicated variable names.",  call. = FALSE)
  if (length(missing_vars         <- setdiff(vars, names(data)))        >  0L)
    stop("Variables not found in 'data': ",
         paste(missing_vars,       collapse=", "),        ".",  call. = FALSE)
  # 'dimensions' checks
  if (missing(dimensions)      || !is.character(dimensions)                   ||
      length(dimensions) == 0L || anyNA(dimensions)                           ||
      any(dimensions     == ""))
    stop("'dimensions' must be a non-empty character vector ",
         "of variable names.",                                  call. = FALSE)
  if (anyDuplicated(dimensions))
    stop("'dimensions' must not contain duplicated variable ",
         "names.",                                              call. = FALSE)
  if (length(missing_dimensions   <- setdiff(dimensions, names(data)))  >  0L)
    stop("Dimension variables not found in 'data': ",
         paste(missing_dimensions, collapse=", "),        ".",  call. = FALSE)
  if (length(overlapping_vars     <- intersect(vars,     dimensions))   >  0L)
    stop("Variables in 'vars' cannot also be dimension ",
         "variables: ",
         paste(overlapping_vars,   collapse=", "),        ".",  call. = FALSE)
  # observations checks
  if (anyNA(data[dimensions]))
    stop("Missing values are not permitted in dimension ",
         "variables.",                                          call. = FALSE)
  if (anyDuplicated(data[dimensions]))
    stop("Each combination of dimension variables must ",
         "identify a unique observation.",                      call. = FALSE)
  # arguments checks
  if (!is.logical(replace)     || length(replace)                     !=   1L ||
      is.na(replace))
    stop("'replace' must be either TRUE or FALSE.",             call. = FALSE)
  if (!replace                                                                &&
      (!is.character(prefix)   || length(prefix)                       !=   1L ||
       is.na(prefix)           || prefix == ""))
    stop("'prefix' must be a non-empty character string ",
         "when 'replace = FALSE'.",                             call. = FALSE)
  if (!is.null(preestimation)  && !is.function(preestimation))
    stop("'preestimation' must be NULL or a function.",         call. = FALSE)
  if (!is.null(estimate)       && !is.function(estimate))
    stop("'estimate' must be NULL or a function.",              call. = FALSE)
  if ( is.null(estimate)       && !is.null(formula))
    stop("'formula' cannot be supplied when 'estimate' ",
         "is NULL.",                                            call. = FALSE)
  if (!is.null(estimate)       &&  is.null(formula))
    stop("'formula' must be supplied when 'estimate' is used.", call. = FALSE)
  if (!is.null(formula)        && !inherits(formula, "formula"))
    stop("'formula' must be a formula.",                        call. = FALSE)
  if (!is.null(postestimation) && !is.function(postestimation))
    stop("'postestimation' must be NULL or a function.",        call. = FALSE)
  if (!is.null(postestimation) &&  is.null(estimate))
    stop("'postestimation' cannot be supplied when ",
         "'estimate' is NULL.",                                 call. = FALSE)
  
  # parse vars
  output_vars                     <- if (replace)    
    vars                       else
      paste0(prefix,  vars)
  if (anyDuplicated(output_vars))
    stop("The prefix produces duplicated output variable ",
         "names.",                                              call. = FALSE)
  if (!replace                                                                &&
      length(existing_vars <- intersect(output_vars, names(data)))      >  0L)
    stop("The following output variables already exist: ",
         paste(existing_vars, collapse = ", "),           ".",
         "Use 'replace = TRUE' or choose another prefix.",      call. = FALSE)
  # parse dimensions
  reversal_dimension              <- utils::tail(dimensions,  1L)
  group_dimensions                <- utils::head(dimensions, -1L)
  groups                          <- if (length(group_dimensions) == 0L)
    list(seq_len(nrow(data)))  else
      split(seq_len(nrow(data)),
            do.call(interaction,
                    c(unname(data[group_dimensions]),
                      list(drop=TRUE, lex.order=TRUE))),
            drop=TRUE)
  # perform reverse
  result_data                     <- data
  for (i in seq_along(vars))      {
    source_var                    <- vars[[i]]
    output_var                    <- output_vars[[i]]
    source_values                 <- data[[source_var]]
    reversed_values               <- source_values
    for (group_rows in groups)    {
      rows                        <- group_rows[order(
        data[[reversal_dimension]][group_rows],
        method="radix")]
      reversed_values[rows]       <- rev(source_values[rows])
    }
    result_data[[output_var]]     <- reversed_values
  }
  # apply the preestimation function (if provided)
  if (!is.null(preestimation))    {
    result_data                   <- preestimation(result_data)
    if (!is.data.frame(result_data))
      stop("'preestimation' must return a data frame.",         call. = FALSE)
  }
  # estimate the model (if provided)
  result_model                    <- NULL
  if (!is.null(estimate))         {
    result_model                  <- estimate(formula=formula, data=result_data,
                                              ...)
  }
  # apply the postestimation function (if provided)
  result                          <- list(data=result_data, model=result_model)
  if (!is.null(postestimation))   {
    result                        <- postestimation(result_data,  result_model)
    if (!is.list(result))
      stop("'postestimation' must return a list.",               call. = FALSE)
    if (!all(c("data", "model")   %in% names(result)))
      stop("'postestimation' must return a named list ",
           "containing 'data' and 'model'.",                     call. = FALSE)
    if (!is.data.frame(result$data))
      stop("The 'data' component returned by 'postestimation' ",
           "must be a data frame.",                              call. = FALSE)
  }
  
  # return the result
  result
}
