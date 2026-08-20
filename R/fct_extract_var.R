#' Indicator variable names for a given family
#'
#' @param x Family name to filter on.
#' @param db_variables Indicator metadata table (replaces the implicit global
#'   used by the original `cliarapp` version).
#'
#' @export
extract_variables <-
  function(x, db_variables) {
    db_variables %>%
      filter(
        family_name == x
      ) %>%
      pull(var_name)
  }
#=====
#' Benchmarked indicator variable names for a given family
#'
#' @param x Family name to filter on.
#' @param db_variables Indicator metadata table (replaces the implicit global
#'   used by the original `cliarapp` version).
#'
#' @export
extract_variables_benchmarked <-
  function(x, db_variables) {
    db_variables %>%
      filter(
        family_name == x, benchmarked_ctf=='Yes'
      ) %>%
      pull(var_name)
  }
