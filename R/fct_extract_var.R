#' Indicator display names for a given institutional family
#'
#' `extract_variables()` returns every indicator's display name (`var_name`) in
#' one institutional family; `extract_variables_benchmarked()` returns only the
#' subset flagged `benchmarked_ctf == "Yes"`. Both are used by
#' [build_app_data()] to build the per-family choice lists behind the tab
#' pickers.
#'
#' @details
#' These replace the `cliarapp` script-app versions that read an implicit global
#' `db_variables`; here the metadata table is passed in explicitly.
#'
#' @param x Character. The family name to filter on (matched against
#'   `db_variables$family_name`).
#' @param db_variables Indicator metadata table -- `app_data$db_variables`, or
#'   `cliaretl::db_variables_final`.
#'
#' @return A character vector of indicator display names (`var_name`) in family
#'   `x`. Empty if the family has no matching rows.
#'
#' @seealso [build_app_data()], [x_scatter_choices()].
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
#' @rdname extract_variables
#' @export
extract_variables_benchmarked <-
  function(x, db_variables) {
    db_variables %>%
      filter(
        family_name == x, benchmarked_ctf=='Yes'
      ) %>%
      pull(var_name)
  }
