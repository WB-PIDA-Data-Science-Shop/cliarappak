#' Drop family-average entries from an indicator list
#'
#' Removes any element whose name contains the word `"Average"` from a
#' character vector of indicator names. Used when building the Time Trends /
#' World Map indicator pickers, where the family-average aggregates that appear
#' in the benchmarking tabs are not meaningful.
#'
#' @param family Character vector of indicator (or family) names, possibly
#'   including entries like `"Political institutions - Average"`.
#'
#' @return The input vector with the `"Average"` entries removed. Names and
#'   order of the remaining elements are preserved.
#'
#' @seealso [build_app_data()], which applies this to `variable_list` to build
#'   `filtered_variable_list`.
#' @export
remove_average_items <- function(family) {
  family_filtered <- family[!grepl("Average", family)]
  return(family_filtered)
}
