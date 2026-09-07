#' Does a column have zero interquartile spread?
#'
#' Helper for the low-variance screen used across the benchmarking calculations:
#' returns `TRUE` when a numeric column's 25th and 75th percentiles are both
#' non-missing and equal, i.e. at least half the values are identical and the
#' column carries no comparative signal.
#'
#' @param column A numeric vector.
#'
#' @return A single logical. `TRUE` if `quantile(column, .25) == quantile(column,
#'   .75)` (both non-`NA`), otherwise `FALSE`.
#'
#' @seealso [family_data()], [compute_family_average_app()], [low_variance()].
#' @export
check_quantiles <- function(column) {
  q25 <- quantile(column, 0.25, na.rm = TRUE)
  q75 <- quantile(column, 0.75, na.rm = TRUE)
  if (!is.na(q25) & !is.na(q75) & q25 == q75) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Family-level closeness-to-frontier scores for the benchmarking plots
#'
#' `family_data()` collapses the indicator-level closeness-to-frontier table to
#' one score per institutional family (the simple mean of the family's
#' indicators), for the static (single latest period) dataset. `family_data_dyn()`
#' does the same per year for the dynamic dataset. Both feed the "Overview" view
#' of the Country Benchmarking tab.
#'
#' @details
#' Before averaging, indicators are dropped if:
#'
#' - the base country has no data for them (`family_data()`: *any* `NA`;
#'   `family_data_dyn()`: *100%* `NA`), or
#' - `family_data()` only: they are flat across the base + comparison countries
#'   ([check_quantiles()] returns `TRUE`).
#'
#' Family-average columns (names containing `_avg`) and the `country_code`
#' column are also removed. The remaining indicators are pivoted long, joined to
#' `variable_names` for their `family_var`, averaged within
#' `country_name` (and `year`, for `_dyn`), and pivoted back wide with one column
#' per family. `NaN` means (a family with no observed indicators) become `NA`.
#'
#' @param data Wide closeness-to-frontier table -- `app_data$global_data` for
#'   `family_data()`, `app_data$global_data_dyn` for `family_data_dyn()`. First
#'   few columns are identifiers; the rest are one indicator each.
#' @param base_country Character vector of base-country name(s); drives the
#'   missing-data screen.
#' @param variable_names Indicator dictionary (`app_data$variable_names`) keyed
#'   by `variable`, supplying `family_var`.
#' @param comparison_countries Character vector of comparison-country names
#'   (`family_data()` only) -- used together with `base_country` for the
#'   low-variance screen.
#'
#' @return A wide tibble: one row per `country_name` (per `country_name` and
#'   `year` for `family_data_dyn()`), one column per institutional family, values being
#'   the mean closeness-to-frontier of that family's indicators.
#'
#' @seealso [compute_family_average_app()] for the `_avg` columns used
#'   elsewhere; [def_quantiles()]; [static_plot()].
#' @export
family_data <- function(data, base_country, variable_names,comparison_countries) {

  na_indicators <-
    data %>%
    ungroup() %>%
    filter(country_name %in% base_country) %>%
    select(-(1:5)) %>%
    summarise(across(everything(), ~ if_else(any(is.na(.)), NA, sum(., na.rm = TRUE)))) %>%
    select(where(is.na)) %>%
    distinct() %>%
    names

  lv_data<-data%>%
    filter(country_name %in% c(base_country,comparison_countries))%>%
    select(-(1:5))

  result <- sapply(lv_data, check_quantiles)

  lv_indicators <- names(result[result == TRUE])

  data <-
    data %>%
    select(-c(union(na_indicators, lv_indicators),country_code)) %>%
    ungroup %>%
    select(country_name, everything())


  dtf_family_level <-
    data %>%
    pivot_longer(cols = 4:ncol(.),
                 names_to = "variable") %>%
    left_join(variable_names,
              by = "variable") %>%
    filter(!variable %in% grep("_avg", variable, value = T)) %>%
    group_by(country_name, family_var) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>%
    mutate(
      value = ifelse(is.nan(value),NA,value)
    ) %>%
    pivot_wider(names_from = family_var)

  return(dtf_family_level)
}


#' @rdname family_data
#' @export
family_data_dyn <- function(data, base_country, variable_names) {

  na_indicators_df <-
    data %>%
    ungroup() %>%
    filter(country_name == base_country)

  missing_vars <- sapply(na_indicators_df, function(x) sum(is.na(x)) / length(x))
  na_indicators <- names(missing_vars[missing_vars == 1])

  data <-
    data %>%
    select(-c(na_indicators, country_code)) %>%
    ungroup %>%
    select(country_name, everything())

  dtf_family_level <-
    data %>%
    pivot_longer(cols = 5:ncol(.),
      names_to = "variable") %>%
    left_join(variable_names,
      by = c("variable")) %>%
    filter(!variable %in% grep("_avg", variable, value = T)) %>%
    group_by(country_name, family_var, year) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>%
    mutate(
      value = ifelse(is.nan(value),NA,value)
    ) %>%
    pivot_wider(names_from = family_var)

 return(dtf_family_level)

}

#' Family-average closeness-to-frontier columns (`*_avg`)
#'
#' Produces the per-family average closeness-to-frontier columns (named
#' `<family>_avg`) that the benchmarking plots draw as the family-level markers.
#' This is a thin app-side wrapper: it applies the app's missing-data and
#' low-variance pre-filtering, then hands off to
#' [cliaretl::compute_family_average()] for the actual aggregation, so the
#' dashboard and the `cliaretl` pipeline stay in lock-step on how families are
#' averaged.
#'
#' @details
#' Pre-filtering removes indicators the base country has no data for (any `NA`)
#' and indicators that are flat across the base + comparison countries
#' ([check_quantiles()]), then the same names are dropped from `vars`. The
#' delegated call passes `require_complete = FALSE` and `exclude_pattern =
#' "gdp"`. The `type` argument selects a static (one period) or dynamic
#' (per-year) aggregation and is forwarded unchanged.
#'
#' @param cliar_data Wide closeness-to-frontier table -- `app_data$global_data`
#'   (static) or `app_data$global_data_dyn` (dynamic).
#' @param vars Character vector of indicator codes to aggregate.
#' @param type One of `"static"` or `"dynamic"`.
#' @param db_variables Indicator dictionary (`app_data$db_variables`).
#' @param base_country Character vector of base-country name(s); drives the
#'   missing-data screen.
#' @param comparison_countries Character vector of comparison-country names;
#'   used with `base_country` for the low-variance screen.
#'
#' @return A wide tibble of `<family>_avg` columns keyed by `country_code`
#'   (and `year`, when `type = "dynamic"`), as returned by
#'   [cliaretl::compute_family_average()].
#'
#' @seealso [family_data()], [def_quantiles()],
#'   [cliaretl::compute_family_average()].
#' @export
compute_family_average_app <- function(cliar_data, vars, type = "static", db_variables,
                                        base_country, comparison_countries) {
  na_indicators <- cliar_data |>
    dplyr::ungroup() |>
    dplyr::filter(country_name %in% base_country) |>
    dplyr::select(-(1:5)) |>
    dplyr::summarise(dplyr::across(dplyr::everything(),
      ~ if_else(any(is.na(.)), NA, sum(., na.rm = TRUE)))) |>
    dplyr::select(where(is.na)) |>
    dplyr::distinct() |>
    names()

  lv_data <- cliar_data |>
    dplyr::filter(country_name %in% c(base_country, comparison_countries)) |>
    dplyr::select(-(1:5))
  lv_indicators <- names(Filter(check_quantiles, lv_data))

  cliar_data <- cliar_data |>
    dplyr::select(-c(union(na_indicators, lv_indicators))) |>
    dplyr::ungroup() |>
    dplyr::select(country_name, dplyr::everything())
  vars <- setdiff(vars, union(na_indicators, lv_indicators))

  cliaretl::compute_family_average(
    cliar_data, vars, type = type, db_variables = db_variables,
    require_complete = FALSE, exclude_pattern = "gdp"
  )
}
