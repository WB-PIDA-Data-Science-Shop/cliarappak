#' Classify indicators into strength tiers by within-comparison-group quantile
#'
#' `def_quantiles()` is the core benchmarking calculation behind the Country
#' Benchmarking tab. Given a base country and a set of comparison countries, it
#' ranks every selected indicator *within that comparison group* and labels each
#' country's value as `"Weak"`, `"Emerging"`, or `"Strong"` according to where it
#' falls in the group's distribution.
#'
#' `def_quantiles_dyn()` is the same calculation for the dynamic (year-by-year)
#' dataset: it additionally groups by `year`, so an indicator can be classified
#' differently in different years.
#'
#' @details
#' Steps, in order:
#'
#' 1. **Drop indicators the base country has no data for.** `def_quantiles()`
#'    treats an indicator as missing if the base country has *any* `NA` for it;
#'    `def_quantiles_dyn()` uses the stricter rule of *100%* missing (so an
#'    indicator with at least one observed year survives). Family-average
#'    columns (names ending `_avg`) are never dropped by this step.
#' 2. **Restrict to base + comparison countries and the surviving indicators**,
#'    then pivot long and join `variable_names` for display labels and family.
#' 3. **Compute, per indicator (`def_quantiles_dyn()`: per indicator-year):**
#'    - `dtt` -- `dplyr::percent_rank()` of the value (0-1),
#'    - `q_lv_25` / `q_lv_75` -- the 25th / 75th percentiles (used only for the
#'      low-variance screen below),
#'    - `q_cutoff1` / `q_cutoff2` -- the percentile values at the two `threshold`
#'      cut points,
#'    - `status` -- the `"Weak"` / `"Emerging"` / `"Strong"` label, and
#'    - `nrank` -- `dplyr::min_rank(-value)`, i.e. 1 = highest value in the group.
#'    The original `value` column is renamed `dtf` ("distance to frontier").
#' 4. **Screen out low-variance indicators**: if the base country's 25th and
#'    75th percentiles are equal, the indicator carries no comparative signal
#'    and its rows are removed (again, `_avg` columns are exempt). See
#'    [low_variance()] for the same screen exposed as a standalone list.
#'
#' Both functions return their result **invisibly** (the last statement is an
#' assignment), which is why callers either assign the result or pass it
#' straight into another call such as [family_data()].
#'
#' @param data Long/wide CLIAR indicator table -- `app_data$global_data` for the
#'   static variant, `app_data$global_data_dyn` for the dynamic one. First five
#'   columns are identifiers (`country_name`, `country_code`, region/income
#'   grouping, etc.); the remaining columns are one indicator each.
#' @param base_country Character. One or more country names. The country (or
#'   countries) being benchmarked; drives both the missing-data and
#'   low-variance screens.
#' @param country_list Data frame of country metadata with at least a
#'   `country_name` column. Used to resolve `comparison_countries` to the set of
#'   rows to keep. This is `app_data$country_list`.
#' @param comparison_countries Character vector of country names to benchmark
#'   the base country against.
#' @param vars Character vector of indicator *codes* (the `variable` column, not
#'   the display name) to evaluate.
#' @param variable_names Indicator dictionary: a data frame keyed by `variable`
#'   with `var_name` (display label) and `family_var` / family columns. This is
#'   `app_data$variable_names`.
#' @param threshold One of `"Default"` (cut points at the 25th and 50th
#'   percentiles) or `"Terciles"` (33rd and 66th). Controls both the `status`
#'   labels and `q_cutoff1` / `q_cutoff2`.
#'
#' @return A tibble in long form, one row per country-indicator
#'   (`def_quantiles_dyn()`: per country-indicator-year), with columns:
#'   `country_name`, `variable`, `var_name` (plus the other dictionary columns),
#'   `year` (dynamic only), `dtf` (the value), `dtt` (percentile rank),
#'   `q_lv_25`, `q_lv_75`, `q_cutoff1`, `q_cutoff2`, `status`, `nrank`.
#'   Returned invisibly.
#'
#' @seealso [low_variance()] and [missing_var()] for the screens used here
#'   exposed on their own; [family_data()] and [compute_family_average_app()]
#'   for the family-level roll-up; [static_plot()] / [static_plot_dyn()] which
#'   consume this output.
#' @export
def_quantiles <- function(data, base_country, country_list, comparison_countries, vars, variable_names,threshold) {

# List all relevant countries
  comparison_list <-
    country_list %>%
    filter(country_name %in% comparison_countries)

# List all variables that are missing for the base country -- these will be removed from the data
  na_indicators <-
    data %>%
    ungroup() %>%
    filter(country_name %in% base_country) %>%
    select(-(1:5)) %>%
    summarise(across(everything(), ~ if_else(any(is.na(.)), NA, sum(., na.rm = TRUE)))) %>%
    select(where(is.na)) %>%
    distinct() %>%
    names

# List final relevant variables: those selected, minus those missing
#
if(length(na_indicators) > 0){
  variables <-
    setdiff(vars, na_indicators)

  variables <-
    intersect(variables, names(data))
}else{
  variables <- vars
}


# This is the relevant data to be used
  quantiles <-
    data %>%
    ungroup() %>%
    filter(
      country_name %in% c(base_country, comparison_list$country_name)
    ) %>%
    select(
      country_name,
      any_of(variables)
    )

# Merge with variable dictionary
  quantiles <-
    quantiles %>%

    # Make long per indicator
    pivot_longer(
      cols = any_of(variables),
      names_to = "variable"
    ) %>%

    # Add variables definition and family
    left_join(
      variable_names,
      by = "variable"
    )

if (threshold=="Default"){
    cutoff<-c(25,50)
}else if (threshold=="Terciles")
{
  cutoff<-c(33,66)
}

# Calculate quantiles
  quantiles <-
    quantiles %>%
    # Remove missing values
    filter(!is.na(value)) %>%
    # Calculate relevant indicators
    group_by(variable, var_name) %>%
    mutate(
      dtt = percent_rank(value),
      q_lv_25 = quantile(value,c(0.25)),
      q_lv_75 = quantile(value,c(0.75)),
      q_cutoff1 = quantile(value, c(cutoff[1]/100)),
      q_cutoff2 = quantile(value, c(cutoff[2]/100)),
      status = case_when(
        dtt <= cutoff[1]/100 ~ paste0("Weak\n(bottom ", cutoff[1],"%)"),
        dtt > cutoff[1]/100 & dtt <= cutoff[2]/100 ~ paste0("Emerging\n(",cutoff[1],"% - ",cutoff[2],"%)"),
        dtt > cutoff[2]/100 ~ paste0("Strong\n(top ",100-cutoff[2],"%)")
      ),
      nrank = min_rank(-value)
    ) %>%
    ungroup %>%
    rename(dtf = value)

  # Remove indicators where there is too little variance
  low_variance_indicators <-
    quantiles %>%
    filter(country_name == base_country & q_lv_25==q_lv_75) %>%
    select(variable) %>%
    unlist

  low_variance_indicators <- low_variance_indicators[!grepl("_avg", low_variance_indicators)]


  quantiles <-
    quantiles %>%
    filter(!(variable %in% low_variance_indicators))

}

#' @rdname def_quantiles
#' @export
def_quantiles_dyn <- function(data, base_country, country_list, comparison_countries, vars, variable_names,threshold) {
  # List all relevant countries
  comparison_list <-
    country_list %>%
    filter(country_name %in% comparison_countries)


  # List all variables that are missing for the base country -- these will be removed from the data
  na_indicators_df <-
    data %>%
    ungroup() %>%
    filter(country_name == base_country)

  missing_vars <- sapply(na_indicators_df, function(x) sum(is.na(x)) / length(x))
  na_indicators <- names(missing_vars[missing_vars == 1])

  na_indicators <- na_indicators[!grepl("_avg", na_indicators)]


  # List final relevant variables: those selected, minus those missing
  if(length(na_indicators) != 0){
    variables <-
      setdiff(vars, na_indicators)
    variables <-
      intersect(variables, names(data))
  }else{
    variables <- vars
  }


  # This is the relevant data to be used
  quantiles <-
    data %>%
    ungroup() %>%
    filter(
      country_name %in% c(base_country, comparison_list$country_name)
    ) %>%
    select(
      country_name,
      year,
      any_of(variables)
    )

  quant_vars <- names(quantiles)[names(quantiles) %in% variables]

  # Merge with variable dictionary
  quantiles <-
    quantiles %>%

    # Make long per indicator
    pivot_longer(
      cols = any_of(quant_vars),
      names_to = "variable"
    ) %>%

    # Add variables definition and family
    left_join(
      variable_names,
      by = "variable"
    )

  if (threshold=="Default"){
    cutoff<-c(25,50)
  }else if (threshold=="Terciles")
  {
    cutoff<-c(33,66)
  }

  # Calculate quantiles
  quantiles <-
    quantiles %>%
    # Remove missing values
    filter(!is.na(value)) %>%
    # Calculate relevant indicators
    group_by(variable, var_name, year) %>%
    mutate(
      dtt = percent_rank(value),
      q_lv_25 = quantile(value, c(0.25)),
      q_lv_75 = quantile(value, c(0.75)),
      q_cutoff1 = quantile(value, c(cutoff[1]/100)),
      q_cutoff2 = quantile(value, c(cutoff[2]/100)),
      status = case_when(
        dtt <= cutoff[1]/100 ~ paste0("Weak\n(bottom ", cutoff[1],"%)"),
        dtt > cutoff[1]/100 & dtt <= cutoff[2]/100 ~ paste0("Emerging\n(",cutoff[1],"% - ",cutoff[2],"%)"),
        dtt > cutoff[2]/100 ~ paste0("Strong\n(top ",100-cutoff[2],"%)")
      ),
      nrank = min_rank(-value)
    ) %>%
    ungroup %>%
    rename(dtf = value) %>%
    # Remove indicators where there is too little variance
    mutate(todrop = ifelse(country_name == base_country & q_lv_25==q_lv_75, 1, 0)) %>%
    filter(todrop != 1) %>%
    select(-todrop)


}


#' Indicator codes that are flat within the comparison group
#'
#' `low_variance()` returns the indicator *codes* for which the base country's
#' 25th and 75th percentiles (computed across the base + comparison countries)
#' are identical -- i.e. the indicator has no spread and so carries no
#' benchmarking signal. [def_quantiles()] applies the same screen internally to
#' drop rows; this function exposes the list so callers (e.g.
#' `mod_benchmark_server()`) can also show it in the plot notes.
#'
#' `low_variance_dyn()` is the dynamic-dataset variant: it uses the "100%
#' missing" rule for the missing-data pre-filter (matching [def_quantiles_dyn()])
#' and de-duplicates the result.
#'
#' @details
#' The screen is computed on the long, dictionary-joined table exactly as in
#' [def_quantiles()], using `threshold`-independent quartiles (`q25` / `q75`).
#' Only indicators the base country actually has data for are considered; unlike
#' [def_quantiles()], these functions do **not** exempt `_avg` family-average
#' columns.
#'
#' @inheritParams def_quantiles
#'
#' @return Character vector of indicator codes (`variable` values) with zero
#'   interquartile spread for the base country. `low_variance()` may contain
#'   duplicates; `low_variance_dyn()` is de-duplicated. Returned via an explicit
#'   `return()`.
#'
#' @seealso [def_quantiles()], [missing_var()], [plot_notes_function()].
#' @export
low_variance <- function(data, base_country, country_list, comparison_countries, vars, variable_names) {

  # List all relevant countries
  comparison_list <-
    country_list %>%
    filter(country_name %in% comparison_countries)

  # List all variables that are missing for the base country -- these will be removed from the data
  na_indicators <-
    data %>%
    ungroup() %>%
    filter(country_name %in% base_country) %>%
    select(-(1:5)) %>%
    summarise(across(everything(), ~ if_else(any(is.na(.)), NA, sum(., na.rm = TRUE)))) %>%
    select(where(is.na)) %>%
    distinct() %>%
    names

  # List final relevant variables: those selected, minus those missing
  variables <-
    setdiff(vars, na_indicators)

  variables <-
    intersect(variables, names(data))

  # This is the relevant data to be used
  quantiles <-
    data %>%
    ungroup() %>%

    # Keep only the base and comparison countries
    filter(
      (country_name %in% comparison_list$country_name) | (country_name == base_country)
    ) %>%

    # Keep only selected, non-missing indicators
    select(
      country_name,
      all_of(variables)
    )

  # Merge with variable dictionary
  quantiles <-
    quantiles %>%

    # Make long per indicator
    pivot_longer(
      cols = all_of(variables),
      names_to = "variable"
    ) %>%

    # Add variables definition and family
    left_join(
      variable_names,
      by = "variable"
    )

  # Calculate quantiles
  quantiles <-
    quantiles %>%

    # Remove missing values
    filter(!is.na(value)) %>%

    # Calculate relevant indicators
    group_by(variable, var_name) %>%
    mutate(
      dtt = percent_rank(value),
      q25 = quantile(value, c(0.25)),
      q75 = quantile(value, c(0.75)),
      status = case_when(
        dtt <= .25 ~ "Weak\n(bottom 25%)",
        dtt > .25 & dtt <= .50 ~ "Emerging\n(25% - 50%)",
        dtt > .50 ~ "Strong\n(top 50%)"
      )
    ) %>%
    ungroup %>%
    rename(dtf = value) %>%
    filter(country_name == base_country & q25==q75) %>%
    select(variable) %>%
    unlist

  return(quantiles)

}

#' @rdname low_variance
#' @export
low_variance_dyn <- function(data, base_country, country_list, comparison_countries, vars, variable_names) {

  # List all relevant countries
  comparison_list <-
    country_list %>%
    filter(country_name %in% comparison_countries)


  # List all variables that are missing for the base country -- these will be removed from the data
  # na_indicators <-
  #   data %>%
  #   ungroup() %>%
  #   filter(country_name == base_country) %>%
  #   select(where(is.na)) %>%
  #   names

  na_indicators_df <-
    data %>%
    ungroup() %>%
    filter(country_name == base_country)

  missing_vars <- sapply(na_indicators_df, function(x) sum(is.na(x)) / length(x))
  na_indicators <- names(missing_vars[missing_vars == 1])


  # List final relevant variables: those selected, minus those missing
  # variables <-
  #   setdiff(vars, na_indicators)
  #
  # variables <-
  #   intersect(variables, names(data))

  if(length(na_indicators) != 0){
    variables <-
      setdiff(vars, na_indicators)
    variables <-
      intersect(variables, names(data))
  }else{
    variables <- vars
  }

  # This is the relevant data to be used
  quantiles <-
    data %>%
    ungroup() %>%

    # Keep only the base and comparison countries
    filter(
      (country_name %in% comparison_list$country_name) | (country_name == base_country)
    ) %>%

    # Keep only selected, non-missing indicators
    select(
      country_name,
      all_of(variables)
    )

  # Merge with variable dictionary
  quantiles <-
    quantiles %>%

    # Make long per indicator
    pivot_longer(
      cols = all_of(variables),
      names_to = "variable"
    ) %>%

    # Add variables definition and family
    left_join(
      variable_names,
      by = "variable"
    )

  # Calculate quantiles
  quantiles <-
    quantiles %>%

    # Remove missing values
    filter(!is.na(value)) %>%

    # Calculate relevant indicators
    group_by(variable, var_name) %>%
    mutate(
      dtt = percent_rank(value),
      q25 = quantile(value, c(0.25)),
      q75 = quantile(value, c(0.75)),
      status = case_when(
        dtt <= .25 ~ "Weak\n(bottom 25%)",
        dtt > .25 & dtt <= .50 ~ "Emerging\n(25% - 50%)",
        dtt > .50 ~ "Strong\n(top 50%)"
      )
    ) %>%
    ungroup %>%
    rename(dtf = value) %>%
    filter(country_name == base_country & q25==q75) %>%
    distinct(variable) %>%
    unlist

  return(quantiles)

}



#' Display names of indicators the base country is missing
#'
#' `missing_var()` returns the human-readable names (`var_name`) of the
#' indicators in `vars` that the base country has no data for, so the Country
#' Benchmarking tab can list them in the plot notes ("the following indicators
#' are not considered because the base country has no information...").
#'
#' `missing_var_dyn()` is the dynamic-dataset variant: an indicator counts as
#' missing only when it is *100%* missing for the base country, and the result
#' is de-duplicated.
#'
#' @details
#' Where [low_variance()] returns indicator *codes*, these functions return
#' *display names*, resolved through `variable_names`. The two lists are
#' concatenated by the caller before being passed to [plot_notes_function()].
#' Both functions return their value invisibly.
#'
#' @inheritParams def_quantiles
#'
#' @return Character vector of indicator display names (`var_name`) that are
#'   missing for the base country. `missing_var_dyn()` is de-duplicated.
#'   Returned invisibly.
#'
#' @seealso [low_variance()], [def_quantiles()], [plot_notes_function()].
#' @export
missing_var <- function(data, base_country, country_list, comparison_countries, vars, variable_names) {

  # List all relevant countries
  comparison_list <-
    country_list %>%
    filter(country_name %in% comparison_countries)

  # List all variables that are missing for the base country -- these will be removed from the data
  na_indicators <-
    data %>%
    ungroup() %>%
    filter(country_name %in% base_country) %>%
    select(-(1:5)) %>%
    summarise(across(everything(), ~ if_else(any(is.na(.)), NA, sum(., na.rm = TRUE)))) %>%
    select(where(is.na)) %>%
    distinct() %>%
    names

  # List final relevant variables: those selected, minus those missing
  variables <-
    setdiff(vars, na_indicators)

  variables <-
    intersect(variables, names(data))

  # List specific family variables missing
  missing_variables <-
    vars[vars %in% na_indicators] %>%
    data.frame() %>%
    rename("variable"=".") %>%
    left_join(variable_names %>% select(variable,var_name), by = "variable") %>%
    .$var_name

}


#' @rdname missing_var
#' @export
missing_var_dyn <- function(data, base_country, country_list, comparison_countries, vars, variable_names) {

  # List all relevant countries
  comparison_list <-
    country_list %>%
    filter(country_name %in% comparison_countries)

  # List all variables that are missing for the base country -- these will be removed from the data
  # na_indicators <-
  #   data %>%
  #   ungroup() %>%
  #   filter(country_name == base_country) %>%
  #   select(where(is.na)) %>%
  #   names

  na_indicators_df <-
    data %>%
    ungroup() %>%
    filter(country_name == base_country)

  missing_vars <- sapply(na_indicators_df, function(x) sum(is.na(x)) / length(x))
  na_indicators <- names(missing_vars[missing_vars == 1])

  # List final relevant variables: those selected, minus those missing
  # variables <-
  #   setdiff(vars, na_indicators)
  #
  # variables <-
  #   intersect(variables, names(data))

  if(length(na_indicators) != 0){
    variables <-
      setdiff(vars, na_indicators)
    variables <-
      intersect(variables, names(data))
  }else{
    variables <- vars
  }

  # List specific family variables missing
  missing_variables <-
    vars[vars %in% na_indicators] %>%
    data.frame() %>%
    rename("variable"=".") %>%
    left_join(variable_names %>% select(variable,var_name), by = "variable") %>%
    .$var_name

  missing_variables <- unique(missing_variables)

}
