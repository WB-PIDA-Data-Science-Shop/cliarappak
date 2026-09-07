#' Make a data frame's column names unique
#'
#' Applies [make.unique()] to `names(data)` and puts them back, with a
#' dimensionality sanity check. Used by the download-prep functions before they
#' hand data to writers that require unique column names.
#'
#' @param data A data frame.
#'
#' @return `data`, unchanged except that duplicate column names have been made
#'   unique (`"x"`, `"x.1"`, ...).
#'
#' @section Errors:
#' Stops if `make.unique()` returns a different number of names than `data` has
#' columns (should never happen; guards against an upstream malformed input).
#'
#' @seealso [dta_prep()], [rds_prep()], [csv_prep()].
#' @export
make_colnames_unique <- function(data) {
  #unique columns on original data
  CN<- make.unique(names(data))

  #Dimensionality Check
  if (length(CN) == ncol(data)) {
    names(data) <- CN
  } else {
    stop("ERROR: Dimensionality Mismatch in initial column uniqueness check. Check Original Dataset.")
  }

  return(data)
}

#' Prepare the download table for a given file format
#'
#' The Data tab lets a user download the closeness-to-frontier table as Stata
#' (`dta_prep()`), RDS (`rds_prep()`) or CSV (`csv_prep()`). Each function takes
#' the shared in-memory table and returns a copy with column names cleaned to
#' what that format needs.
#'
#' @details
#' All three: take a `data.table::copy()` first (\pkg{data.table}'s
#' `setnames()` mutates by reference, so without the copy the shared
#' `pre_download_data()` reactive would be corrupted for the next format the
#' user downloads in the same session); optionally swap indicator codes for
#' display names; de-duplicate; then replace every character that is not
#' `[A-Za-z0-9_]` with `_`.
#'
#' They differ only in how far names are truncated:
#'
#' \describe{
#'   \item{`dta_prep()`}{display names truncated to 32 chars on rename, column
#'     names truncated to 30 before `make.unique()` (Stata's variable-name
#'     limit, with headroom for the `make.unique()` suffix).}
#'   \item{`rds_prep()`}{display names truncated to 32 chars on rename; no
#'     further truncation.}
#'   \item{`csv_prep()`}{no truncation at all -- full display names kept.}
#' }
#'
#' @param data The wide closeness-to-frontier table to export.
#' @param des_names Logical. `TRUE` renames indicator-code columns to their
#'   human-readable `var_name`s; `FALSE` (the `dta_prep()` default) keeps the
#'   codes.
#' @param db_variables Indicator dictionary (`app_data$db_variables`) providing
#'   the `variable` -> `var_name` mapping used when `des_names = TRUE`.
#'
#' @return A `data.table` copy of `data` with cleaned (and, for `dta_prep()`,
#'   truncated) unique column names.
#'
#' @seealso [make_colnames_unique()], [mod_data_server()].
#' @export
dta_prep <- function(data, des_names=FALSE, db_variables) {
# data.table::setnames() below mutates by reference, even on a plain
# data.frame -- without this copy, calling *_prep() would silently corrupt
# the shared `pre_download_data()` reactive's cached value for the next
# format a user downloads in the same session (e.g. rds then csv then dta).
data <- data.table::copy(data)
#Handles descriptive data names switch
if (des_names==TRUE){
  db_data <- data %>%
    # Rename columns to ones in db_variables
    setnames(
      old = as.character(db_variables$variable),
      new = substr(as.character(db_variables$var_name), 1, 32),
      skip_absent = TRUE
    )}
else{db_data<-data}

#Duplication in original data check
 if (any(duplicated(names(db_data)))){
   prepared_dta_data<- make_colnames_unique(db_data)
 }
  else{prepared_dta_data <- db_data}

  #Old data names before preparation
  column_names <- names(prepared_dta_data)

  #We are shortening to 30 characters because of the added characters in uniqueness code below
  truncated_names <- substr(column_names, 1, 30)

  # Make sure names are unique (This function uses illegal "." characters)
  unique_names <- make.unique(truncated_names)

  # Replace invalid characters for .dta files
  cleaned_dta_names <- str_replace_all(unique_names, "[^A-Za-z0-9_]", "_")


  # Rename columns in the final data
  setnames(prepared_dta_data, old = column_names, new = cleaned_dta_names)

  return(prepared_dta_data)
}


#' @rdname dta_prep
#' @export
rds_prep <- function(data, des_names, db_variables) {
  # See dta_prep() for why this copy is required -- setnames() mutates by reference.
  data <- data.table::copy(data)
  #Handles descriptive data names switch
  if (des_names==TRUE){
    Rdb_data <- data %>%
      # Rename columns to ones in db_variables
      setnames(
        old = as.character(db_variables$variable),
        new = substr(as.character(db_variables$var_name), 1, 32),
        skip_absent = TRUE
      )}
  else{Rdb_data<-data}

  #Duplication in original data check
  if (any(duplicated(names(Rdb_data)))){
    prepared_rds_data<- make_colnames_unique(Rdb_data)
  }
  else{prepared_rds_data <- Rdb_data}

  # Set Column Names for cleaning
  column_names <- names(prepared_rds_data)

  # Make sure names are unique
  unique_names <- make.unique(column_names)

  # Replace invalid characters
  cleaned_rds_names <- str_replace_all(unique_names, "[^A-Za-z0-9_]", "_")


  # Rename columns in the data
  setnames(prepared_rds_data, old = column_names, new = cleaned_rds_names)

  # Return the prepared data

  return(prepared_rds_data)
}

#' @rdname dta_prep
#' @export
csv_prep<- function(data,des_names, db_variables){
  # See dta_prep() for why this copy is required -- setnames() mutates by reference.
  data <- data.table::copy(data)
  if (des_names==TRUE){
    XL_data <- data %>%
    # Rename columns to ones in db_variables
    setnames(
      old = as.character(db_variables$variable),
      new = as.character(db_variables$var_name),
      skip_absent = TRUE)}
  else{
    XL_data<-data}
    # Set Column Names for cleaning
  column_names <- names(XL_data)

    # Make sure names are unique
  unique_names <- make.unique(column_names)

    # Replace invalid characters
  cleaned_XL_names <- str_replace_all(unique_names, "[^A-Za-z0-9_]", "_")


    # Rename columns in the data
  setnames(XL_data, old = column_names, new = cleaned_XL_names)

    # Return the prepared data

  return(XL_data)
  }

#' Is closeness-to-frontier data missing for a country / indicator pair?
#'
#' Guard used by the Cross-Country Comparison, Time Trends and Bivariate
#' Correlation tabs to decide whether to draw a plot or show a "not available"
#' message.
#'
#' @details
#' With `indicator_2 = NULL` (single-indicator mode, used by the bar chart and
#' time-series tabs) it returns whether the base country's value for
#' `indicator_1` is `NA`. With `indicator_2` supplied (bivariate mode) it
#' returns `TRUE` if *either* indicator is missing for the country, or if either
#' indicator column is absent from `data` altogether.
#'
#' @param data Wide closeness-to-frontier table (`app_data$global_data`).
#' @param country Character. Base-country name.
#' @param indicator_1 Character. Indicator display name (`var_name`).
#' @param indicator_2 Character. Optional second indicator display name for
#'   bivariate mode; `NULL` for single-indicator mode.
#' @param db_variables Indicator dictionary (`app_data$db_variables`).
#'
#' @return A logical. `TRUE` means data is missing (don't plot). In
#'   single-indicator mode a length-1 logical; the return is used as a scalar
#'   condition.
#'
#' @seealso [trends_check_data()], [check_spatial_data()].
#' @export
check_data <-function(data,country,indicator_1, indicator_2=NULL, db_variables){

  #One variable scenario (used in bar and time trend)
  if (is.null(indicator_2)){
    #Establishes the variable (column) searched for
    var <-
      db_variables %>%
      filter(var_name == indicator_1) %>%
      pull(variable)

    #Establishes the base country (row) that is in use
    indicator_val <-
      data %>%
      filter(country_name == country) %>%
      pull(var)

    #Returns a boolean on whether or not the column is null for the selected indicator1
    return(is.na(indicator_val))}

  #Two indicator scenario used in bivariate correlation
  else{

    # Extracts the columns for the given indicators
    vars <- db_variables %>%
      filter(var_name %in% c(indicator_1, indicator_2)) %>%
      pull(variable)

    # Checks that both indicators are in data
    if (all(vars %in% names(data))) {
      has_na <- data %>%
        filter(country_name == country) %>%
        select(all_of(vars)) %>%
        summarise(across(everything(), ~ any(is.na(.)), .names = "has_na_{col}")) %>%
        summarise(across(starts_with("has_na_"), any)) %>%
        unlist() %>%
        any()

      return(has_na)
    } else {
      # If any of the variables do not exist in dv_variables, this indicates missing data
      return(TRUE)
  }
}}

#' Does a country have gaps in an indicator over a year range?
#'
#' The Time Trends tab uses this to narrow the list of comparison countries to
#' those with a complete series for the chosen indicator over the base
#' country's observed year span.
#'
#' @param start_year,end_year Numeric. Inclusive bounds of the year range to
#'   check.
#' @param country Character. The comparison-country name to check.
#' @param var Character. Indicator *code* (column name in `raw_data`).
#' @param raw_data Wide indicator panel with `country_name` / `Year` columns
#'   (`app_data$raw_data`).
#'
#' @return A single logical: `TRUE` if any year in the range has an `NA` for
#'   `var` (exclude this country), `FALSE` if the series is complete.
#'
#' @seealso [check_data()], [trends_plot()], [mod_trends_server()].
#' @export
trends_check_data <- function(start_year, end_year, country, var, raw_data) {

  # Filter the data down to start and end years and current comparison country
    comp_data <- raw_data %>%
    select(country_name, Year, !!sym(var)) %>%  # Select only necessary columns
    filter(Year >= start_year & Year <= end_year, country_name == country)

  # Check for nulls in the input variable column
  any_nulls <- comp_data %>%
    summarise(has_nulls = any(is.na(!!sym(var)))) %>%
    pull(has_nulls)

  return(any_nulls)  # Return TRUE if any nulls are found, FALSE otherwise
}

#' Is an indicator entirely missing from the map layer?
#'
#' Guard for the World Map tab: returns whether the chosen indicator has no
#' non-missing values anywhere in the spatial layer.
#'
#' @param data The spatial (`sf`) layer (`app_data$spatial_data`), which carries
#'   one `value_<code>` column per indicator.
#' @param indicator Character. Indicator display name (`var_name`).
#' @param db_variables Indicator dictionary (`app_data$db_variables`).
#'
#' @return A single logical: `TRUE` if every value of `value_<code>` is `NA`.
#'
#' @seealso [check_data()], [static_map()], [mod_world_map_server()].
#' @export
check_spatial_data <-function(data,indicator,db_variables){

  var <-
    db_variables %>%
    filter(var_name == indicator) %>%
    pull(variable)

  indicator_val <-
    data %>%
    pull(paste0("value_",var))

  return(all(is.na(indicator_val)))
}
