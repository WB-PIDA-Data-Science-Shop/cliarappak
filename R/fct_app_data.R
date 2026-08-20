#' Build the CLIAR app's shared data objects
#'
#' Ports the "Auxiliary functions" / "Data" / "Options" sections of the
#' original `cliarapp`'s `global.R` (and the `vars-control.R` file it
#' sourced) into one function, computed once and returned as a named list.
#'
#' In the original script app these were implicit globals available to every
#' tab because everything ran in one `global.R` + flat `server()` scope. Per
#' the migration's Tier-1 rule, `cliarappak` never relies on that: `app_ui()`
#' and `app_server()` each call this once and pass the pieces each module
#' needs as explicit arguments.
#'
#' Two intentional deviations from a literal line-by-line port, both because
#' the original behavior was found to be dead code, not a design to preserve:
#' - `global.R:38-45` joined `cliaretl::family_order` onto `db_variables`, but
#'   `vars-control.R` (sourced immediately after) reassigns `db_variables <-
#'   cliaretl::db_variables_final` before anything reads the joined columns,
#'   so the join was never observable. Not reproduced here. `family_order`
#'   itself is still kept in the returned list, unjoined -- `server.R:1358`
#'   (the benchmark "Overview" plot) uses it directly via its own
#'   `left_join()`/`arrange()`, independent of the dead `db_variables` join.
#' - `vars-control.R` computed 14 per-family `vars_*` vectors
#'   (`vars_anticorruption`, `vars_mkt`, ...) and several `vars_*_ctf`
#'   benchmarking vectors. Grepping `server.R`/`ui.R` for each name found zero
#'   references outside the file that defined them — dead. Only `vars_all`
#'   and `vars_family`, which genuinely are used downstream, are kept.
#'
#' The original app also bundled a static `coverage_ctf_for_analysis.csv`
#' (→ `year_ctf_dynamic`), used only by the Coverage Report. That file was
#' never sourced from `cliaretl` at runtime -- it was the *output* of a
#' one-off script (`cliarapp/source/data_coverage_processing.R`) that
#' someone had to remember to re-run and re-copy in by hand each data-release
#' cycle. It's not shipped here at all; see [prepare_app_data_coverage()],
#' which computes the same thing live and is called lazily from
#' `mod_reports.R`'s Coverage Report download handler (not from here) since
#' it's only needed for that one report, not every session.
#'
#' @return A named list of data objects shared across `cliarappak`'s modules.
#' @export
build_app_data <- function() {
  db_variables_base <- cliaretl::db_variables_final

  vars_all <- db_variables_base %>%
    dplyr::filter(var_level == "indicator") %>%
    dplyr::pull(variable)

  vars_family <- db_variables_base %>%
    dplyr::filter(family_var != "vars_other") %>%
    dplyr::pull(family_var) %>%
    unique()

  # definitions is built from the pre-filter (broader) db_variables, matching
  # global.R's original execution order (this runs before the vars_all filter
  # below in the source script).
  definitions <-
    db_variables_base %>%
    dplyr::filter(var_level == "indicator") %>%
    dplyr::mutate(
      family_name = dplyr::if_else(
        is.na(family_name) | family_name == "",
        "(other indicators)",
        family_name
      )
    ) %>%
    dplyr::select(
      Family = family_name,
      Indicator = var_name,
      Description = description,
      Source = source
    ) %>%
    dplyr::group_by(Family) %>%
    tidyr::nest(definitions = c(Indicator, Description, Source))

  raw_data <-
    fs::path_package("extdata", "compiled_indicators.rds", package = "cliaretl") %>%
    readr::read_rds() %>%
    dplyr::filter(year >= 1990) %>%
    dplyr::rename(Year = year) %>%
    dplyr::mutate(Year = as.double(Year))

  global_data <-
    cliaretl::closeness_to_frontier_static %>%
    dplyr::ungroup()

  ctf_long <-
    cliaretl::closeness_to_frontier_static %>%
    tidyr::pivot_longer(
      cols = -c(country_code, country_name, income_group, region),
      names_to = "variable",
      values_to = "value"
    )

  global_data_dyn <-
    cliaretl::closeness_to_frontier_dynamic %>%
    dplyr::filter(year <= 2024) %>%
    dplyr::ungroup()

  ctf_long_dyn <-
    cliaretl::closeness_to_frontier_dynamic %>%
    tidyr::pivot_longer(
      cols = -c(country_code, country_name, income_group, region, year),
      names_to = "variable",
      values_to = "value"
    )

  country_groups <- cliaretl::wb_country_groups

  family_order <- cliaretl::family_order

  country_list <- cliaretl::wb_country_list

  spatial_data <-
    fs::path_package("extdata", "indicators_map.rds", package = "cliaretl") %>%
    readr::read_rds()

  # Order datasets by country name for consistency
  country_list <- country_list[order(country_list$country_name, decreasing = FALSE), ]
  ctf_long <- ctf_long[order(ctf_long$country_name, decreasing = FALSE), ]
  ctf_long_dyn <- ctf_long_dyn[order(ctf_long_dyn$country_name, decreasing = FALSE), ]
  raw_data <- raw_data[order(raw_data$country_name, decreasing = FALSE), ]
  global_data <- global_data[order(global_data$country_name, decreasing = FALSE), ]
  global_data_dyn <- global_data_dyn[order(global_data_dyn$country_name, decreasing = FALSE), ]
  spatial_data <- spatial_data[order(spatial_data$country_name, decreasing = FALSE), ]

  sf::st_crs(spatial_data) <- "+proj=robin"

  # Load data control -- the FINAL db_variables used by the rest of the app
  db_variables <-
    db_variables_base %>%
    dplyr::filter(variable %in% vars_all | var_level == "family")

  # Add label attributes to columns for DTA export
  for (i in colnames(global_data)[4:length(colnames(global_data))]) {
    name <- subset(db_variables$var_name, db_variables$variable == i)
    if (length(name) > 0) {
      name <- stringr::str_replace_all(name, "[[:punct:]]", "")
      attr(global_data[[i]], "label") <- name
    }
  }
  for (i in colnames(global_data_dyn)[5:length(colnames(global_data_dyn))]) {
    name <- subset(db_variables$var_name, db_variables$variable == i)
    if (length(name) > 0) {
      name <- stringr::str_replace_all(name, "[[:punct:]]", "")
      attr(global_data_dyn[[i]], "label") <- name
    }
  }
  for (i in colnames(raw_data)[4:length(colnames(raw_data))]) {
    name <- subset(db_variables$var_name, db_variables$variable == i)
    if (length(name) > 0) {
      name <- stringr::str_replace_all(name, "[[:punct:]]", "")
      attr(raw_data[[i]], "label") <- name
    }
  }

  family_names <- db_variables %>%
    dplyr::select(variable = family_var, var_name = family_name) %>%
    dplyr::distinct() %>%
    dplyr::filter(variable != "vars_other")

  variable_names <-
    db_variables %>%
    dplyr::select(variable, var_level, var_name, family_var, family_name) %>%
    dplyr::filter(family_var != "vars_other")

  countries <-
    global_data %>%
    dplyr::select(country_name) %>%
    dplyr::filter(!(country_name %in% country_groups$group_name)) %>%
    unlist() %>%
    unname() %>%
    unique() %>%
    sort()

  country_get_palestine <- c("West Bank and Gaza" = "PS")

  flags_with_countries <- mapply(
    function(country, code) {
      flag_html <- htmltools::tags$img(
        src = paste0("https://flagcdn.com/w20/", tolower(code), ".png"),
        alt = code
      )
      label_html <- htmltools::tags$span(country)
      paste(flag_html, label_html, sep = " ")
    },
    countries,
    ifelse(
      countries == "West Bank and Gaza",
      country_get_palestine["West Bank and Gaza"],
      countrycode::countrycode(countries, "country.name.en", "ecb")
    ),
    SIMPLIFY = FALSE
  )

  variable_list <- lapply(
    family_names$var_name,
    extract_variables,
    db_variables = db_variables
  )
  names(variable_list) <- family_names$var_name

  variable_list_benchmarked <- lapply(
    family_names$var_name,
    extract_variables_benchmarked,
    db_variables = db_variables
  )
  names(variable_list_benchmarked) <- family_names$var_name

  # Exclude Monetary Stability - #296
  variable_list_benchmarked$`Public Finance Institutions` <-
    variable_list_benchmarked$`Public Finance Institutions`[
      variable_list_benchmarked$`Public Finance Institutions` != "Monetary stability"
    ]

  filtered_variable_list <- lapply(variable_list, remove_average_items)

  group_list <- list(
    Economic = country_groups %>% dplyr::filter(group_category == "Economic") %>% dplyr::pull(group_name),
    Region = country_groups %>% dplyr::filter(group_category == "Region") %>% dplyr::pull(group_name),
    Income = country_groups %>% dplyr::filter(group_category == "Income") %>% dplyr::pull(group_name)
  )

  all_groups <- group_list %>% unlist() %>% unname()

  y_scatter_choices <- append("Log GDP per capita, PPP", variable_list)

  list(
    db_variables = db_variables,
    vars_all = vars_all,
    vars_family = vars_family,
    definitions = definitions,
    raw_data = raw_data,
    global_data = global_data,
    ctf_long = ctf_long,
    global_data_dyn = global_data_dyn,
    ctf_long_dyn = ctf_long_dyn,
    country_groups = country_groups,
    family_order = family_order,
    country_list = country_list,
    spatial_data = spatial_data,
    family_names = family_names,
    variable_names = variable_names,
    countries = countries,
    flags_with_countries = flags_with_countries,
    variable_list = variable_list,
    variable_list_benchmarked = variable_list_benchmarked,
    filtered_variable_list = filtered_variable_list,
    group_list = group_list,
    all_groups = all_groups,
    y_scatter_choices = y_scatter_choices,
    plot_height = 500
  )
}

#' Build the coverage-analysis long dataset for the Coverage Report
#'
#' Ports `cliarapp/source/data_coverage_processing.R` -- previously a
#' one-off script, run manually each data-release cycle, that pivoted
#' `compiled_indicators.rds` to long format and joined in indicator metadata
#' to produce a static `coverage_ctf_for_analysis.csv` someone then had to
#' remember to copy into the app's data folder. Computing it live here means
#' it can never go stale. The historical "static 2020 to 2024 period" window
#' (the original script's own comment reads "2025 release, should take the
#' static 2020 to 2024 period") is now derived from `db_variables`'s own
#' `ref_year` attribute rather than a number a person had to remember to
#' bump by hand -- `ref_year = 2025` reproduces `2020:2024` exactly.
#'
#' @param raw_data Compiled indicators data, same shape as
#'   [build_app_data()]'s `raw_data` element (has a `Year` column, not
#'   `year`).
#' @param db_variables Indicator metadata -- pass `cliaretl::db_variables`
#'   (NOT `app_data$db_variables`, which [build_app_data()] pre-filters to
#'   benchmarked/family-level variables; this needs the full table so every
#'   indicator's `source`/`family_name`/`benchmarked_ctf` shows up in the
#'   coverage report, not just the subset the interactive dashboard
#'   benchmarks).
#' @param year_window Integer vector of years to include. Defaults to the 5
#'   years ending the year before `db_variables`'s `ref_year` attribute.
#'
#' @return A long-format tibble: one row per country x indicator x year,
#'   with `indicator_value`, and `var_name`/`source`/`family_name`/
#'   `benchmarked_ctf` metadata joined in.
#' @export
prepare_app_data_coverage <- function(raw_data, db_variables, year_window = NULL) {
  if (is.null(year_window)) {
    ref_year <- attr(db_variables, "ref_year")
    year_window <- (ref_year - 5):(ref_year - 1)
  }

  raw_data %>%
    dplyr::filter(Year %in% year_window) %>%
    tidyr::pivot_longer(
      cols = -c(country_code, income_group, region, country_name, Year),
      names_to = "indicators",
      values_to = "indicator_value"
    ) %>%
    dplyr::rename(year = Year) %>%
    dplyr::left_join(
      db_variables %>% dplyr::select(variable, var_name, source, family_name, benchmarked_ctf),
      by = c("indicators" = "variable")
    )
}
