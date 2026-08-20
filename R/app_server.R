app_server <- function(input, output, session) {
  app_data <- golem::get_golem_options("app_data")

  bench <- mod_benchmark_server("benchmark", app_data)
  country_comparison <- mod_country_comparison_server("country", bench, app_data)

  mod_bivariate_server("scatter", bench, app_data, country_comparison)
  mod_world_map_server("world_map", bench, app_data)
  mod_trends_server("trends", bench, app_data)
  mod_data_server("data", bench, app_data)
  # Shares mod_benchmark's own "benchmark" namespace -- its downloadHandlers
  # (report/advreport/pptreport/download_Coverage) render into download
  # buttons defined in mod_benchmark_ui(), not a UI of their own. Calling
  # moduleServer() a second time with the same id is how Shiny supports
  # splitting one module's server logic across multiple functions/files.
  mod_reports_server("benchmark", bench, app_data)
  mod_methodology_server("methodology_ug", app_data)
  mod_publications_server("publications")
}
