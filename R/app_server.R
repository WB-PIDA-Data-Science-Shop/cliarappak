#' Top-level app server
#'
#' Wires the module servers together. Retrieves the shared data list via
#' `golem::get_golem_options("app_data")`, starts [mod_benchmark_server()] first
#' (it owns the shared selection state, returned as `bench`), then passes
#' `bench` -- and, for the scatter tab, `country_comparison` -- into every other
#' module server.
#'
#' @details
#' `observe_helpers()` (shinyhelper) is called here at the root session, not in
#' a module, because its click JS signals the unnamespaced root session. The
#' Reports server is deliberately mounted on the `"benchmark"` id so its
#' download handlers render into buttons defined by [mod_benchmark_ui()].
#'
#' @param input,output,session Standard Shiny server arguments.
#'
#' @return `NULL`, invisibly.
#'
#' @seealso `app_ui()`, [run_app()], [mod_benchmark_server()].
#' @noRd
app_server <- function(input, output, session) {
  app_data <- golem::get_golem_options("app_data")

  # shinyhelper's click-handling JS always signals the app's ROOT session
  # (Shiny.onInputChange("shinyhelper-modal_params", ...) is not namespaced),
  # so observe_helpers() must be called here, not inside any moduleServer --
  # called from within a module it listens on that module's child session,
  # which never receives the unnamespaced event, and every "?" icon in the
  # app silently does nothing.
  observe_helpers()

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
