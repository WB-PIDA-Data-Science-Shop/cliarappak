
#' Run the CLIAR Shiny Application
#'
#' @details
#' `run_app()` wraps its call to [shiny::shinyApp()] in [golem::with_golem_options()].
#' This solves a plumbing problem: `shiny::shinyApp()` doesn't have a way for
#' `run_app()` to pass arbitrary custom parameters through to `app_server()`,
#' because `app_server` has to keep the exact signature Shiny expects --
#' `function(input, output, session)`. There's no room in that signature for
#' extra arguments.
#'
#' What `with_golem_options()` actually does: it takes the `shinyApp()` object
#' and a list of arbitrary values (`golem_opts`), and stashes that list
#' somewhere Shiny's internals can retrieve later -- via
#' [golem::get_golem_options()], callable from inside `app_server()` or any
#' module server, even though it was never passed as a formal argument. It's
#' essentially a side-channel for configuration that bypasses the
#' fixed-signature constraint.
#'
#' Any arguments passed to `run_app(...)` beyond the named ones below are
#' collected into `...` and threaded into `golem_opts` this way.
#'
#' `golem_opts$app_data` in particular is how `app_ui()`/`app_server()` reach
#' the shared data objects built by [build_app_data()] -- it's built here
#' once per app start (not per session, and not per implicit global the way
#' the original `cliarapp` script did it) and passed through
#' [golem::get_golem_options()] to every module. Pass `app_data` explicitly
#' yourself only if you need to override it (e.g. tests injecting fixtures);
#' otherwise it's built automatically, using `dynamic_year_cutoff` below.
#'
#' @param dynamic_year_cutoff Integer year, or `NULL` (default) to derive one
#'   from `cliaretl`'s current data vintage -- see [resolve_dynamic_year_cutoff()].
#'   Controls how far into the dynamic-benchmarking / Time Trends data
#'   `build_app_data()` reaches; pass an explicit year to preview a different
#'   cutoff without waiting on a new `cliaretl` release. A future `deploy_app()`
#'   will accept this same argument to bake a chosen cutoff into a deployment,
#'   which is why it's resolved once, explicitly, rather than left implicit.
#' @param ... further arguments to pass to golem_opts.
#' @param onStart,options,enableBookmarking,uiPattern See `?shiny::shinyApp`.
#' @export
run_app <- function(onStart = NULL,
                    options = list(),
                    enableBookmarking = NULL,
                    uiPattern = "/",
                    dynamic_year_cutoff = NULL,
                    ...) {

  golem_opts <- list(...)
  if (is.null(golem_opts$app_data)) {
    golem_opts$app_data <- build_app_data(dynamic_year_cutoff = dynamic_year_cutoff)
  }

  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = golem_opts
  )

}
