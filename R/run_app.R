
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
#' Any arguments passed to `run_app(...)` beyond `onStart`, `options`,
#' `enableBookmarking`, and `uiPattern` are collected into `...` and threaded
#' into `golem_opts` this way.
#'
#' `golem_opts$app_data` in particular is how `app_ui()`/`app_server()` reach
#' the shared data objects built by [build_app_data()] -- it's built here
#' once per app start (not per session, and not per implicit global the way
#' the original `cliarapp` script did it) and passed through
#' [golem::get_golem_options()] to every module. Pass `app_data` explicitly
#' yourself only if you need to override it (e.g. tests injecting fixtures);
#' otherwise it's built automatically.
#'
#' @param ... arguments to pass to golem_opts.
#' @param onStart,options,enableBookmarking,uiPattern See `?shiny::shinyApp`.
#' @export
run_app <- function(onStart = NULL,
                    options = list(),
                    enableBookmarking = NULL,
                    uiPattern = "/",
                    ...) {

  golem_opts <- list(...)
  if (is.null(golem_opts$app_data)) {
    golem_opts$app_data <- build_app_data()
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