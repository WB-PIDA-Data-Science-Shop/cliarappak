#' Build a "Save / Load Selection" action button
#'
#' Small UI helper: wraps a [shinyWidgets::actionBttn()] (jelly style, primary,
#' small, upload icon) in a `div(class = "load_save_btns")`. Used for the
#' save-selection / load-selection buttons on the Country Benchmarking tab.
#'
#' @param id Character. The `inputId` for the button (already namespaced by the
#'   calling module).
#' @param lab Character. The button label.
#'
#' @return A [shiny::div()] containing the button.
#'
#' @seealso [mod_benchmark_ui()].
#' @export
buttons_func <- function(id, lab) {

  div(class = "load_save_btns",
    shinyWidgets::actionBttn(
      inputId = id,
      label = lab,
      icon = shiny::icon("upload"),
      style = "jelly",
      color = "primary",
      size = "sm"
    )
  )

}


#' Path to the CLIAR user data directory
#'
#' The per-user, per-OS directory where the app persists a saved selection
#' (`cliar_inputs.rds`). A thin wrapper around
#' [rappdirs::user_data_dir()] with `appname = "CLIAR"`.
#'
#' @return Character scalar. The absolute path to the user's app data
#'   directory (not guaranteed to exist yet).
#'
#' @seealso [check_input_file_exists()].
#'
#' @importFrom rappdirs user_data_dir
#' @export
user_data_dir <- function() {

  dir <- rappdirs::user_data_dir(
    appname = "CLIAR"
  )

  return(dir)

}


#' Does the user have a saved CLIAR selection on disk?
#'
#' Checks for `cliar_inputs.rds` in [user_data_dir()] -- the file written when a
#' user clicks "Save Selection of Countries".
#'
#' @return A single logical.
#'
#' @seealso [user_data_dir()].
#'
#' @importFrom fs file_exists path
#' @export
check_input_file_exists <- function(){
  fs::file_exists(fs::path(user_data_dir(), "cliar_inputs.rds"))
}


#' Show a bottom-right toast notification
#'
#' Thin wrapper around [shinyFeedback::showToast()] with the app's standard
#' options (duplicates prevented, bottom-right position).
#'
#' @param type Character. Toast type -- `"success"`, `"error"`, `"warning"` or
#'   `"info"`.
#' @param text Character. The message to display.
#'
#' @return Called for its side effect (displays the toast); returns the value
#'   of [shinyFeedback::showToast()] invisibly.
#'
#' @seealso [modal_function()].
#' @export
toast_messages_func <- function(type, text) {
  shinyFeedback::showToast(
    type = type,
    message = text,
    .options = list(
      preventDuplicates = TRUE,
      positionClass = "toast-bottom-right"
    )
  )
}

#' Show a simple dismissable modal
#'
#' Thin wrapper around [shiny::showModal()] / [shiny::modalDialog()] for a
#' one-message modal with a single "Dismiss" button.
#'
#' @param title Character. The modal title.
#' @param mes The modal body -- a string or any Shiny tag(s).
#'
#' @return Called for its side effect (displays the modal); returns `NULL`
#'   invisibly.
#'
#' @seealso [toast_messages_func()].
#' @export
modal_function <- function(title, mes){
  shiny::showModal(shiny::modalDialog(
    title = title,
    mes,
    footer = shiny::modalButton("Dismiss"),
  ))
}

#' Use 'bs4Dash' in 'shiny'
#'
#' Allow to use functions from 'bs4Dash' into a classic 'shiny' app,
#' specifically `bs4ValueBox`, `bs4InfoBox` and `bs4Card`. Attaches the
#' `bs4Dash` HTML dependencies to an empty `div` so they load even when the
#' page is not a `bs4Dash::bs4DashPage()`.
#'
#' @param ... Not used.
#'
#' @return A `<div>` with the `bs4Dash` HTML dependencies attached.
#'
#' @importFrom htmltools findDependencies attachDependencies
#' @export
useBs4Dash <- function(...) {
  if (!requireNamespace(package = "bs4Dash"))
    message("Package 'bs4Dash' is required to run this function")
  deps <- findDependencies(bs4Dash::bs4DashPage(
    header = bs4Dash::bs4DashNavbar(),
    sidebar = bs4Dash::bs4DashSidebar(),
    body = bs4Dash::bs4DashBody()
  ))
  attachDependencies(tags$div(), value = deps)
}


#Plotting Prep Functions:

#' Build a sidebar `customItem` link
#'
#' Constructs a bs4Dash-style sidebar `<li>` containing an external link. Only
#' renders when `href` is non-`NULL` (a `NULL` `href` returns `NULL`).
#'
#' @param text Character. The visible link text.
#' @param icon A [shiny::icon()] to show before the text. Defaults to a warning
#'   triangle.
#' @param href Character URL. The link target (opened in a new tab). If `NULL`,
#'   nothing is rendered.
#' @param ... Currently ignored.
#'
#' @return A `<li class="nav-item">` tag, or `NULL` when `href` is `NULL`.
#'
#' @seealso `app_ui()`, which assembles the sidebar.
#' @export
customItem <-
  function(text,
           icon = shiny::icon("warning"),
           href = NULL, ...) {

    if (is.null(href))

      tags$li(
        a(href = href, icon, text, class = "nav-link", target = "_blank"),
        class = "nav-item"
      )
  }

#=========== Bivariate Correlation Functions

#' X-axis indicator choices for the Bivariate Correlation tab
#'
#' Builds the grouped choice list for the scatter plot's x-axis picker: every
#' indicator, organised by institutional family, with the currently selected
#' y-axis indicator removed so a user cannot plot an indicator against itself.
#' `"Log GDP per capita, PPP"` is prepended as an ungrouped first choice.
#'
#' @details
#' Replaces the `cliarapp` script-app version that read implicit `db_variables`
#' and `family_names` globals; both are now passed in.
#'
#' @param yvar Character. The selected y-axis indicator display name, excluded
#'   from every family's list.
#' @param db_variables Indicator metadata table (`app_data$db_variables`).
#' @param family_names Data frame of family `variable` / `var_name` pairs
#'   (`app_data$family_names`).
#'
#' @return A named list suitable for `shinyWidgets::pickerInput(choices = )`:
#'   one element per family (a character vector of indicator names), plus a
#'   leading `"Log GDP per capita, PPP"` entry.
#'
#' @seealso [static_scatter()], [mod_bivariate_ui()].
#' @export
x_scatter_choices <- function(yvar, db_variables, family_names){

  extract_xvar_choices <-
    function(x, yvar) {
      db_variables %>%
        dplyr::filter(
          var_name != yvar
        ) %>%
        dplyr::filter(
          family_name == x
        ) %>%
        pull(var_name)
    }

  xvar_choice_list <- purrr::map2(family_names$var_name, yvar, extract_xvar_choices)
  names(xvar_choice_list) <- family_names$var_name

  xvar_choice_list <- c("Log GDP per capita, PPP",xvar_choice_list)

  return(xvar_choice_list)
}
