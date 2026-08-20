#' buttons
#'
#' @description Generates app buttons
#'
#' @param id Input id
#' @param lab Button label
#' @param id icon. Defaults to None
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

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


#' Construct user app data directory path
#'
#' @description
#' Consists of three pieces of information:
#'
#' - System location
#' - name
#'
#' @importFrom rappdirs user_data_dir
#'
#' @return Character. Path to user's app data directory
user_data_dir <- function() {
  
  dir <- rappdirs::user_data_dir(
    appname = "CLIAR"
  )
  
  return(dir)
  
}


#' check_input_file_exists
#'
#' @description
#' Check if the input file exists in the user_data_dir() directory

#' @import fs
#'
#' @return Character. Path to user's app data directory
check_input_file_exists <- function(){
  fs::file_exists(fs::path(user_data_dir(), "cliar_inputs.rds"))
}


#' toast_messages_func
#'
#' @description Displays toast messages
#' @param type success or errors
#' @param text the message to be displayed
#'
#' @return a toast message informing the end user of an action that has just been carried out
#' @export
#'
#' @noRd
#'
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

#' modal_function
#'
#' @description Displays modal messages
#' @param title title of the message
#' @param text the message to be displayed
#'
#' @return a modal message 
#' @export
#'
#' @noRd
#'
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
#' specifically `bs4ValueBox`, `bs4InfoBox` and `bs4Card`.
#'
#' @export
#'
#' @param ... Not used.
#'
#' @importFrom htmltools findDependencies attachDependencies
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

#======= Custom Item: this function prepares customItems
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
#' This function establishes x_axis variable choices
#'
#' @param yvar Selected y-axis variable name to exclude from x-axis choices.
#' @param db_variables Indicator metadata table (replaces the implicit global
#'   used by the original `cliarapp` version).
#' @param family_names Data frame of family variable/name pairs (replaces the
#'   implicit global used by the original `cliarapp` version).
#'
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