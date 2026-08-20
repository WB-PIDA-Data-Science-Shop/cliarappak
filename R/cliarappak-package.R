#' @keywords internal
#'
#' @description
#' The original `cliarapp` script app made every one of these packages'
#' exports available unqualified everywhere via `global.R`'s `library(...)`
#' calls -- a script-app pattern that doesn't carry over to a package.
#' `NAMESPACE` needs the equivalent `import()`/`importFrom()` directives or
#' every bare `pickerInput()`, `fluidRow()`, `reactive()`, `tags$...`, etc.
#' across the ported `mod_*.R`/`fct_*.R` files fails the moment it actually
#' executes -- which `devtools::load_all()` alone won't catch, since it only
#' parses and defines functions, never calls them.
#'
#' @import shiny
#' @import golem
#' @import bs4Dash
#' @import shinyWidgets
#' @import shinyjs
#' @import plotly
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @import stringr
#' @import purrr
#' @import readr
#' @import sf
#' @importFrom forcats fct_reorder
#' @importFrom hrbrthemes theme_ipsum
#' @importFrom zoo na.approx
#' @importFrom DT renderDataTable dataTableOutput datatable
#' @importFrom shinycssloaders withSpinner
#' @importFrom colourpicker colourInput
#' @importFrom shinyhelper helper observe_helpers
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner
#' @importFrom waiter waiter_show waiter_hide spin_ring
#' @importFrom haven write_dta
#' @importFrom data.table setnames
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @importFrom officer read_pptx ph_with ph_location fpar ftext fp_text on_slide add_slide
#' @importFrom rvg dml
"_PACKAGE"
