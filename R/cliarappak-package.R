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
#' `shiny`, `bs4Dash`, `shinyWidgets`, `shinyjs`, and `plotly` all export
#' overlapping function names (e.g. `column`, `actionButton`, `last_plot`),
#' which under a blanket `@import` produces "replacing previous import"
#' warnings at load time and leaves the winner decided by NAMESPACE's
#' alphabetical directive order rather than by any actual intent. These five
#' are therefore `@importFrom`'d per function actually used (audited against
#' the full `R/` source, 2026-08-25) instead of `@import`'d whole -- this also
#' means only the currently-used name wins any name clash, matching what was
#' already verified working in Phase 4/6, and any future addition of a new
#' *unqualified* call to one of these packages fails loudly and immediately
#' (`could not find function`) instead of silently resolving to whichever
#' package happens to sort last. Packages below that don't collide with
#' anything today (`golem`, `dplyr`, `ggplot2`, `tidyr`, `stringr`, `purrr`,
#' `readr`, `sf`) are left as `@import` -- converting those too would be pure
#' churn with no collision to fix.
#'
#' @import golem
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @import stringr
#' @import purrr
#' @import readr
#' @import sf
#' @importFrom shiny a bindCache bindEvent br code column conditionalPanel div
#'   downloadButton downloadHandler enableBookmarking eventReactive fluidRow
#'   h3 HTML htmlOutput icon img includeCSS isolate modalDialog moduleServer
#'   need NS numericInput observe observeEvent p reactive removeModal
#'   renderTable renderUI req shinyApp showNotification span strong
#'   tableOutput tagList tags uiOutput validate
#' @importFrom bs4Dash box bs4Card dashboardBody dashboardBrand
#'   dashboardHeader dashboardPage dashboardSidebar menuItem sidebarMenu
#'   tabItem tabItems
#' @importFrom shinyWidgets checkboxGroupButtons pickerInput prettyCheckbox
#'   radioGroupButtons updateCheckboxGroupButtons updatePickerInput
#' @importFrom shinyjs toggleState useShinyjs
#' @importFrom plotly config ggplotly layout plotlyOutput renderPlotly style
#' @importFrom stats median na.omit quantile setNames
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
