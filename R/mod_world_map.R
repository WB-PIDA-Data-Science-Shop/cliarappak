#' World Map module UI
#'
#' The World Map tab: pick an indicator and a value type (latest value vs
#' closeness-to-frontier), and see a choropleth of every country with data.
#' Controls are a selection `box()`; the map itself is a `plotlyOutput()`.
#'
#' @param id Character. The module id; must match [mod_world_map_server()].
#' @param app_data Shared data list from [build_app_data()]. Uses
#'   `$variable_list` for the indicator picker.
#'
#' @return A `shiny::tagList` of UI elements.
#'
#' @seealso [mod_world_map_server()], [static_map()].
#' @export
mod_world_map_ui <- function(id, app_data) {
  ns <- NS(id)

  tagList(
    box(
      width = 12,
      solidHeader = TRUE,
      title = "Select information to display",
      status = "success",
      collapsible = TRUE,

      fluidRow(
        column(
          width = 5,
          pickerInput(
            ns("vars_map"),
            label = "Select indicator",
            choices = app_data$variable_list,
            selected = NULL,
            options = list(
              `live-search` = TRUE,
              title = "Click to select family or indicator"
            ),
            width = "100%"
          )
        ),
        column(
          width = 3,
          radioGroupButtons(
            ns("countries_map"),
            label = "Select countries to display",
            choices = c(
              "All" = FALSE,
              "Base + comparison countries" = TRUE
            ),
            justified = TRUE,
            selected = FALSE,
            checkIcon = list(
              yes = icon("ok", lib = "glyphicon"))
          )
        ),
        column(
          width = 4,
          radioGroupButtons(
            ns("value_map"),
            label = "Select data source",
            choices = c(
              "Closeness to frontier" = "ctf",
              "Original indicator" = "raw"
            ),
            justified = TRUE,
            selected = "ctf",
            checkIcon = list(
              yes = icon("ok", lib = "glyphicon"))
          )
        )
      )
    ),

    conditionalPanel(
      "input.vars_map !== ''",
      ns = ns,

      bs4Card(
        width = 12,
        solidHeader = FALSE,
        gradientColor = "primary",
        collapsible = FALSE,

        plotlyOutput(
          ns("map"),
          height = paste0(app_data$plot_height, "px")
        ) %>% shinycssloaders::withSpinner(color = "#051f3f", type = 8)
      )
    )
  )
}

#' World Map module server
#'
#' Renders the choropleth (`output$map`) via [static_map()] piped into
#' [interactive_map()], and disables the "closeness to frontier" value option
#' when a family-average indicator is selected (there is no CTF for those).
#' Shows a "Map is not available" message via [check_spatial_data()] when the
#' indicator has no data anywhere.
#'
#' @details
#' **Cross-module contract.** Reads two elements of `bench` (the list from
#' [mod_benchmark_server()]): `bench$base_country()` and `bench$countries()`,
#' both used only to outline those countries on the map. Exposes nothing back.
#'
#' @param id Character. The module id; must match [mod_world_map_ui()].
#' @param bench Named list of reactives from [mod_benchmark_server()] -- uses
#'   `$base_country()` and `$countries()`.
#' @param app_data Shared data list from [build_app_data()]. Uses
#'   `$spatial_data`, `$db_variables`, `$variable_names`.
#'
#' @return `NULL`, invisibly. Called for its side effects (registers the
#'   `value_map` observer and the `map` plotly output on `session`).
#'
#' @seealso [mod_world_map_ui()], [static_map()], [check_spatial_data()].
#' @export
mod_world_map_server <- function(id, bench, app_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # shinyjs::disable/enable auto-namespace their `id` argument inside a
    # module, but NOT their `selector` argument -- has to be built manually.
    observeEvent(input$vars_map, {
      if (grepl("Average", input$vars_map)) {
        shinyjs::disable(selector = paste0("#", ns("value_map"), " button:eq(1)"))
      } else {
        shinyjs::enable(selector = paste0("#", ns("value_map"), " button:eq(1)"))
      }
    })

    output$map <-
      renderPlotly({
        validate(need(check_spatial_data(app_data$spatial_data, input$vars_map, app_data$db_variables) == FALSE, 'Map is not available for this Indicator for the selected base country'))

        if (input$vars_map != "") {
          var_selected <-
            app_data$variable_names %>%
            filter(var_name == input$vars_map) %>%
            pull(variable)

          static_map(
            input$value_map,
            var_selected,
            input$vars_map,
            input$countries_map,
            bench$base_country(),
            bench$countries(),
            app_data$spatial_data
          ) %>%
            interactive_map(
              var_selected,
              app_data$db_variables,
              plotly_remove_buttons,
              input$value_map
            )
        }
      })
  })
}
