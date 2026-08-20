#' world map module UI
#'
#' @param id a unique identifier for this module.
#' @param app_data Shared data list from [build_app_data()].
#'
#' @return a `tagList` of UI elements
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

#' world map module server
#'
#' @param id a unique identifier for this module.
#' @param bench Named list of reactives returned by [mod_benchmark_server()].
#' @param app_data Shared data list from [build_app_data()].
#'
#' @return NULL; called for its side effects.
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
