#' bivariate correlation module UI
#'
#' @param id a unique identifier for this module.
#' @param app_data Shared data list from [build_app_data()].
#'
#' @return a `tagList` of UI elements
#' @export
mod_bivariate_ui <- function(id, app_data) {
  ns <- NS(id)

  tagList(
    bs4Card(
      title = "Select indicators to visualize",
      status = "success",
      solidHeader = TRUE,
      width = 12,

      fluidRow(
        column(
          width = 3,
          pickerInput(
            ns("country_scatter"),
            label = "Select a base country",
            choices = c("", app_data$countries),
            selected = NULL,
            multiple = FALSE,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE
            )
          )
        ),
        column(
          width = 3,
          pickerInput(
            ns("y_scatter"),
            label = "Select indicator for Y axis",
            choices = app_data$y_scatter_choices,
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
          pickerInput(
            ns("x_scatter"),
            label = "Select indicator for X axis",
            choices = NULL,
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
          pickerInput(
            ns("high_group"),
            label = "Highlight a group",
            choices = append("No highlight", app_data$group_list),
            selected = NULL,
            multiple = FALSE,
            options = list(
              `live-search` = TRUE,
              `actions-box` = TRUE
            ),
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          shinyWidgets::materialSwitch(
            inputId = ns("linear_fit"),
            label = "Show linear fit line",
            value = FALSE,
            status = "success"
          )
        ),
        column(width = 6),
        column(
          width = 2.4,
          shinyjs::hidden(downloadButton(
            ns("download_bivariate_data"),
            "Download Chart Data",
            style = "width:100%; background-color: #204d74; color: white"
          ))
        ),
      )
    ),

    bs4Card(
      title = "Select individual comparison countries",
      width = 12,
      status = "success",
      collapsed = TRUE,

      checkboxGroupButtons(
        inputId = ns("countries_scatter"),
        individual = TRUE,
        label = NULL,
        choices = app_data$countries,
        checkIcon = list(
          yes = icon("ok", lib = "glyphicon")
        )
      )
    ),
    bs4Card(
      title = "Select Bar Graph Colors",
      status = "success",
      collapsed = TRUE,
      width = 12,

      fluidRow(
        column(
          width = 4,
          colourInput(
            ns("color_base_scatter"),
            "Choose a base country color:",
            value = "#f29411"
          )
        ),
        column(
          width = 4,
          colourInput(
            ns("color_comp_scatter"),
            "Choose a comparison country color:",
            value = "#080770")
        )
      )),

    conditionalPanel(
      'input.y_scatter !== ""',
      ns = ns,

      bs4Card(
        width = 12,
        solidHeader = FALSE,
        gradientColor = "primary",
        collapsible = FALSE,

        plotlyOutput(
          ns("scatter_plot"),
          height = paste0(app_data$plot_height * 1.6, "px")
        )
      )
    )
  )
}

#' bivariate correlation module server
#'
#' @param id a unique identifier for this module.
#' @param bench Named list of reactives returned by [mod_benchmark_server()].
#' @param app_data Shared data list from [build_app_data()].
#' @param country_comparison Named list returned by
#'   [mod_country_comparison_server()] -- `high_group()` below reads its
#'   `custom_df_bar()`, a genuine cross-module dependency found in
#'   `server.R:1971-1979` (not just shared `bench` state).
#'
#' @return NULL; called for its side effects.
#' @export
mod_bivariate_server <- function(id, bench, app_data, country_comparison) {
  moduleServer(id, function(input, output, session) {

    # Cross-tab sync -- see mod_country_comparison_server() for why this is
    # split into a live (bench$country()) and an Apply-gated (bench$select_trigger())
    # observer, mirroring server.R's two distinct original sync mechanisms.
    observeEvent(bench$country(), {
      updatePickerInput(session, "country_scatter", selected = bench$country())
    }, ignoreNULL = FALSE)

    observeEvent(bench$select_trigger(), {
      updatePickerInput(session, "country_scatter", selected = bench$base_country())
      updateCheckboxGroupButtons(session, "countries_scatter", selected = bench$countries())
    }, ignoreNULL = TRUE)

    observeEvent(bench$custom_grps_df(), {
      updateCheckboxGroupButtons(session, "countries_scatter", selected = bench$countries())
    }, ignoreNULL = TRUE)

    high_group <- reactive({
      high_group_df <- app_data$country_list %>%
        filter(group %in% input$high_group) %>%
        select(group, country_name)

      if (!is.null(country_comparison$custom_df_bar()) & any(input$high_group %in% country_comparison$custom_df_bar()$Grp)) {
        custom_df_data <- country_comparison$custom_df_bar() %>%
          filter(Grp %in% input$high_group) %>%
          select(Grp, Countries) %>%
          rename(group = Grp, country_name = Countries) %>%
          left_join(., app_data$country_list %>% select(country_name), by = c("country_name"))

        high_group_df <- bind_rows(high_group_df, custom_df_data)
      }

      return(high_group_df)
    })

    filtered_countries_scatter <- reactive({
      req(input$y_scatter, input$x_scatter)
      app_data$countries %>%
        .[!sapply(., function(country) check_data(app_data$global_data, country, input$y_scatter, input$x_scatter, db_variables = app_data$db_variables))]
    })

    observeEvent(
      list(input$country_scatter, input$x_scatter),
      {
        available_countries_scatter <- na.omit(filtered_countries_scatter())
        updateCheckboxGroupButtons(
          session,
          "countries_scatter",
          choices = available_countries_scatter,
          checkIcon = list(
            yes = icon("ok", lib = "glyphicon", style = "color: #00000")
          ),
          selected = intersect(input$countries_scatter, available_countries_scatter)
        )
      })

    output$scatter_plot <-
      renderPlotly({
        shiny::req(input$y_scatter)
        shiny::req(input$x_scatter)

        validate(need(check_data(app_data$global_data, input$country_scatter, input$y_scatter, input$x_scatter, db_variables = app_data$db_variables) == FALSE,
                      'Country Comparison is not available for this Indicator for the selected base country'))

        static_scatter(
          app_data$global_data,
          input$country_scatter,
          input$countries_scatter,
          high_group(),
          input$y_scatter,
          input$x_scatter,
          app_data$variable_names,
          app_data$country_list,
          input$linear_fit,
          input$color_base_scatter,
          input$color_comp_scatter
        )$sc_plot %>%
          interactive_scatter(
            input$y_scatter,
            input$x_scatter,
            app_data$db_variables,
            high_group(),
            plotly_remove_buttons
          )
      })

    # server.R:2173-2184 -- physically located between the Trends and Data
    # Download sections, but logically part of this tab (populates x_scatter
    # from y_scatter). The original guide's line-range map missed this block.
    shiny::observeEvent(input$y_scatter, {
      shiny::req(input$y_scatter)
      updatePickerInput(
        session,
        inputId = "x_scatter",
        choices = x_scatter_choices(input$y_scatter, app_data$db_variables, app_data$family_names)
      )
    })

    # Downloadable csv of Bivariate dataset
    observe({
      inputs_not_blank <- input$country_scatter != "" &&
        input$y_scatter != "" &&
        input$x_scatter != ""

      condition <- inputs_not_blank &&
        check_data(app_data$global_data, input$country_scatter, input$y_scatter, input$x_scatter, db_variables = app_data$db_variables) == FALSE

      if (condition) {
        shinyjs::show("download_bivariate_data")
      } else {
        shinyjs::hide("download_bivariate_data")
      }
    })

    output$download_bivariate_data <-
      downloadHandler(
        filename = function() {
          paste0("CLIAR Bivariate Analysis-", input$country_scatter, " - data.csv")
        },
        content = function(file) {
          show_modal_spinner(color = "#17a2b8", text = "Loading Data")
          on.exit(remove_modal_spinner())

          write_csv(
            static_scatter(
              app_data$global_data,
              input$country_scatter,
              input$countries_scatter,
              high_group(),
              input$y_scatter,
              input$x_scatter,
              app_data$variable_names,
              app_data$country_list,
              input$linear_fit,
              input$color_base_scatter,
              input$color_comp_scatter
            )$sc_data,
            file,
            na = "")
        }
      )
  })
}
