#' time trends module UI
#'
#' @param id a unique identifier for this module.
#' @param app_data Shared data list from [build_app_data()].
#'
#' @return a `tagList` of UI elements
#' @export
mod_trends_ui <- function(id, app_data) {
  ns <- NS(id)

  tagList(
    box(
      width = 12,
      solidHeader = TRUE,
      title = "Select indicator to visualize",
      status = "success",
      collapsible = TRUE,

      fluidRow(
        column(
          width = 3,
          pickerInput(
            ns("country_trends"),
            label = "Select a base country",
            choices = c("", app_data$countries),
            selected = NULL,
            multiple = FALSE,
            options = list(
              `live-search` = TRUE
            )
          )
        ),
        column(
          width = 4,
          pickerInput(
            ns("group_trends"),
            label = "Select comparison groups",
            choices = app_data$group_list,
            selected = NULL,
            multiple = TRUE,
            options = list(
              "max-options" = 5,
              `live-search` = TRUE
            )
          )
        ),
        column(
          width = 5,
          pickerInput(
            ns("vars_trends"),
            label = "Select indicator to visualize",
            choices = app_data$filtered_variable_list,
            selected = NULL,
            options = list(
              `live-search` = TRUE,
              title = "Click to select family or indicator"
            ),
            width = "100%"
          )
        )
      )
    ),

    bs4Card(
      title = "Select individual comparison countries",
      width = 12,
      status = "success",
      collapsed = TRUE,

      checkboxGroupButtons(
        inputId = ns("countries_trends"),
        individual = TRUE,
        label = NULL,
        choices = app_data$countries,
        checkIcon = list(
          yes = icon("ok", lib = "glyphicon")
        )
      )
    ),
    # Color Select BS4 Card
    bs4Card(
      title = "Select Time Trend Colors",
      status = "success",
      collapsed = TRUE,
      width = 12,

      fluidRow(
        column(
          width = 4,
          colourInput(
            ns("color_base_trends"),
            "Choose a base country color:",
            value = "#f29411"
          )
        ),
        column(
          width = 4,
          colourInput(
            ns("color_comp_trends"),
            "Choose a comparison country color:",
            value = "#080770"
          )
        ),
        column(
          width = 4,
          colourInput(
            ns("color_groups_trends"),
            "Choose a comparison group color:",
            value = "#808080"))
      )
    ),

    #==============================
    conditionalPanel(
      'input.vars_trends !== null && input.country_trends != ""',
      ns = ns,

      bs4Card(
        width = 12,
        solidHeader = FALSE,
        gradientColor = "primary",
        collapsible = FALSE,

        plotlyOutput(
          ns("time_series"),
          height = paste0(app_data$plot_height * 1.6, "px")
        )
      )
    )
  )
}

#' time trends module server
#'
#' @param id a unique identifier for this module.
#' @param bench Named list of reactives returned by [mod_benchmark_server()].
#' @param app_data Shared data list from [build_app_data()].
#'
#' @return NULL; called for its side effects.
#' @export
mod_trends_server <- function(id, bench, app_data) {
  moduleServer(id, function(input, output, session) {

    # Cross-tab sync -- see mod_country_comparison_server() for the
    # live-vs-Apply-gated split this mirrors.
    observeEvent(bench$country(), {
      updatePickerInput(session, "country_trends", selected = bench$country())
    }, ignoreNULL = FALSE)

    observeEvent(bench$select_trigger(), {
      updatePickerInput(session, "country_trends", selected = bench$base_country())
      if (!is.null(bench$groups())) {
        updatePickerInput(session, "group_trends", selected = bench$groups())
      }
    }, ignoreNULL = TRUE)

    observeEvent(bench$custom_grps_df(), {
      cdf <- bench$custom_grps_df()
      if (!is.null(cdf)) {
        Custom <- list(unique(cdf$Grp))
        if (length(unique(cdf$Grp)) == 1) {
          names(Custom) <- unique(cdf$Grp)
        } else {
          names(Custom) <- "Custom"
        }
        updatePickerInput(
          session, "group_trends",
          choices = as.list(append(app_data$group_list, Custom)),
          selected = unique(c(bench$groups(), unique(cdf$Grp)))
        )
      } else {
        updatePickerInput(session, "group_trends", choices = app_data$group_list, selected = bench$groups())
      }
    }, ignoreNULL = TRUE)

    custom_df_trend <- reactive({
      if (any(!input$group_trends %in% unlist(app_data$group_list))) {
        custom_df_trend <- bench$custom_grps_df()[bench$custom_grps_df()$Grp %in% input$group_trends, ]
      } else {
        custom_df_trend <- NULL
      }
      return(custom_df_trend)
    })

    #=== REACTIVE Comparison Country MENU ITEMS-Trends
    filtered_countries_trends <- reactive({
      req(input$vars_trends)
      req(input$country_trends)

      fullvar <- app_data$db_variables %>%
        filter(var_name == input$vars_trends) %>%
        select(variable) %>%
        pull()

      filter_years <-
        app_data$raw_data %>%
        filter(
          country_name == input$country_trends,
          !is.na(get(fullvar))
        ) %>%
        summarise(
          min = min(Year, na.rm = TRUE),
          max = max(Year, na.rm = TRUE)
        )

      trends_start <- filter_years %>% pull(min)
      trends_end <- filter_years %>% pull(max)

      app_data$countries %>%
        .[!sapply(., function(country) trends_check_data(trends_start, trends_end, country, fullvar, app_data$raw_data))]
    })

    observeEvent(
      list(input$country_trends, input$vars_trends),
      {
        available_countries_trends <- na.omit(filtered_countries_trends())
        updateCheckboxGroupButtons(
          session,
          "countries_trends",
          choices = available_countries_trends,
          checkIcon = list(
            yes = icon("ok", lib = "glyphicon", style = "color: #00000")
          ),
          selected = intersect(input$countries_trends, available_countries_trends)
        )
      })

    output$time_series <-
      renderPlotly({
        shiny::req(input$country_trends)
        shiny::req(input$vars_trends)
        validate(need(check_data(app_data$raw_data, input$country_trends, input$vars_trends, db_variables = app_data$db_variables) == FALSE, 'Country Comparison is not available for this Indicator for the selected base country'))

        if (input$vars_trends != "") {
          var <-
            app_data$db_variables %>%
            filter(var_name == input$vars_trends) %>%
            pull(variable)

          trends_plot(
            app_data$raw_data,
            var,
            input$vars_trends,
            input$country_trends,
            input$countries_trends,
            app_data$country_list,
            input$group_trends,
            app_data$db_variables,
            custom_df_trend(),
            input$color_base_trends,
            input$color_comp_trends,
            input$color_groups_trends
          )
        }
      })
  })
}
