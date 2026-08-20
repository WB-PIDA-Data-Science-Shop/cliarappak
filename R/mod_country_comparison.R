#' country comparison module UI
#'
#' Cross-Country Comparison tab (bar chart).
#'
#' @param id a unique identifier for this module.
#' @param app_data Shared data list from [build_app_data()].
#'
#' @return a `tagList` of UI elements
#' @export
mod_country_comparison_ui <- function(id, app_data) {
  ns <- NS(id)

  tagList(
    bs4Card(
      title = "Select information to display",
      status = "success",
      solidHeader = TRUE,
      width = 12,

      fluidRow(
        column(
          width = 3,
          pickerInput(
            ns("country_bar"),
            label = "Select a base country",
            choices = c("", app_data$countries),
            selected = NULL,
            multiple = FALSE
          )
        ),
        column(
          width = 3,
          pickerInput(
            inputId = ns("groups_bar"),
            label = "Select comparison groups",
            choices = app_data$group_list,
            selected = NULL,
            multiple = TRUE,
            options = list(
              `live-search` = TRUE,
              `actions-box` = TRUE
            )
          )
        ),
        column(
          width = 3,
          pickerInput(
            ns("vars_bar"),
            label = "Select indicator",
            choices = app_data$variable_list_benchmarked,
            selected = NULL,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              "max-options" = 3
            ),
            width = "100%"
          )
        ),
        column(
          width = 3,
          radioGroupButtons(
            ns("value_bar"),
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

    bs4Card(
      title = "Select individual comparison countries",
      width = 12,
      status = "success",
      collapsed = TRUE,

      checkboxGroupButtons(
        inputId = ns("countries_bar"),
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
      title = "Select Bar Graph Colors",
      status = "success",
      collapsed = TRUE,
      width = 12,

      fluidRow(
        column(
          width = 4,
          colourInput(
            ns("color_base_bar"),
            "Choose a base country color:",
            value = "#f29411"
          )
        ),
        column(
          width = 4,
          colourInput(
            ns("color_comp_bar"),
            "Choose a comparison country color:",
            value = "#080770")
        ),
        column(
          width = 4,
          colourInput(
            ns("color_groups_bar"),
            "Choose a comparison group color:",
            value = "#808080")
        )
      )),

    #======================================
    conditionalPanel(
      'input.country_bar !== "" && input.vars_bar != null',
      ns = ns,

      bs4Card(
        width = 12,
        solidHeader = FALSE,
        gradientColor = "primary",
        collapsible = FALSE,

        plotlyOutput(
          ns("bar_plot"),
          height = paste0(app_data$plot_height * 1.6, "px")
        )
      )
    )
  )
}

#' country comparison module server
#'
#' @param id a unique identifier for this module.
#' @param bench Named list of reactives returned by [mod_benchmark_server()].
#' @param app_data Shared data list from [build_app_data()].
#'
#' @return NULL; called for its side effects.
#' @export
mod_country_comparison_server <- function(id, bench, app_data) {
  moduleServer(id, function(input, output, session) {

    # Cross-tab sync: the original updated this tab's `country_bar`/`groups_bar`
    # directly from the flat server() scope (server.R:242-349, 642-661, 692-711,
    # 1657-1677) whenever the benchmark tab's own inputs changed. Each consuming
    # module now owns that syncing itself, watching `bench`'s reactives instead.
    observeEvent(bench$select_trigger(), {
      if (!is.null(bench$groups())) {
        updatePickerInput(session, "groups_bar", selected = bench$groups())
      }
      updatePickerInput(session, "country_bar", selected = bench$base_country())
    }, ignoreNULL = TRUE)

    observeEvent(bench$country(), {
      if (length(bench$country()) <= 1) {
        updatePickerInput(session, "country_bar", selected = bench$country())
      }
    }, ignoreNULL = FALSE)

    # groups_bar choices grow to include any custom groups the benchmark tab saved
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
          session, "groups_bar",
          choices = as.list(append(app_data$group_list, Custom)),
          selected = unique(c(bench$groups(), unique(cdf$Grp)))
        )
      } else {
        updatePickerInput(session, "groups_bar", choices = app_data$group_list, selected = bench$groups())
      }
    }, ignoreNULL = TRUE)

    # The original (server.R:1875-1885) checked `input$group_trends` here --
    # a copy-paste leftover from the Trends tab's identical `custom_df_trend`
    # (server.R:2079-2089), which correctly checks its own `group_trends`.
    # This tab's own commented-out earlier draft (server.R:1871-1873) confirms
    # the intent was always `groups_bar`.
    custom_df_bar <- reactive({
      if (any(!input$groups_bar %in% unlist(app_data$group_list))) {
        custom_df_bar <- bench$custom_grps_df()[bench$custom_grps_df()$Grp %in% input$groups_bar, ]
      } else {
        custom_df_bar <- NULL
      }
      return(custom_df_bar)
    })

    # Reactive expression to get dataset based on user input. Named `bar_data`
    # here -- the original named this bare `data`, which silently shadowed the
    # unrelated `data` reactive in the benchmark tab's own scope for the rest
    # of the flat server() function. See MIGRATION_GUIDE.md 4d-1.
    bar_data <- reactive({
      if (input$value_bar == "ctf") {
        app_data$global_data
      } else {
        app_data$raw_data %>%
          select(-Year) %>%
          group_by(country_code, country_name, income_group, region) %>%
          fill(everything()) %>%
          slice(n())
      }
    })

    filtered_countries_bar <- reactive({
      req(input$vars_bar)
      app_data$countries[!sapply(app_data$countries, function(country) check_data(bar_data(), country, input$vars_bar, db_variables = app_data$db_variables))]
    })

    observeEvent(
      input$vars_bar,
      {
        available_countries_bar <- na.omit(filtered_countries_bar())
        updateCheckboxGroupButtons(
          session,
          "countries_bar",
          choices = available_countries_bar,
          checkIcon = list(
            yes = icon("ok", lib = "glyphicon", style = "color: #00000")
          ),
          selected = input$countries_bar
        )
      })

    output$bar_plot <-
      renderPlotly({
        validate(need(check_data(app_data$global_data, input$country_bar, input$vars_bar, db_variables = app_data$db_variables) == FALSE, 'Country Comparison is not available for this Indicator for the selected base country'))

        static_bar(
          bar_data(),
          input$country_bar,
          input$countries_bar,
          input$groups_bar,
          input$vars_bar,
          app_data$variable_names,
          custom_df_bar(),
          input$color_base_bar,
          input$color_comp_bar,
          input$color_groups_bar,
          app_data$ctf_long_dyn
        ) %>%
          interactive_bar(
            input$vars_bar,
            app_data$db_variables,
            plotly_remove_buttons
          )
      })

    output$definition_bar <-
      renderTable({
        app_data$db_variables %>%
          filter(var_name == input$vars_bar) %>%
          select(Indicator = var_name, Family = family_name, Description = description, Source = source)
      })

    # server.R:1971-1979 (mod_bivariate's `high_group` reactive) reads this
    # tab's custom_df_bar() directly out of the flat scope -- a genuine
    # cross-module dependency, not just the shared `bench` state. Returned so
    # app_server.R can thread it into mod_bivariate_server().
    list(
      custom_df_bar = custom_df_bar
    )
  })
}
