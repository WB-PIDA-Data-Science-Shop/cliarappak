#' data module UI
#'
#' Data tab: bulk CSV/rds/dta downloads plus an interactive, filterable table.
#'
#' @param id a unique identifier for this module.
#' @param app_data Shared data list from [build_app_data()].
#'
#' @return a `tagList` of UI elements
#' @export
mod_data_ui <- function(id, app_data) {
  ns <- NS(id)

  tagList(
    bs4Card(
      title = "Data download",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = F,

      fluidRow(
        column(
          width = 2.4,
          downloadButton(
            ns("down_clust_ctf_stat"),
            "CTF Static (Cluster-level aggregates only)",
            style = "width:100%; background-color: #204d74; color: white"
          )
        ),
        column(
          width = 2.4,
          downloadButton(
            ns("down_all_ctf_stat"),
            "CTF Static (All indicators)",
            style = "width:100%; background-color: #204d74; color: white"
          )
        ),
        column(
          width = 2.4,
          downloadButton(
            ns("down_clust_ctf_dyn"),
            "CTF Dynamic (Cluster-level aggregates only)",
            style = "width:100%; background-color: #204d74; color: white"
          )
        ),
        column(
          width = 2.4,
          downloadButton(
            ns("down_all_ctf_dyn"),
            "CTF Dynamic (All indicators)",
            style = "width:100%; background-color: #204d74; color: white"
          )
        ),
        column(
          width = 2.4,
          downloadButton(
            ns("down_original"),
            "Original indicators",
            style = "width:100%; background-color: #204d74; color: white"
          )
        ),
        column(
          width = 2.4,
          downloadButton(
            ns("down_db_var"),
            "Data Dictionary",
            style = "width:100%; background-color: #204d74; color: white"
          )
        )
      )
    ),

    bs4Card(
      title = "Pre-Download Base & Comparison Country Selection",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsed = TRUE,
      fluidRow(
        column(
          width = 6,
          style = "padding-left: 24px",
          pickerInput(
            ns("country_dwnld"),
            label = helper(
              shiny_tag = tags$span("Base country:", style = "font-size: 28px; color: #051f3f;"),
              type = "inline",
              icon = "circle-question",
              title = "Base country",
              content = c(
                "Choose the base country of interest. (For some analysis, you can select more than one.) This menu can also be accessed in the Country Benchmarking tab"
              ),
              buttonLabel = "Close",
              fade = T,
              size = "s"
            ),
            choices = app_data$countries,
            choicesOpt = list(
              content = app_data$flags_with_countries,
              style = rep(length(app_data$flags_with_countries))
            ),
            selected = NULL,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE
            )
          )
        )),
      fluidRow(style = "height: 5px;"),

      ### Comparison card
      shiny::fluidRow(
        column(
          width = 6,
          pickerInput(
            ns("groups_dwnld"),
            label = helper(
              shiny_tag = "Select comparison groups",
              type = "inline",
              icon = "circle-question",
              title = "Pre-defined groups",
              content = c(
                "There are multiple ways to select the comparator countries. Here you can select one (or more) pre-defined group(s) (either as a comparator group itself or as a shortcut for selecting individual countries). When selecting more than one, it is the union (i.e., sum) of the groups that will be analyzed.This menu can also be accessed in the Country Benchmarking tab"
              ),
              buttonLabel = "Close",
              fade = T,
              size = "s"
            ),
            choices = app_data$group_list,
            selected = NULL,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE
            )
          )
        ),
        column(
          id = "show_countries_column_dwnld",
          width = 3,
          style = "display: flex; align-items: center; justify-content: center;",
          shinyWidgets::materialSwitch(
            inputId = ns("show_countries_dwnld"),
            label = helper(
              shiny_tag = tags$b("Show list of countries"),
              type = "inline",
              icon = "circle-question",
              title = "List of countries",
              content = c(
                "Here you can add and remove individual comparator countries. If you have already selected one or more the pre-defined groups, those countries will appear as selected, and you can manually add or remove."
              ),
              buttonLabel = "Close",
              fade = T,
              size = "s"
            ),
            value = FALSE,
            status = "success"
          )
        ),

        shiny::conditionalPanel(
          "input.show_countries_dwnld == true",
          ns = ns,

          fluidRow(style = "height: 15px;"),

          fluidRow(
            column(
              width = 12,
              checkboxGroupButtons(
                inputId = ns("countries_dwnld"),
                individual = TRUE,
                label = NULL,
                choices = app_data$countries,
                selected = "countries",
                checkIcon = list(
                  yes = icon("ok", lib = "glyphicon")
                )
              )
            )
          )
        )
      ) #fluid row
    ), #bs4


    bs4Card(
      title = "Interactive Data Access & Custom Download",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = F,
      fluidRow(
        column(
          width = 8,
          DT::dataTableOutput(ns("benchmark_datatable")) |>
            shinycssloaders::withSpinner(color = "#051f3f", type = 8)
        ),
        column(
          width = 4,
          bs4Card(
            title = "Select information to display",
            status = "success",
            width = 12,
            collapsible = FALSE,

            pickerInput(
              ns("countries_data"),
              label = "Select countries to include",
              choices = c("All", "Base country only", "Base + comparison countries"),
              selected = "All",
              options = list(
                "All" = list(disabled = FALSE),
                "Base country only" = list(disabled = TRUE),
                "Base + comparison countries" = list(disabled = TRUE)
              )
            ),

            pickerInput(
              ns("vars"),
              label = "Select institutional families to include",
              choices = app_data$definitions |> pull(Family),
              selected = app_data$definitions |> pull(Family),
              multiple = TRUE,
              options = list(`actions-box` = TRUE)
            ),

            radioGroupButtons(
              ns("data_source"),
              label = "Select a data source",
              choices = c("Closeness to frontier (Static)",
                          "Closeness to frontier (Dynamic)",
                          "Original indicators"),
              selected = "Closeness to frontier (Static)",
              direction = "vertical",
              justified = TRUE,
              checkIcon = list(
                yes = icon("ok", lib = "glyphicon")
              )
            ),
            #Input selector for download column names
            div(
              style = "display: flex; flex-direction: column; gap: 6px; align-items: flex-start;",
              helper(
                shiny_tag = tags$b("Descriptive Columns"),
                type = "inline",
                icon = "circle-question",
                title = "Descriptive Names",
                content = c(
                  "Here you can select whether you want abrreviated or full names for each of the columns in the downloaded data."
                ),
                buttonLabel = "Close",
                fade = TRUE,
                size = "s"
              ),
              shinyWidgets::materialSwitch(
                inputId = ns("descriptions_dwnld"),
                label = NULL,
                value = FALSE,
                status = "success"
              )
            ),
            shinyjs::hidden(
              radioGroupButtons(
                ns("data_value"),
                label = "Select information to show",
                choices = c("Value"),
                selected = "Value",
                direction = "vertical",
                justified = TRUE,
                checkIcon = list(
                  yes = icon("ok", lib = "glyphicon")
                )
              )
            ),

            p(tags$b("Download data")),

            downloadButton(
              ns("download_global_rds"),
              ".rds",
              style = "width:100%; background-color: #204d74; color: white"
            ),

            downloadButton(
              ns("download_global_csv"),
              ".csv",
              style = "width:100%; background-color: #204d74; color: white"
            ),

            downloadButton(
              ns("download_global_dta"),
              ".dta",
              style = "width:100%; background-color: #204d74; color: white"
            )
          )
        )
      )
    )
  )
}

#' data module server
#'
#' @param id a unique identifier for this module.
#' @param bench Named list of reactives returned by [mod_benchmark_server()].
#' @param app_data Shared data list from [build_app_data()].
#'
#' @return NULL; called for its side effects.
#' @export
mod_data_server <- function(id, bench, app_data) {
  moduleServer(id, function(input, output, session) {

    # Cross-tab sync (server.R:1659-1667, 1679-1710) -- all three are live,
    # ungated by "Apply selection", matching the original.
    observeEvent(bench$country(), {
      updatePickerInput(session, "country_dwnld", selected = bench$country())
    }, ignoreNULL = FALSE)

    observeEvent(bench$groups(), {
      updatePickerInput(session, "groups_dwnld", selected = bench$groups())
    }, ignoreNULL = FALSE)

    observeEvent(bench$countries(), {
      updateCheckboxGroupButtons(
        session, "countries_dwnld",
        choices = app_data$countries,
        checkIcon = list(yes = icon("ok", lib = "glyphicon", style = "color: #00000")),
        selected = bench$countries()
      )
    }, ignoreNULL = FALSE)

    # countries_data choices (server.R:266-275, part of the benchmark tab's
    # "Apply selection" handler)
    observeEvent(bench$select_trigger(), {
      updatePickerInput(
        session, "countries_data",
        choices = c("All", "Base country only", "Base + comparison countries"),
        options = list(
          "All" = list(disabled = FALSE),
          "Base country only" = list(disabled = FALSE),
          "Base + comparison countries" = list(disabled = FALSE)
        )
      )
    }, ignoreNULL = TRUE)

    #group_dwnld countries_dwnld agreement:
    observeEvent(
      input$groups_dwnld,
      {
        selected_dgroups <- input$groups_dwnld
        selected_dcountry <- input$country_dwnld

        if (is.null(selected_dgroups)) {
          selected <- NULL
        } else {
          selected <-
            app_data$country_list %>%
            filter(group %in% selected_dgroups) %>%
            select(country_name) %>%
            unique()

          if (!is.null(selected_dcountry)) {
            selected <- selected %>% filter(country_name != selected_dcountry)
          }

          selected <- selected %>% pluck(1)
        }

        updateCheckboxGroupButtons(
          session,
          "countries_dwnld",
          label = NULL,
          choices = app_data$countries,
          checkIcon = list(
            yes = icon("ok", lib = "glyphicon", style = "color: #e94152")
          ),
          selected = selected
        )
      },
      ignoreNULL = FALSE
    )

    #This creates a reactive pre-download version of the dataset for the user
    pre_download_data <- reactive({
      # Step 1: Select Data Based input$data_source
      data <- switch(
        input$data_source,
        "Closeness to frontier (Static)" = app_data$global_data,
        "Closeness to frontier (Dynamic)" = app_data$global_data_dyn,
        "Original indicators" = app_data$raw_data %>% select(-ends_with("_avg"))
      )
      # Step 2: Determine Groups
      groups <- app_data$all_groups
      #Step 3: Deal with Selected Countries
      selected_countries <- switch(
        input$countries_data,
        "All" = app_data$countries,
        "Base country only" = input$country_dwnld,
        "Base + comparison countries" = c(input$country_dwnld, input$countries_dwnld)
      )

      # Step 4: Pull Vars
      vars <- app_data$variable_names %>%
        filter(family_name %in% input$vars, var_level == "indicator") %>%
        pull(variable)

      # Step 5: Determine Variables Table for Selection
      vars_table <- switch(
        input$data_source,
        "Closeness to frontier (Static)" = c("country_name", "country_code", "country_group", "income_group", "region", vars),
        "Closeness to frontier (Dynamic)" = c("country_name", "country_code", "country_group", "income_group", "region", "year", vars),
        #For the raw dataset
        names(data)
      )
      vars_table <- unname(vars_table)

      # Step 6: Process Data (to ensure formatting)
      data <- data %>%
        filter(country_name %in% c(selected_countries, groups)) %>%
        ungroup() %>%
        mutate(across(where(is.numeric), \(x) round(x, 3))) %>%
        select(any_of(vars_table))
      # Step 7: Handle Rank selection
      if (input$data_value == "Rank") {
        data1 <-
          data %>%
          filter(country_group == 0) %>%
          mutate(
            across(
              6:ncol(.),
              ~ rank(desc(.), ties.method = "min")
            )
          )

        data <- data1
      }

      return(data)
    })

    output$benchmark_datatable <-
      DT::renderDataTable(
        server = FALSE,
        datatable(
          pre_download_data() %>%
            setnames(
              .,
              as.character(app_data$db_variables$variable),
              as.character(app_data$db_variables$variable),
              skip_absent = TRUE
            ),
          rownames = FALSE,
          extensions = c("FixedColumns"),
          filter = "none",
          options = list(
            scrollX = TRUE,
            scrollY = "550px",
            pageLength = 25,
            autoWidth = TRUE,
            dom = "lftipr",
            fixedColumns = list(leftColumns = 1, rightColumns = 0)
          )
        )
      )

    # Downloadable rds of selected dataset
    output$download_global_rds <-
      downloadHandler(
        filename = function() {
          paste0("CLIAR ", input$data_source, " data.rds")
        },
        content = function(file) {
          show_modal_spinner(color = "#17a2b8", text = "Loading Data")
          on.exit(remove_modal_spinner())

          write_rds(
            rds_prep(pre_download_data(), input$descriptions_dwnld, app_data$db_variables),
            file)
        }
      )

    # Downloadable csv of selected dataset
    output$download_global_csv <-
      downloadHandler(
        filename = function() {
          paste0("CLIAR ", input$data_source, " data.csv")
        },
        content = function(file) {
          show_modal_spinner(color = "#17a2b8", text = "Loading Data")
          on.exit(remove_modal_spinner())

          write_csv(
            csv_prep(pre_download_data(), input$descriptions_dwnld, app_data$db_variables),
            file,
            na = "")
        }
      )

    # Downloadable dta of selected dataset
    output$download_global_dta <-
      downloadHandler(
        filename = function() {
          paste0("CLIAR ", input$data_source, " data.dta")
        },
        content = function(file) {
          show_modal_spinner(color = "#17a2b8", text = "Loading Data")
          on.exit(remove_modal_spinner())

          write_dta(
            dta_prep(pre_download_data(), input$descriptions_dwnld, app_data$db_variables),
            file)
        }
      )

    #CTF Static (Cluster-level aggregates only)
    output$down_clust_ctf_stat <-
      downloadHandler(
        filename = function() {
          paste0("CTF Static (Cluster-level aggregates only) data.csv")
        },
        content = function(file) {
          show_modal_spinner(color = "#17a2b8", text = "Loading Data")
          on.exit(remove_modal_spinner())

          write_csv(app_data$ctf_long, file, na = "")
        }
      )

    #CTF Static (All indicators)
    output$down_all_ctf_stat <-
      downloadHandler(
        filename = function() {
          paste0("CLIAR CTF Static (All indicators) data.csv")
        },
        content = function(file) {
          show_modal_spinner(color = "#17a2b8", text = "Loading Data")
          on.exit(remove_modal_spinner())

          write_csv(app_data$global_data, file, na = "")
        }
      )

    #CTF Dynamic (Cluster-level aggregates only) data
    output$down_clust_ctf_dyn <-
      downloadHandler(
        filename = function() {
          paste0("CTF Dynamic (Cluster-level aggregates only) data.csv")
        },
        content = function(file) {
          show_modal_spinner(color = "#17a2b8", text = "Loading Data")
          on.exit(remove_modal_spinner())

          write_csv(
            app_data$global_data_dyn %>% select(1:6, (ncol(.) - 6):ncol(.)),
            file,
            na = ""
          )
        }
      )

    #CTF Dynamic (All indicators)
    output$down_all_ctf_dyn <-
      downloadHandler(
        filename = function() {
          paste0("CLIAR CTF Dynamic (All indicators) data.csv")
        },
        content = function(file) {
          show_modal_spinner(color = "#17a2b8", text = "Loading Data")
          on.exit(remove_modal_spinner())

          write_csv(app_data$global_data_dyn, file, na = "")
        }
      )

    #Original indicators
    output$down_original <-
      downloadHandler(
        filename = function() {
          paste0("CLIAR Original indicators data.csv")
        },
        content = function(file) {
          show_modal_spinner(color = "#17a2b8", text = "Loading Data")
          on.exit(remove_modal_spinner())

          write_csv(
            app_data$raw_data %>% select(-ends_with("_avg")),
            file,
            na = ""
          )
        }
      )

    #Data Dictionary - CLIAR
    output$down_db_var <-
      downloadHandler(
        filename = function() {
          paste0("CLIAR Data Dictionary.csv")
        },
        content = function(file) {
          show_modal_spinner(color = "#17a2b8", text = "Loading Data")
          on.exit(remove_modal_spinner())

          write_csv(app_data$db_variables, file, na = "")
        }
      )
  })
}
