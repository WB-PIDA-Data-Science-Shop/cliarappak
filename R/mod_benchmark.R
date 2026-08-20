#' benchmark module UI
#'
#' Country benchmarking tab. Owns almost all of the shared selection state
#' (base country, comparison countries/groups, custom groups, family,
#' thresholds) that every other tab reads -- see [mod_benchmark_server()].
#'
#' @param id a unique identifier for this module.
#' @param app_data Shared data list from [build_app_data()].
#'
#' @return a `tagList` of UI elements
#' @export
mod_benchmark_ui <- function(id, app_data) {
  ns <- NS(id)

  tagList(
    useShinyjs(),

    fluidRow(
      column(
        width = 6,
        style = "padding-left: 24px",
        pickerInput(
          ns("country"),
          label = helper(
            shiny_tag = tags$span("Base country:", style = "font-size: 28px; color: #051f3f;"),
            type = "inline",
            icon = "circle-question",
            title = "Base country",
            content = c(
              "Choose the base country of interest. (For some analysis, you can select more than one.)"
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
      ),
      ## Guide action button
      column(
        width = 3,
        tags$span("Guided tour", style = "font-size: 1rem; color: #051f3f; font-weight:bold"),
        fluidRow(
          shinyWidgets::actionBttn(
            inputId = ns("start_guide_bench"),
            label = "Start",
            icon = shiny::icon("gear"),
            style = "jelly",
            color = "primary",
            size = "sm"
          )
        )
      ),
      column(
        id = "input_buttons",
        width = 3,
        fluidRow(
          column(
            width = 8,
            helper(
              shiny_tag = tags$span("Selection of Countries", style = "font-size: 1rem; color: #051f3f; font-weight:bold"),
              type = "inline",
              icon = "circle-question",
              title = "Saving and loading Selection of Countries",
              content = c(
                "You can save your selected inputs to return to at a future time: click “Save Selection of Countries” button to download a .rds file to your computer with that information. When you return to the dashboard, you can click “Load Selection of Countries” button and then “Browse” to select this same .rds file. Loading this .rds file will re-populate all of the selections that you previously made."
              ),
              buttonLabel = "Close",
              fade = T,
              size = "s"
            )
          ),
          column(4)
        ),
        fluidRow(
          ## Load inputs button
          buttons_func(
            id = ns("load_inputs"),
            lab = "Load"
          ),
          ## Save inputs button
          shinyjs::disabled(
            downloadButton(
              ns("save_inputs"),
              "Save"
            )
          )
        )
      )
    ),

    fluidRow(style = "height: 5px;"),

    ### Comparison card
    bs4Card(
      title = "Comparator countries",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,

      shiny::fluidRow(
        column(
          width = 6,
          pickerInput(
            ns("groups"),
            label = helper(
              shiny_tag = "Select comparison groups",
              type = "inline",
              icon = "circle-question",
              title = "Pre-defined groups",
              content = c(
                "There are multiple ways to select the comparator countries. Here you can select one (or more) pre-defined group(s) (either as a comparator group itself or as a shortcut for selecting individual countries). When selecting more than one, it is the union (i.e., sum) of the groups that will be analyzed."
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
          id = "show_countries_column",
          width = 3,
          style = "display: flex; flex-direction: column; gap: 6px; align-items: flex-start;",
          helper(
            shiny_tag = tags$b("Show list of countries"),
            type = "inline",
            icon = "circle-question",
            title = "List of countries",
            content = c(
              "Here you can add and remove individual comparator countries. If you have already selected one or more the pre-defined groups, those countries will appear as selected, and you can manually add or remove."
            ),
            buttonLabel = "Close",
            fade = TRUE,
            size = "s"
          ),
          shinyWidgets::materialSwitch(
            inputId = ns("show_countries"),
            label = NULL,
            value = FALSE,
            status = "success"
          )
        ),
        column(
          id = "custom_grps_column",
          width = 3,
          style = "display: flex; flex-direction: column; gap: 6px; align-items: flex-start;",
          helper(
            shiny_tag = tags$b("Create custom groups"),
              type = "inline",
              icon = "circle-question",
              title = "Custom groups",
              content = paste0(
                "Alternative, you may create up to three custom groups of countries. This feature will additionally display in the Benchmarking graphs the median estimates of each custom group.",
                "<br><br><b>Note:</b> Currently custom groups are not allowed when displaying ranks instead of values, when ranking from best to worst, or when doing the dynamic benchmark."
              ),
              buttonLabel = "Close",
              fade = T,
              size = "s"
          ),
          shinyWidgets::materialSwitch(
            inputId = ns("create_custom_grps"),
            label = NULL,
            value = FALSE,
            status = "success"
          )
        )
      ),

      shiny::conditionalPanel(
        "input.create_custom_grps == true",
        ns = ns,

        fluidRow(
          column(
            width = 12,
            shinyWidgets::materialSwitch(
               inputId = ns("show_custom_grps"),
               label = tags$b("Show custom groups"),
               status = "success",
               value = TRUE
             )
          )
        ),
        fluidRow(
          column(
            width = 3,
            numericInput(
              inputId = ns("custom_grps_count"),
              label = "Number of groups",
              value = 1,
              min = 1,
              max = 3,
              step = 1
            )
          ),
          column(
            width = 3,
            style = "display: flex; align-items: center; justify-content: center;",
            shinyWidgets::actionBttn(
              inputId = ns("save_custom_grps"),
              label = "Save custom groups",
              icon = shiny::icon("save"),
              style = "jelly",
              color = "primary",
              size = "sm"
            )
          ),
          column(
            width = 12,
            conditionalPanel(
              "input.custom_grps_count >= 1",
              ns = ns,
              uiOutput(ns("custom_grps"))
            )
          )
        )
      ),
      #### Countries list
      shiny::conditionalPanel(
        "input.show_countries == true",
        ns = ns,

        fluidRow(style = "height: 15px;"),

        fluidRow(
          column(
            width = 12,
            checkboxGroupButtons(
              inputId = ns("countries"),
              individual = TRUE,
              label = NULL,
              choices = app_data$countries,
              selected = NULL,
              checkIcon = list(
                yes = icon(
                  "ok",
                  lib = "glyphicon"
                )
              )
            )
          )
        )
      )
    ),

    ### Bench card
    bs4Card(
      title = "Benchmarking options",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      fluidRow(
        column(
          width = 6,
          pickerInput(
            inputId = ns("threshold"),
            label = helper(
              shiny_tag = tags$b("Benchmarking Thresholds"),
              type = "inline",
              icon = "circle-question",
              title = "Benchmarking Thresholds",
              content = c(
                "The default benchmarking thresholds for weak, emerging and strong institutions are 25th and 50th percentiles. You can also select the “Terciles” option, which uses 33rd and 66th percentiles as thresholds instead."
              ),
              buttonLabel = "Close",
              fade = T,
              size = "s"
            ),
            choices = c("Default","Terciles")
          )
        ),
        column(width = 1),
        column(
          width = 5,
          div(
            id = "benchmark_dots_div",
            prettyCheckbox(
              inputId = ns("benchmark_dots"),
              label = helper(
                shiny_tag = tags$b("Show comparison countries"),
                type = "inline",
                icon = "circle-question",
                title = "Show comparison countries",
                content = c(
                  "Select this option to show the comparison countries as white circles on the plots. You may hover over each circle to see the country name. Note that individual countries are represented by circles in the first example below. This shows the distribution of values for the comparison group."
                ),
                buttonLabel = "Close",
                fade = T,
                size = "s"
              ),
              value = FALSE,
              icon = icon("check"),
              status = "success"
            )
          ),
          div(
            id = "rank_div",
            prettyCheckbox(
              inputId = ns("rank"),
              label = helper(
                shiny_tag = tags$b("Show rank instead of value"),
                type = "inline",
                icon = "circle-question",
                title = "Show rank instead of value",
                content = c(
                  "Select this option to change the x-axis of the static benchmarking plot to display rankings instead of the CTF value."
                ),
                buttonLabel = "Close",
                fade = T,
                size = "s"
              ),
              value = FALSE,
              icon = icon("check"),
              status = "success"
            )
          ),
          div(
            id = "preset_order_div",
            prettyCheckbox(
              inputId = ns("preset_order"),
              label = helper(
                shiny_tag = tags$b("Rank indicators from best to worst"),
                type = "inline",
                icon = "circle-question",
                title = "Rank indicators from best to worst",
                content = c(
                  "Select this option to change the ordering of the variables on the vertical axis of the figure. Ranking from best to worst will place the indicator for which the base country has the highest value first and the indicator with the lowest value last."
                ),
                buttonLabel = "Close",
                fade = T,
                size = "s"
              ),
              value = FALSE,
              icon = icon("check"),
              status = "success"
            )
          )
        )
      )
    ),

    ### Outputs card
    bs4Card(
      title = "Outputs",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      fluidRow(
        column(
          width = 6,
          pickerInput(
            ns("family"),
            label = helper(
              shiny_tag = tags$b("Select institutional cluster"),
              type = "inline",
              icon = "circle-question",
              title = "Institutional cluster",
              content = c(
                "Choose the institutional cluster you would like to display. The overview displays the aggregate results at the institutional-cluster level. When selecting a specific institutional-cluster, the individual indicators/components will be displayed."
              ),
              buttonLabel = "Close",
              fade = T,
              size = "s"
            ),
            choices = c("Overview", names(app_data$variable_list)),
            selected = NULL
          )
        ),
        # Please do not delete the below button, though not being displayed. It has downstream impact on other process.
        column(
          width = 1,
          pickerInput(
            inputId = ns("benchmark_median"),
            label = "Show group median",
            choices = append(
              "Comparison countries",
              app_data$group_list
            ),
            selected = NULL,
            multiple = TRUE,
            options = list(
              `live-search` = TRUE,
              "max-options" = 3
            )
          )
        ),
        column(
          width = 4,
          style = "display: flex; align-items: center; justify-content: center;",
          fluidRow(
            column(
              width = 10,
              uiOutput(
                ns("select_button")
              )
            ),
            column(
              width = 1,
              helper(
                shiny_tag = NULL,
                type = "inline",
                icon = "circle-question",
                title = "Apply",
                content = c(
                  "Click on this box to (re-)run the analysis and (re-)load the resulting graphs. Note that this has to be done for every new selection or option, including a different institutional cluster. This option is enabled when the base country and at least 10 comparison countries are selected."
                ),
                buttonLabel = "Close",
                fade = T,
                size = "s"
              )
            ),
            column(1)
          )
        )
      ),
      fluidRow(
        column(
          width = 2,
          helper(
            shiny_tag = tags$b("Downloads"),
            type = "inline",
            icon = "circle-question",
            title = "Pre-populated reports and data",
            content = c(
              "Download pre-populated Word or Power Point documents with the results. Note that you may select the
              “Advanced Report” box to receive more detailed information - including all dynamic graphs. Select the help
              button next to the checkbox to learn more.

              Click the download “Data” button to download a CSV file that contains the data needed to recreate the benchmarking graphs."
            ),
            buttonLabel = "Close",
            fade = T,
            size = "s"
          ),
        )
      ),
      fluidRow(
        column(
          width = 9,
          fluidRow(
            id = "download_reports",
            column(
              width = 3,
              shinyjs::disabled(
                downloadButton(
                  ns("report"),
                  "Editable report",
                  style = "width:100%; background-color: #204d74; color: white"
                )
              ),

            ),
            column(
              width = 3,
              shinyjs::disabled(
                downloadButton(
                  ns("advreport"),
                  "Advanced Report",
                  style = "width:100%; background-color: #204d74; color: white"
                )
              )
            ),
            column(
              width = 3,
              shinyjs::disabled(
                downloadButton(
                  ns("pptreport"),
                  "PPT report",
                  style = "width:100%; background-color: #204d74; color: white"
                )
              )
            ),
            column(
              id = "download_data_opt",
              width = 3,
              shinyjs::disabled(
                downloadButton(
                  ns("download_data_1"),
                  "Data",
                  style = "width:100%; background-color: #204d74; color: white"

                )
              )
            )
          )
        ),

        column(
          width = 3,
          shinyjs::disabled(
            downloadButton(
              ns("download_Coverage"),
              "Coverage report",
              style = "width:100%; background-color: #204d74; color: white"
            )
          )
        )
      )
    ),

    ### Static Benchmarks ----
    bs4Card(
      title = "Static Benchmarks",
      collapsible = TRUE,
      width = 12,

      conditionalPanel(
        "input.select !== 0",
        ns = ns,
        fluidRow(
          column(
            width = 12,
            plotlyOutput(
              ns("plot"),
              height = paste0(2.25 * app_data$plot_height, "px")
            ) %>% shinycssloaders::withSpinner(color = "#051f3f", type = 8)
          )
        ),
        fluidRow(
          shinyWidgets:: materialSwitch(
            inputId = ns("show_plot_notes"),
            label = "Show notes",
            status = "success",
            value = FALSE
          )

        ),

        conditionalPanel(
          "input.show_plot_notes !== false",
          ns = ns,

          fluidRow(

            column(
              width = 12,
              htmlOutput(
                ns("plot_notes")
              )
            )

          )
        )
      )
    ),

    ### Dynamic benchmark tab  -------------------------------------------------------
    bs4Card(
      width = 12,
      solidHeader = FALSE,
      gradientColor = "primary",
      title = "Dynamic Benchmarks",
      collapsible = TRUE,
      tags$style(paste0("
          #", ns("dynamic_benchmark_plot"), " {
            height: 100%;
            overflow-y: scroll;
          }
        ")),

      conditionalPanel(
        "input.select !== 0 && output.plot!=null",
        ns = ns,
        fluidRow(

          column(
            width = 12,
            plotlyOutput(
              ns("dynamic_benchmark_plot"),
              height =  paste0(app_data$plot_height * 4, "px"),

            ) %>% shinycssloaders::withSpinner(color = "#051f3f", type = 8)
          )

        )

      )

    ),

    bs4Card(
      title = "Indicator definitions",
      collapsible = TRUE,
      collapsed = TRUE,
      status = "secondary",
      solidHeader = TRUE,
      width = 12,

      tableOutput(ns('definition'))
    )
  )
}

#' benchmark module server
#'
#' Owns the country/comparison-group/family/threshold selection state that
#' every other tab in the app reads. Returns a named list of reactives (the
#' "bench" list) that consuming modules pull from instead of reaching for a
#' shared top-level `server()` scope the way the original `cliarapp` did.
#'
#' Three deliberate departures from a literal line-by-line port of
#' `server.R:47-350,353-980,984-1250,1325-1655,2870-2880,2959-3095`, each
#' because the original code was found to be dead or broken, not because the
#' design changed:
#' - `server.R:57-64` (`input$start` / `guide_landing_page`) is not ported:
#'   `guide_landing_page` is entirely commented out in `guides.R`, and no UI
#'   element anywhere sets `input$start`, so this observer could only ever
#'   error if it somehow fired -- which it never can.
#' - `server.R:1237-1247` (`na_indicators`) and `server.R:1295-1306`
#'   (`data_family_median`) are not ported: both are computed once and never
#'   read anywhere else in `server.R`.
#' - `download_data_1`'s `data2` uses `data_avg()`, not the ambiguous `data`
#'   reactive from the original flat scope -- see MIGRATION_GUIDE.md 4d-1.
#'
#' Cross-tab syncing that the original did directly (e.g. updating the
#' Cross-Country Comparison tab's `groups_bar` picker whenever `groups` or
#' custom groups change) is NOT done here -- it can't be, `mod_benchmark`
#' has no access to other modules' namespaced inputs. Instead this returns
#' `select_trigger` and `custom_df` so each consuming module can replicate
#' the sync itself, watching this module's state instead of owning it.
#'
#' @param id a unique identifier for this module.
#' @param app_data Shared data list from [build_app_data()].
#'
#' @return A named list of reactives read by every other module.
#' @export
mod_benchmark_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shinyjs::disable("preset_order")
    shinyjs::hide("benchmark_median")

    observe_helpers()

    observeEvent(input$start_guide_bench, {
      guide_benchmark$init()$start()
    })

    ## Base country ------------------------------------------------------------
    base_country <- eventReactive(input$select, input$country, ignoreNULL = FALSE)

    ## When load inputs button is clicked
    shiny::observeEvent(input$load_inputs, {
      shiny::showModal(
        modalDialog(
          title = htmltools::tags$span(htmltools::tags$strong("Please upload an input file")),
          tagList(
            shiny::fluidRow(
              shiny::fileInput(
                inputId = ns("input_file"),
                label = "",
                accept = ".rds"
              )
            ),
            shiny::fluidRow(
              buttons_func(ns("submit"), "Submit")
            ),
            shiny::fluidRow(style = "height:15px;")
          ),
          easyClose = FALSE
        ))
    })

    observeEvent(input$rank, {
      if (input$rank == FALSE) {
        shinyjs::disable("preset_order")
      } else {
        shinyjs::enable("preset_order")
      }
    })

    ## Once the submit button is clicked, check to see if the file contains core fields
    saved_inputs_df <- shiny::eventReactive(input$submit, {
      file <- input$input_file
      req(file)

      saved_inputs_df <- readRDS(file$datapath)

      core_fields <- c("country", "groups", "family","benchmark_median","benchmark_dots","rank",
                       "threshold", "worst_to_best_order", "comparison_countries", "create_custom_groups"
      )

      if (all(!core_fields %in% names(saved_inputs_df))) {
        saved_inputs_df <- NULL
      }

      return(saved_inputs_df)
    })

    shiny::observeEvent(input$submit, {
      core_fields <- c("country", "groups", "family","benchmark_median","benchmark_dots","rank",
                       "threshold", "worst_to_best_order", "comparison_countries", "create_custom_groups"
      )

      if (all(!core_fields %in% names(saved_inputs_df()))) {
        toast_messages_func("error", "Invalid file")
      }

      if (all(core_fields %in% names(saved_inputs_df()))) {
        waiter::waiter_show(html = shiny::tagList(
          waiter::spin_ring(),
          shiny::h4("Fetching data ...")
        ))

        shinyWidgets::updatePickerInput(session = session, inputId = "country", selected = saved_inputs_df()$country)
        shinyWidgets::updatePickerInput(session = session, inputId = "groups", selected = unlist(strsplit(saved_inputs_df()$groups, ";")))
        shinyWidgets::updatePickerInput(session = session, inputId = "family", selected = saved_inputs_df()$family)
        shinyWidgets::updatePickerInput(session = session, inputId = "benchmark_median", selected = unlist(strsplit(saved_inputs_df()$benchmark_median, ";")))
        shinyWidgets::updatePrettyCheckbox(session = session, inputId = "benchmark_dots", value = saved_inputs_df()$benchmark_dots)
        shinyWidgets::updatePrettyCheckbox(session = session, inputId = "rank", value = saved_inputs_df()$rank)
        shinyWidgets::updatePickerInput(session = session, inputId = "threshold", selected = saved_inputs_df()$threshold)
        shinyWidgets::updatePrettyCheckbox(session = session, inputId = "preset_order", value = saved_inputs_df()$worst_to_best_order)

        removeModal()
        waiter::waiter_hide()

        shinyjs::show("save_inputs")
        shinyjs::disable("save_inputs")
        shinyjs::disable("download_data_1")
      }
    })

    ## Start of benchmark tab control inputs----------------------

    ### Update this tab's own state/buttons based on "Apply selection" -------
    ### (cross-tab picker syncing that the original did here directly has
    ### moved into each consuming module -- see select_trigger below)
    observeEvent(
      input$select,
      {
        toggleState(id = "report", condition = input$select, shinyjs::disable("report"))
        toggleState(id = "advreport", condition = input$select, shinyjs::disable("advreport"))
        toggleState(id = "pptreport", condition = input$select, shinyjs::disable("pptreport"))
        toggleState(id = "download_Coverage", condition = input$select, shinyjs::disable("download_Coverage"))
        toggleState(id = "download_missing", condition = input$select, shinyjs::disable("download_missing"))
        toggleState(id = "download_data_1", condition = input$select, shinyjs::disable("download_data_1"))
      },
      ignoreNULL = TRUE
    )

    ## Create comparison group inputs where users can insert custom group names and countries
    custom_group_fields_reactive <- reactive({
      n_fields <- input$custom_grps_count
      ui_fields <- c()

      lapply(1:n_fields, function(i) {
        custom_names <- ""
        custom_countries <- NULL

        if (n_fields >= 1) {
          custom_names <- isolate(input[[paste("custom_grps_names", i, sep = "_")]])
          custom_countries <- isolate(input[[paste("custom_grps_countries", i, sep = "_")]])

          value_textInput <- custom_names
          selected_pickerinput <- custom_countries

          ui_fields[[i]] <- shiny::fluidRow(
            width = 6,
            shiny::column(
              width = 6,
              shiny::textInput(
                inputId = ns(paste("custom_grps_names", i, sep = "_")),
                label = paste("Insert the name of group ", i),
                value = value_textInput
              )
            ),
            shiny::column(
              width = 6,
              shinyWidgets::pickerInput(
                inputId = ns(paste("custom_grps_countries", i, sep = "_")),
                label = paste("Select countries that fall into group ", i),
                choices = c("", app_data$countries[!app_data$countries %in% input$country]),
                selected = selected_pickerinput,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `live-search` = TRUE
                )
              )
            )
          )
        }
      })
    })

    shiny::observeEvent(input$submit, {
      if (nrow(saved_inputs_df()) > 0 & saved_inputs_df()$create_custom_groups == TRUE) {
        shinyWidgets::updatePrettyCheckbox(session = session, inputId = "create_custom_grps", value = saved_inputs_df()$create_custom_groups)
        shiny::updateNumericInput(session = session, inputId = "custom_grps_count", value = saved_inputs_df()$no_custom_grps)

        lapply(1:input$custom_grps_count, function(i) {
          if (saved_inputs_df()$no_custom_grps <= input$custom_grps_count) {
            shiny::updateTextInput(
              session = session,
              inputId = paste("custom_grps_names", i, sep = "_"),
              label = paste("Insert the name of group ", i),
              value = saved_inputs_df()[paste("custom_grps_names", i, sep = "_")]
            )

            shinyWidgets::updatePickerInput(
              session = session,
              inputId = paste("custom_grps_countries", i, sep = "_"),
              label = paste("Select countries that fall into group ", i),
              choices = c("", saved_inputs_df()$comparison_sountries[!saved_inputs_df()$comparison_sountries %in% input$country]),
              selected = unlist(strsplit(as.character(saved_inputs_df()[paste("custom_grps_countries", i, sep = "_")]), split = ";"))
            )
          }
        })
      }
    })

    ## Display the ui
    output$custom_grps <- renderUI({
      custom_group_fields_reactive()
    })

    ## Generate a dataframe containing the custom groups
    custom_grps_df <- shiny::eventReactive(input$save_custom_grps, {
      n_fields <- input$custom_grps_count

      if (n_fields > 0) {
        custom_grps_list <- list()

        for (i in 1:n_fields) {
          grp_name <- as.character(input[[paste("custom_grps_names", i, sep = "_")]])
          country_selection <- as.vector(input[[paste("custom_grps_countries", i, sep = "_")]])

          if (!is.null(grp_name) & !is.null(country_selection)) {
            custom_grps_list[[i]] <- data.frame(Category = "Custom", Grp = grp_name, Countries = country_selection)
          } else {
            custom_grps_list[[i]] <- NULL
          }
        }

        custom_grps_df <- dplyr::bind_rows(custom_grps_list)
      } else {
        custom_grps_df <- NULL
      }

      if (nrow(custom_grps_df) == 0) {
        custom_grps_df <- NULL
      }

      return(custom_grps_df)
    })

    ## once the save button is clicked
    shiny::observeEvent(input$save_custom_grps, {
      if (is.null(custom_grps_df())) {
        shinyWidgets::updatePrettyCheckbox(session = session, inputId = "create_custom_grps", value = FALSE)
      }

      if (input$create_custom_grps == FALSE) {
        custom_grps_df() <- NULL
      }
    })

    ## Disable and hide
    shiny::observeEvent(input$create_custom_grps, {
      if (input$create_custom_grps == TRUE) {
        shinyWidgets::updateMaterialSwitch(session = session, inputId = "show_countries", value = FALSE)
        shinyjs::disable(id = "show_countries")
      } else {
        shinyjs::enable(id = "show_countries")
      }
    })

    ## Turning on the "Show custom groups" switch shows the custom groups ui
    shiny::observeEvent(input$show_custom_grps, {
      if (input$show_custom_grps == TRUE) {
        shinyjs::show(id = "custom_grps_count")
        shinyjs::show(id = "custom_grps")
        shinyjs::show(id = "save_custom_grps")
      } else {
        shinyjs::hide(id = "custom_grps_count")
        shinyjs::hide(id = "custom_grps")
        shinyjs::hide(id = "save_custom_grps")
      }
    })

    ### Once the save button is clicked (***)
    shiny::observeEvent(input$save_custom_grps, {
      ### check if any of the custom group names is part of group list.
      ### If so, ask the user to change the name
      if (any(custom_grps_df()$Grp %in% unlist(app_data$group_list))) {
        dup_grp_names <- unique(custom_grps_df()$Grp[custom_grps_df()$Grp %in% unlist(app_data$group_list)])

        shiny::showModal(
          modalDialog(
            shiny::tagList(
              shiny::tags$p(
                paste0("The following list includes group name(s) that already exist(s) within the
                      original group list. Please modify the group name(s) to continue.")
              ),
              shiny::tags$p(
                paste(as.character(dup_grp_names), collapse = " , ")
              )
            )
          )
        )
      } else {
        ### turn off the "Show custom groups" switch
        shinyWidgets::updateMaterialSwitch(session = session, inputId = "show_custom_grps", value = FALSE)

        ## and edit the "Select comparison groups" and "Show group median" fields to include these custom groups
        Custom <- list(unique(custom_grps_df()$Grp))

        if (length(unique(custom_grps_df()$Grp)) == 1) {
          names(Custom) <- unique(custom_grps_df()$Grp)
        } else {
          names(Custom) <- "Custom"
        }

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "groups",
          choices = as.list(append(app_data$group_list, Custom)),
          selected = unique(c(input$groups, unique(custom_grps_df()$Grp)))
        )

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "benchmark_median",
          choices = append("Comparison countries", append(app_data$group_list, Custom)),
          selected = unique(c(input$benchmark_median, custom_grps_df()$Grp))[1:3],
          options = list(
            `live-search` = TRUE,
            "max-options" = 3
          )
        )
      }
    })

    ## Unselecting the "Create custom groups" field should reset all custom group fields,
    ## but retain all other inputs
    shiny::observeEvent(input$create_custom_grps, {
      if (input$create_custom_grps == FALSE) {
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "groups",
          choices = app_data$group_list,
          selected = input$groups[!input$groups %in% unique(custom_grps_df()$Grp)]
        )

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "benchmark_median",
          choices = append("Comparison countries", app_data$group_list),
          selected = input$benchmark_median[!input$benchmark_median %in% unique(custom_grps_df()$Grp)],
          options = list(
            `live-search` = TRUE,
            "max-options" = 3
          )
        )

        updateCheckboxGroupButtons(
          session,
          "countries",
          label = NULL,
          choices = app_data$countries,
          checkIcon = list(
            yes = icon("ok", lib = "glyphicon", style = "color: #e94152")
          ),
          selected = input$countries[!input$countries %in% unique(custom_grps_df()$Countries)]
        )
      }
    })

    ## Comparison countries
    observeEvent(
      input$groups,
      {
        selected_groups <- input$groups
        selected_country <- input$country

        if (is.null(selected_groups)) {
          selected <- NULL
        } else {
          selected <-
            app_data$country_list %>%
            filter(group %in% selected_groups) %>%
            select(country_name) %>%
            unique()

          if (!is.null(selected_country)) {
            selected <- selected %>% filter(country_name != selected_country)
          }

          selected <- selected %>% pluck(1)
        }

        updateCheckboxGroupButtons(
          session,
          "countries",
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

    # When custom groups are included and the group field is updated, append the countries to
    # the initial list of countries displayed
    observeEvent(
      list(input$groups, input$save_custom_grps),
      {
        if (!is.null(custom_grps_df())) {
          custom_grp_countries <- custom_grps_df()$Countries[custom_grps_df()$Grp %in% input$groups]

          preselected_grp_countries <- app_data$country_list %>%
            filter(group %in% input$groups) %>%
            pull(country_name)

          if (length(preselected_grp_countries) > 0) {
            selected_c <- unique(c(custom_grp_countries, preselected_grp_countries))
          } else {
            selected_c <- unique(custom_grp_countries)
          }

          updateCheckboxGroupButtons(
            session,
            "countries",
            label = NULL,
            choices = app_data$countries,
            checkIcon = list(
              yes = icon("ok", lib = "glyphicon", style = "color: #e94152")
            ),
            selected = selected_c
          )
        }
      }
    )

    ## Validate options
    output$select_button <-
      renderUI({
        if ((length(input$countries) + length(input$groups)) >= 10 && length(input$country) >= 1) {
          bs4Dash::actionButton(
            ns("select"),
            "Apply selection",
            icon = icon("check"),
            status = "success",
            width = "100%",
            shinyjs::enable("save_inputs")
          )
        } else {
          bs4Dash::actionButton(
            ns("select"),
            "Select a base country and at least 10 comparison countries to apply selection",
            icon = icon("triangle-exclamation"),
            status = "warning",
            width = "100%",
            shinyjs::disable("report"),
            shinyjs::disable("advreport"),
            shinyjs::disable("pptreport"),
            shinyjs::disable('download_missing'),
            shinyjs::disable('download_Coverage'),
            shinyjs::disable("download_data_1"),
            shinyjs::disable("save_inputs")
          )
        }
      })

    observeEvent(
      input$countries,
      {
        toggleState(id = "select", condition = length(input$countries) >= 10, shinyjs::disable("report"))
        toggleState(id = "select", condition = length(input$countries) >= 10, shinyjs::disable("advreport"))
        toggleState(id = "select", condition = length(input$countries) >= 10, shinyjs::disable("pptreport"))
        toggleState(id = "select", condition = length(input$countries) >= 10, shinyjs::disable("download_missing"))
        toggleState(id = "select", condition = length(input$countries) >= 10, shinyjs::disable("download_data_1"))
      },
      ignoreNULL = FALSE
    )

    ## End of benchmark tab control inputs----------------------

    ## Reactive objects ==============================================

    ### Benchmark data -----------------------------------------------
    vars <- eventReactive(
      input$select,
      {
        if (input$family == "Overview") {
          app_data$vars_all
        } else {
          app_data$variable_names %>%
            filter(family_name == input$family) %>%
            pull(variable) %>%
            unique()
        }
      }
    )

    ### Comparison group note (group or countries) -------------------------
    note_compare <- eventReactive(
      input$select,
      {
        group_list_countries <- app_data$country_list %>%
          filter(group %in% input$groups) %>%
          pull(country_name)

        custom_df_countries <- NULL
        if (input$create_custom_grps == TRUE) {
          custom_df_countries <- custom_grps_df()$Countries[custom_grps_df()$Grp %in% input$groups &
                                                              custom_grps_df()$Countries %in% input$countries]
        }

        if (is.null(input$groups)) {
          return(input$countries)
        } else if (
          all(
            unique(input$countries) %in%
            unique(c(group_list_countries, custom_df_countries))
          )
        ) {
          return(input$groups)
        } else {
          return(input$countries)
        }
      }
    )

    ### Indicators with low variance -------------------------------------------
    low_variance_indicators <- eventReactive(
      input$select,
      {
        app_data$global_data %>%
          low_variance(
            base_country(),
            app_data$country_list,
            input$countries,
            vars(),
            app_data$variable_names
          )
      }
    )

    low_variance_indicators_dyn <- eventReactive(
      input$select,
      {
        app_data$global_data_dyn %>%
          low_variance_dyn(
            base_country(),
            app_data$country_list,
            input$countries,
            vars(),
            app_data$variable_names
          )
      }
    )

    data_avg <- eventReactive(
      input$select,
      {
        static_avg_data <- app_data$global_data %>% select(-matches("_avg"))
        vars_static_avg_data <- names(static_avg_data)[6:length(static_avg_data)]

        static_avg <- compute_family_average_app(
          static_avg_data, vars_static_avg_data, "static", app_data$db_variables,
          base_country(), input$countries
        )

        static_avg <- static_avg %>% select(-matches('NA'))
        static_avg_data <- static_avg_data %>% left_join(., static_avg, by = 'country_code')

        static_avg_data %>%
          def_quantiles(
            base_country(),
            app_data$country_list,
            input$countries,
            app_data$vars_all,
            app_data$variable_names,
            input$threshold
          )
      }
    )

    data <- eventReactive(
      input$select,
      {
        app_data$global_data %>%
          def_quantiles(
            base_country(),
            app_data$country_list,
            input$countries,
            app_data$vars_all,
            app_data$variable_names,
            input$threshold
          )
      }
    )

    data_dyn_avg <- eventReactive(
      input$select,
      {
        dynamic_avg_data <- app_data$global_data_dyn %>%
          select(-matches("_avg")) %>%
          filter(year %% 2 == 0)

        vars_dynamic_avg_data <- names(dynamic_avg_data)[6:length(dynamic_avg_data)]

        dynamic_avg <- compute_family_average_app(
          dynamic_avg_data, vars_dynamic_avg_data, "dynamic", app_data$db_variables,
          base_country(), input$countries
        )

        dynamic_avg <- dynamic_avg %>%
          select(-matches('NA')) %>%
          select(-matches("vars_other_avg"))

        dynamic_avg_data <- app_data$global_data_dyn %>%
          select(-matches("_avg")) %>%
          left_join(., dynamic_avg, by = c('country_code', 'year'))

        dynamic_avg_data %>%
          def_quantiles_dyn(
            base_country(),
            app_data$country_list,
            input$countries,
            app_data$vars_all,
            app_data$variable_names,
            input$threshold
          )
      }
    )

    data_dyn <- eventReactive(
      input$select,
      {
        app_data$global_data_dyn %>%
          def_quantiles_dyn(
            base_country(),
            app_data$country_list,
            input$countries,
            app_data$vars_all,
            app_data$variable_names,
            input$threshold
          )
      }
    )

    data_family <- eventReactive(
      input$select,
      {
        family_data(
          app_data$global_data,
          base_country(),
          app_data$variable_names,
          input$countries
        ) %>%
          def_quantiles(
            base_country(),
            app_data$country_list,
            input$countries,
            app_data$vars_family,
            app_data$family_names,
            input$threshold
          )
      }
    )

    data_family_dyn <- eventReactive(
      input$select,
      {
        family_data_dyn(
          app_data$global_data_dyn,
          base_country(),
          app_data$variable_names
        ) %>%
          def_quantiles_dyn(
            base_country(),
            app_data$country_list,
            input$countries,
            app_data$vars_family,
            app_data$family_names,
            input$threshold
          )
      }
    )

    ## Make sure only valid groups are chosen ----------------------------------
    observeEvent(
      input$country,
      {
        ## updating family at this point overwrites the update made once the user loads the input file
        if (nrow(saved_inputs_df()) > 0 & input$load_inputs == 1) {
          sel_family <- saved_inputs_df()$family
        } else {
          sel_family <- NULL
        }

        valid_vars <-
          app_data$ctf_long %>%
          filter(country_name == input$country, !is.na(value)) %>%
          select(family_name) %>%
          unique() %>%
          unlist() %>%
          unname()

        updatePickerInput(
          session,
          "family",
          choices = c("Overview", intersect(names(app_data$variable_list), valid_vars)),
          selected = sel_family
        )
      },
      ignoreNULL = FALSE
    )

    ## Benchmark plot ============================================================
    ## custom_df dataset will be used here if the groups in it are part of the benchmark
    ## median groups and its countries are selected
    custom_df <- shiny::eventReactive(input$select, {
      if (input$create_custom_grps == TRUE) {
        custom_df <- custom_grps_df()[custom_grps_df()$Grp %in% input$benchmark_median &
                                        custom_grps_df()$Countries %in% input$countries, ]
      } else {
        custom_df <- NULL
      }
    })

    output$plot <-
      renderPlotly({
        tryCatch({
          if (length(input$countries) >= 10) {
            input$select

            isolate(
              if (input$family == "Overview") {
                missing_variables <-
                  app_data$global_data %>%
                  missing_var(base_country(), app_data$country_list, input$countries, app_data$vars_all, app_data$variable_names)

                low_variance_variables <-
                  low_variance_indicators() %>%
                  data.frame() %>%
                  rename("variable" = ".") %>%
                  left_join(app_data$variable_names %>% select(variable, var_name), by = "variable") %>%
                  .$var_name

                missing_variables <- c(missing_variables, low_variance_variables)

                data_family() %>%
                  left_join(., app_data$family_order, by = c('var_name' = 'family_name')) %>%
                  arrange(family_order, country_name) %>%
                  static_plot(
                    base_country(), input$family, input$rank,
                    dots = input$benchmark_dots, group_median = input$benchmark_median,
                    custom_df = custom_df(), threshold = input$threshold, preset_order = input$preset_order,
                    db_variables = app_data$db_variables, family_order = app_data$family_order,
                    ctf_long = app_data$ctf_long
                  ) %>%
                  interactive_plot(input$family, plotly_remove_buttons, "static")
              } else {
                missing_variables <-
                  app_data$global_data %>%
                  missing_var(base_country(), app_data$country_list, input$countries, vars(), app_data$variable_names)

                low_variance_variables <-
                  low_variance_indicators() %>%
                  data.frame() %>%
                  rename("variable" = ".") %>%
                  left_join(app_data$variable_names %>% select(variable, var_name), by = "variable") %>%
                  .$var_name

                missing_variables <- c(missing_variables, low_variance_variables)

                data_avg() %>%
                  filter(variable %in% vars()) %>%
                  static_plot(
                    base_country(), input$family, input$rank,
                    dots = input$benchmark_dots, group_median = input$benchmark_median,
                    custom_df = custom_df(), threshold = input$threshold, preset_order = input$preset_order,
                    db_variables = app_data$db_variables, family_order = app_data$family_order,
                    ctf_long = app_data$ctf_long
                  ) %>%
                  interactive_plot(input$family, plotly_remove_buttons, "static")
              }
            )
          }
        }, error = function(e) {
          showNotification(
            'Data is missing for the selected base country or countries for the given indicator. Please try a different selection.', '',
            type = "message", duration = 30)
          return()
        })
      }) %>%
      bindCache(input$country, input$groups, input$family, input$benchmark_median,
                input$rank, input$benchmark_dots, input$preset_order, input$create_custom_grps,
                input$show_dynamic_plot, input$threshold, input$countries) %>%
      bindEvent(input$select)

    output$plot_notes <- renderUI({
      if (length(input$countries) >= 10) {
        input$select

        isolate(
          if (input$family == "Overview") {
            missing_variables <-
              app_data$global_data %>%
              missing_var(base_country(), app_data$country_list, input$countries, app_data$vars_all, app_data$variable_names)

            low_variance_variables <-
              low_variance_indicators() %>%
              data.frame() %>%
              rename("variable" = ".") %>%
              left_join(app_data$variable_names %>% select(variable, var_name), by = "variable") %>%
              .$var_name

            missing_variables <- c(missing_variables, low_variance_variables)

            plot_notes_function(base_country(), note_compare(), input$family, missing_variables, "static", custom_df = custom_df())
          } else {
            missing_variables <-
              app_data$global_data %>%
              missing_var(base_country(), app_data$country_list, input$countries, vars(), app_data$variable_names)

            low_variance_variables <-
              low_variance_indicators() %>%
              data.frame() %>%
              rename("variable" = ".") %>%
              left_join(app_data$variable_names %>% select(variable, var_name), by = "variable") %>%
              .$var_name

            missing_variables <- c(missing_variables, low_variance_variables)
            missing_variables <- missing_variables[!grepl("_avg", missing_variables)]

            plot_notes_function(base_country(), note_compare(), input$family, missing_variables, "static", custom_df = custom_df())
          }
        )
      }
    })

    ## Dynamic benchmark plot  ============================================================
    shiny::observeEvent(
      list(input$country, input$groups, input$family, input$rank, input$benchmark_dots,
           input$create_custom_grps, input$threshold, input$preset_order, input$countries), {
        if (length(input$country) == 1) {
          shinyWidgets::updateMaterialSwitch(session = session, inputId = "show_dynamic_plot", value = FALSE)
        }
      })

    output$dynamic_benchmark_plot <-
      renderPlotly({
        tryCatch({
          validate(need(length(input$country) == 1, 'Dynamic Benchmarking is available only when One base Country is selected'))
          validate(need(!(input$family %in% app_data$family_order$family_name[app_data$family_order$Benchmark_dynamic_indicator == "No"]), " No Dynamic Benchmarking Plot available for this family."))

          if (length(input$countries) >= 10 && length(input$country) == 1) {
            isolate(
              if (input$family == "Overview") {
                missing_variables <-
                  app_data$global_data_dyn %>%
                  missing_var_dyn(base_country()[1], app_data$country_list, input$countries, app_data$vars_all, app_data$variable_names)

                low_variance_variables <-
                  low_variance_indicators_dyn() %>%
                  data.frame() %>%
                  rename("variable" = ".") %>%
                  left_join(app_data$variable_names %>% select(variable, var_name), by = "variable") %>%
                  .$var_name

                missing_variables <- c(missing_variables, low_variance_variables)

                data_dyn_avg() %>%
                  filter(str_detect(variable, "_avg")) %>%
                  left_join(., app_data$family_order, by = 'family_name') %>%
                  filter(Benchmark_dynamic_family_aggregate != 'No') %>%
                  static_plot_dyn(
                    base_country(), input$family, input$rank,
                    dots = input$benchmark_dots, group_median = input$benchmark_median,
                    custom_df = custom_df(), threshold = input$threshold, preset_order = input$preset_order,
                    db_variables = app_data$db_variables, ctf_long_dyn = app_data$ctf_long_dyn
                  ) %>%
                  interactive_plot(input$family, plotly_remove_buttons, "dynamic")
              } else {
                missing_variables <-
                  app_data$global_data_dyn %>%
                  missing_var_dyn(base_country(), app_data$country_list, input$countries, vars(), app_data$variable_names)

                low_variance_variables <-
                  low_variance_indicators_dyn() %>%
                  data.frame() %>%
                  rename("variable" = ".") %>%
                  left_join(app_data$variable_names %>% select(variable, var_name), by = "variable") %>%
                  .$var_name

                missing_variables <- c(missing_variables, low_variance_variables)

                plot_data <- data_dyn_avg()

                plot_data_1 <- plot_data %>%
                  filter(str_detect(variable, "_avg")) %>%
                  left_join(., app_data$family_order, by = 'family_name') %>%
                  filter(Benchmark_dynamic_family_aggregate != 'No')

                plot_data_2 <- plot_data %>%
                  filter(!str_detect(variable, "_avg")) %>%
                  left_join(., app_data$family_order, by = 'family_name')

                plot_data <- bind_rows(plot_data_1, plot_data_2)

                plot_data %>%
                  filter(variable %in% vars()) %>%
                  static_plot_dyn(
                    base_country(), input$family, input$rank,
                    dots = input$benchmark_dots, group_median = input$benchmark_median,
                    custom_df = custom_df(), threshold = input$threshold, preset_order = input$preset_order,
                    db_variables = app_data$db_variables, ctf_long_dyn = app_data$ctf_long_dyn
                  ) %>%
                  interactive_plot(input$family, plotly_remove_buttons, "dynamic")
              }
            )
          }
        }, error = function(e) {
          showNotification(' Data is insufficient for the selected base country. No Dynamic Plot was generated', '', type = "message", duration = 10)
          return()
        })
      }) %>%
      bindCache(input$country, input$groups, input$family, input$benchmark_median,
                input$rank, input$benchmark_dots, input$preset_order, input$create_custom_grps,
                input$show_dynamic_plot, input$threshold, input$countries) %>%
      bindEvent(input$select)

    # Definitions ===========================================================================
    output$definition <-
      renderTable({
        shiny::req(input$family)

        variables <- app_data$db_variables %>%
          filter(var_level == "indicator" & benchmarked_ctf == 'Yes' & family_var != 'vars_other')

        if (input$family != "Overview") {
          variables <- variables %>% filter(family_name == input$family)
        }

        variables %>%
          select(Indicator = var_name, Family = family_name, Description = description, Source = source)
      })

    ## Save inputs to be loaded the next time --------------------------------------------------------
    cliar_inputs <- eventReactive(input$select, {
      cliar_inputs <- data.frame(
        country = input$country,
        groups = paste(c(input$groups), collapse = ";"),
        family = input$family,
        benchmark_median = paste(c(input$benchmark_median), collapse = ";"),
        benchmark_dots = input$benchmark_dots,
        rank = input$rank,
        threshold = input$threshold,
        worst_to_best_order = input$preset_order,
        comparison_countries = paste(c(input$countries), collapse = ";"),
        create_custom_groups = input$create_custom_grps
      )

      if (input$create_custom_grps == TRUE) {
        cliar_inputs$no_custom_grps <- input$custom_grps_count

        for (i in 1:input$custom_grps_count) {
          cliar_inputs[, paste("custom_grps_names", i, sep = "_")] <- input[[paste("custom_grps_names", i, sep = "_")]]
          cliar_inputs[, paste("custom_grps_countries", i, sep = "_")] <-
            paste(input[[paste("custom_grps_countries", i, sep = "_")]], collapse = ";")
        }
      }

      return(cliar_inputs)
    })

    # When 'apply selection' button is clicked, show the save button
    shiny::observeEvent(input$select, {
      shinyjs::show("download_data_1")
      shinyjs::enable("download_data_1")
    })

    output$save_inputs <- downloadHandler(
      filename = function() {
        paste("cliar_inputs.rds")
      },
      content = function(file) {
        saveRDS(cliar_inputs(), file)
      })

    observeEvent(input$family, {
      if (input$family == "SOE Corporate Governance" || input$family == "Labor and Social Protection Institutions")
        shinyjs::hide("download_data_1")
      else
        shinyjs::show("download_data_1")
    })

    download_data_1 <- eventReactive(input$select, {
      data1 <- data_family() %>% filter(country_name == base_country())
      # data2 was `data()` in the original -- see MIGRATION_GUIDE.md 4d-1 for why that was a
      # name collision with an unrelated reactive, and why data_avg() is what was actually meant.
      data2 <- data_avg() %>% filter(country_name == base_country())
      data3 <- data_family_dyn() %>% filter(country_name == base_country())
      data4 <- data_dyn_avg() %>% filter(country_name == base_country()) %>% filter(variable != 'wdi_nygdppcapppkd')

      list(data1 = data1, data2 = data2, data3 = data3, data4 = data4)
    })

    output$download_data_1 <- downloadHandler(
      filename = function() {
        paste0("CTF-plot-data.xlsx")
      },
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Compiling Data")
        on.exit(remove_modal_spinner())

        data <- download_data_1()

        data_frame1 <- data$data1
        data_frame2 <- data$data2
        data_frame3 <- data$data3
        data_frame4 <- data$data4

        wb <- createWorkbook()

        sheet1 <- addWorksheet(wb, "Static Overview")
        sheet2 <- addWorksheet(wb, "Static Family")
        sheet3 <- addWorksheet(wb, "Dynamic Overview")
        sheet4 <- addWorksheet(wb, "Dynamic Family")

        writeData(wb, sheet1, data_frame1, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
        writeData(wb, sheet2, data_frame2, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
        writeData(wb, sheet3, data_frame3, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
        writeData(wb, sheet4, data_frame4, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)

        saveWorkbook(wb, file)
      })

    list(
      base_country    = base_country,
      # Live, ungated selection -- server.R:1660-1677 et al synced other tabs'
      # country pickers immediately as the user picked, not gated behind
      # "Apply selection" the way base_country() is. Consuming modules use
      # this one for that live sync and base_country() for anything that
      # should wait for Apply, matching the original's two distinct mechanisms.
      country         = reactive(input$country),
      select_trigger  = reactive(input$select),
      countries       = reactive(input$countries),
      groups          = reactive(input$groups),
      family          = reactive(input$family),
      threshold       = reactive(input$threshold),
      rank            = reactive(input$rank),
      benchmark_dots  = reactive(input$benchmark_dots),
      preset_order    = reactive(input$preset_order),
      benchmark_median = reactive(input$benchmark_median),
      create_custom_grps = reactive(input$create_custom_grps),
      # Two genuinely different reactives -- custom_grps_df is the raw
      # user-entered Category/Grp/Countries table (what mod_country_comparison,
      # mod_bivariate and mod_trends need for their own custom_df_bar/
      # custom_df_trend/high_group derivations); custom_df is specifically
      # filtered to the benchmark plot's own group-median selection
      # (server.R:1314-1323, used again in mod_reports' PPT export).
      custom_grps_df  = custom_grps_df,
      custom_df       = custom_df,
      data_avg        = data_avg,
      data            = data,
      data_dyn_avg    = data_dyn_avg,
      data_dyn        = data_dyn,
      data_family     = data_family,
      data_family_dyn = data_family_dyn
    )
  })
}
