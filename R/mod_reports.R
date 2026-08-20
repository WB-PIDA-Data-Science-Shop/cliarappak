#' reports module server
#'
#' No UI of its own -- the download buttons it serves
#' (`report`/`advreport`/`pptreport`/`download_Coverage`) live inside
#' [mod_benchmark_ui()], per the original app's layout. Renders
#' `inst/rmd/report.Rmd` / `inst/rmd/coverage-report.Rmd` via `app_sys()`
#' rather than the working-directory-relative paths the original script app
#' used.
#'
#' Deliberate departure from `server.R:2571,2631,2692`'s
#' `file.copy("www/", tmp_dir, recursive = TRUE)` calls: dropped, per
#' MIGRATION_GUIDE.md's Phase 3 plan -- unnecessary now that `report.Rmd`'s
#' own `include_graphics()` calls resolve via `system.file()` instead of a
#' relative `www/` path.
#'
#' @param id a unique identifier for this module. Must match the `id` passed
#'   to [mod_benchmark_server()], since it renders into that module's
#'   `report`/`advreport`/`pptreport`/`download_Coverage` download buttons.
#' @param bench Named list of reactives returned by [mod_benchmark_server()].
#' @param app_data Shared data list from [build_app_data()].
#'
#' @return NULL; called for its side effects.
#' @export
mod_reports_server <- function(id, bench, app_data) {
  moduleServer(id, function(input, output, session) {

    output$report <- downloadHandler(
      filename = reactive(paste0("CLIAR-benchmarking-", bench$base_country(), ".docx")),
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Compiling report")
        on.exit(remove_modal_spinner())

        tmp_dir <- tempdir()
        tempReport <- file.path(tmp_dir, "report.Rmd")
        file.copy(app_sys("rmd/report.Rmd"), tempReport, overwrite = TRUE)
        file.copy(app_sys("app/www/template.docx"), file.path(tmp_dir, "template.docx"), overwrite = TRUE)

        params <- list(
          base_country = bench$base_country(),
          comparison_countries = bench$countries(),
          data = bench$data_avg(),
          wb_country_list = app_data$country_list,
          family_data = bench$data_family(),
          data_dyn = bench$data_dyn(),
          data_dyn_avg = bench$data_dyn_avg(),
          family_data_dyn = bench$data_family_dyn(),
          rank = bench$rank(),
          definitions = app_data$definitions,
          variable_names = app_data$variable_names,
          dots = bench$benchmark_dots(),
          group_median = bench$benchmark_median(),
          threshold = bench$threshold(),
          family_order = app_data$family_order,
          global_data = app_data$global_data,
          download_opt = FALSE,
          compiled_indicators = app_data$raw_data,
          db_variables = app_data$db_variables,
          ctf_long = app_data$ctf_long, ctf_long_dyn = app_data$ctf_long_dyn
        )

        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = asNamespace("cliarappak")),
          knit_root_dir = getwd()
        )
      }
    )

    #Advanced Report
    output$advreport <- downloadHandler(
      filename = reactive(paste0("CLIAR-benchmarking-Advanced-Report-", bench$base_country(), ".docx")),
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Compiling report")
        on.exit(remove_modal_spinner())

        tmp_dir <- tempdir()
        tempReport <- file.path(tmp_dir, "report.Rmd")
        file.copy(app_sys("rmd/report.Rmd"), tempReport, overwrite = TRUE)
        file.copy(app_sys("app/www/template.docx"), file.path(tmp_dir, "template.docx"), overwrite = TRUE)

        params <- list(
          base_country = bench$base_country(),
          comparison_countries = bench$countries(),
          data = bench$data_avg(),
          wb_country_list = app_data$country_list,
          family_data = bench$data_family(),
          data_dyn = bench$data_dyn(),
          data_dyn_avg = bench$data_dyn_avg(),
          family_data_dyn = bench$data_family_dyn(),
          rank = bench$rank(),
          definitions = app_data$definitions,
          variable_names = app_data$variable_names,
          dots = bench$benchmark_dots(),
          group_median = bench$benchmark_median(),
          threshold = bench$threshold(),
          family_order = app_data$family_order,
          global_data = app_data$global_data,
          download_opt = TRUE,
          compiled_indicators = app_data$raw_data,
          db_variables = app_data$db_variables,
          ctf_long = app_data$ctf_long, ctf_long_dyn = app_data$ctf_long_dyn
        )

        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = asNamespace("cliarappak")),
          knit_root_dir = getwd()
        )
      }
    )

    #Coverage Report ===============
    output$download_Coverage <- downloadHandler(
      filename = reactive(paste0("Missing_data-", bench$base_country(), ".docx")),
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Compiling report")
        on.exit(remove_modal_spinner())

        tmp_dir <- tempdir()
        tempReport <- file.path(tmp_dir, "coverage-report.Rmd")
        file.copy(app_sys("rmd/coverage-report.Rmd"), tempReport, overwrite = TRUE)

        # Computed lazily -- only worth the cost when this specific report is
        # actually requested, not at every session's startup via build_app_data().
        # See prepare_app_data_coverage() for why this isn't a static file.
        year_ctf_dynamic <- prepare_app_data_coverage(app_data$raw_data, cliaretl::db_variables)

        params <- list(
          ctf_static_long = app_data$ctf_long %>%
            left_join(
              app_data$db_variables %>%
                select(variable, var_name, family_var, family_name),
              by = "variable"
            ),
          ctf_dynamic = year_ctf_dynamic,
          base_country = bench$base_country()
        )

        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = asNamespace("cliarappak")),
          knit_root_dir = getwd()
        )
      }
    )

    # PPT Report ================================================================================
    output$pptreport <- downloadHandler(
      filename = reactive(paste0("CLIAR-PPT-", bench$base_country(), ".pptx")),
      content = function(file) {
        show_modal_spinner(color = "#17a2b8", text = "Compiling report")
        on.exit(remove_modal_spinner())

        ppt <- read_pptx(app_sys("app/www/CLIAR_template.pptx"))

        if (bench$create_custom_grps() == TRUE) {
          custom_df <- bench$custom_grps_df()[bench$custom_grps_df()$Grp %in% bench$benchmark_median() &
                                                bench$custom_grps_df()$Countries %in% bench$countries(), ]
        } else {
          custom_df <- NULL
        }

        plot1 <- bench$data_family() %>%
          left_join(., app_data$family_order, by = c('var_name' = 'family_name')) %>%
          arrange(country_name, family_order) %>%
          static_plot(
            bench$base_country(),
            "Country overview",
            rank = bench$rank(),
            group_median = bench$benchmark_median(),
            dots = bench$benchmark_dots(),
            custom_df = custom_df,
            title = FALSE,
            threshold = bench$threshold(),
            report = TRUE,
            db_variables = app_data$db_variables, family_order = app_data$family_order,
            ctf_long = app_data$ctf_long
          )

        plot2 <- bench$data_dyn_avg() %>%
          filter(str_detect(variable, "_avg")) %>%
          static_plot_dyn(
            bench$base_country()[1],
            "Country overview",
            bench$rank(),
            dots = bench$benchmark_dots(),
            group_median = bench$benchmark_median(),
            custom_df = custom_df,
            threshold = bench$threshold(),
            title = FALSE,
            db_variables = app_data$db_variables, ctf_long_dyn = app_data$ctf_long_dyn
          )
        plot1 <- dml(ggobj = plot1)
        plot2 <- dml(ggobj = plot2)

        properties <- fp_text(color = "black", font.size = 20, bold = FALSE)
        text_1 <- ftext(paste0("Base Country : ", bench$country()), properties)
        text_2 <- ftext(paste0("Comparison Countries : ", paste(c(bench$countries()), collapse = ", ")), properties)

        ppt <- ppt %>%
          on_slide(index = 8) %>%
          ph_with(value = fpar(text_1), ph_location(left = 0.5, width = 12, top = 1.3, bg = "transparent")) %>%
          ph_with(value = fpar(text_2), ph_location(left = 0.5, width = 12, top = 1.8, bg = "transparent"))

        ppt <- ppt %>%
          on_slide(index = 9) %>%
          ph_with(value = plot1, location = ph_location(
            left = 1.5, top = 1.2,
            width = 10.04, height = 4.67, bg = "transparent"
          ))

        slide_index <- 10

        family_n <- app_data$db_variables %>%
          distinct(family_name) %>%
          filter(!is.na(family_name)) %>%
          pull(family_name) %>%
          as.list()

        for (fam_n in app_data$family_order$family_name) {
          if (fam_n %in% family_n) {
            fam_variable_names <- app_data$variable_names %>%
              filter(family_name == fam_n) %>%
              pull(variable) %>%
              unique()

            plt_f <- bench$data_avg() %>%
              filter(variable %in% fam_variable_names) %>%
              static_plot(
                bench$base_country(),
                fam_n,
                bench$rank(),
                dots = bench$benchmark_dots(),
                group_median = bench$benchmark_median(),
                custom_df = bench$custom_df(),
                threshold = bench$threshold(),
                preset_order = bench$preset_order(),
                title = FALSE,
                report = TRUE,
                db_variables = app_data$db_variables, family_order = app_data$family_order,
                ctf_long = app_data$ctf_long
              )

            plt_f <- dml(ggobj = plt_f)

            ppt <- ppt %>%
              add_slide(master = "Custom Design") %>%
              on_slide(index = slide_index) %>%
              ph_with(value = fam_n, location = ph_location(left = 1, top = 0.4, width = 12)) %>%
              ph_with(value = plt_f, location = ph_location(
                left = 1.5, top = 1.2,
                width = 10.04, height = 4.67, bg = "transparent"
              ))

            slide_index <- slide_index + 1
          }
        }

        ppt <- ppt %>%
          add_slide(master = "Custom Design") %>%
          on_slide(index = slide_index) %>%
          ph_with(value = "Dynamic Benchmarking : Overview", location = ph_location(left = 1, top = 0.4, width = 12)) %>%
          ph_with(value = plot2, location = ph_location(
            left = 1.5, top = 1.2,
            width = 10.04, height = 4.67, bg = "transparent"
          ))

        print(ppt, file)
      }
    )
  })
}
