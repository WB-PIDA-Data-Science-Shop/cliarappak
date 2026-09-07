note_size <- 11
note_chars <- 200
color_groups <- colorRampPalette(c("#001f3f", "#60C2F7"))
color_countries <- colorRampPalette(c("grey20", "grey50"))


#============

plotly_remove_buttons <-
  c(
    "zoomIn2d",
    "zoomOut2d",
    "pan2d",
    "autoScale2d",
    "lasso2d",
    "select2d",
    "toggleSpikelines",
    "hoverClosest3d",
    "hoverClosestCartesian",
    "hoverCompareCartesian"
  )

# Benchmark plots ##############################################################

#' Static benchmarking dot/point plot (Country Benchmarking tab)
#'
#' Builds the ggplot behind the Country Benchmarking tab's main figure: one row
#' per indicator (or per institutional family, on the "Overview" tab), a
#' coloured point for the base country positioned at its closeness-to-frontier
#' value, and optionally the comparison-group median and individual comparison
#' countries. Piped into [interactive_plot()] to become the plotly the user
#' sees.
#'
#' @details
#' `static_plot()` works on the static (single latest period) dataset;
#' [static_plot_dyn()] is the year-by-year counterpart.
#'
#' The `data` argument is the output of [def_quantiles()] (or, for the Overview
#' tab, [family_data()] joined to `family_order`) -- it already carries the
#' `dtf`, `dtt`, `status` and `nrank` columns this function draws from. Indicator
#' ordering on the y-axis is either taken from `db_variables$rank_id`
#' (`preset_order = FALSE`) or from the base country's own ranking
#' (`preset_order = TRUE`). The word "Institutions" is stripped from y-axis
#' labels (issue #283).
#'
#' @param data Long tibble from [def_quantiles()] / [family_data()], filtered to
#'   the base and comparison countries, with `var_name`, `dtf`, `dtt`, `status`,
#'   `variable` columns.
#' @param base_country Character vector of base-country name(s).
#' @param tab_name Character. The institutional family being shown, or
#'   `"Overview"`. Drives y-axis ordering and a few layout tweaks.
#' @param rank Logical. `TRUE` plots percentile rank (`dtt`); `FALSE` plots the
#'   raw closeness-to-frontier value (`dtf`).
#' @param group_median Character vector of comparison-group names whose medians
#'   should be drawn, or `NULL`.
#' @param custom_df Data frame of user-defined comparison groups
#'   (`Grp` / `Countries` columns) from `mod_benchmark_server()`, or `NULL`.
#' @param title Logical. Whether to draw the plot title.
#' @param dots Logical. Whether to overlay individual comparison-country points.
#' @param note Character note to print under the plot, or `NULL`.
#' @param threshold One of `"Default"` or `"Terciles"` -- must match the value
#'   passed to [def_quantiles()]; sets the `"Weak"/"Emerging"/"Strong"` factor
#'   levels and legend cut points.
#' @param preset_order Logical. `TRUE` orders indicators by the base country's
#'   own value/rank; `FALSE` uses `db_variables$rank_id`.
#' @param report Logical. `TRUE` when rendering into the downloadable report
#'   (tweaks sizing/labels).
#' @param db_variables Indicator dictionary (`app_data$db_variables`) -- supplies
#'   `rank_id` for indicator ordering.
#' @param family_order Data frame mapping `family_name` to a numeric
#'   `family_order` (`app_data$family_order`); used only on the Overview tab.
#' @param ctf_long Long closeness-to-frontier table (`app_data$ctf_long`) used
#'   to position the group-median markers.
#'
#' @return A `ggplot` object. Returned via an explicit `return()`.
#'
#' @seealso [static_plot_dyn()], [interactive_plot()], [plot_notes_function()],
#'   [def_quantiles()].
#' @export
static_plot <-
  function(
    data,
    base_country,
    tab_name,
    rank,
    group_median = NULL,
    custom_df = NULL, ## New addition made by Shel in August 2023 to accomModate custom groups
    title = TRUE,
    dots = FALSE,
    note = NULL,
    threshold,
    preset_order = FALSE,
    report = FALSE,
    db_variables,
    family_order,
    ctf_long
  ) {
    #browser()

    data$var_name <- ifelse(
      grepl("Average", data$var_name, ignore.case = TRUE),
      toupper(data$var_name),
      data$var_name
    )

    if (threshold == "Default") {
      cutoff <- c(25, 50)
      custom_levels <- c(
        "Weak\n(bottom 25%)",
        "Emerging\n(25% - 50%)",
        "Strong\n(top 50%)"
      )
    } else if (threshold == "Terciles") {
      cutoff <- c(33, 66)
      custom_levels <- c(
        "Weak\n(bottom 33%)",
        "Emerging\n(33% - 66%)",
        "Strong\n(top 34%)"
      )
    }

    if (preset_order == TRUE) {
      ## temporary

      data <- data %>%
        group_by(country_name) %>%
        mutate(rank_id = rank(-dtf, ties.method = "max"))

      base_country_df <- data %>%
        filter(country_name == base_country[1])

      base_country_df$status <- factor(
        base_country_df$status,
        levels = custom_levels,
        ordered = TRUE
      )
      if (rank == FALSE) {
        base_country_df <- base_country_df[
          order(base_country_df$status, base_country_df$dtf),
        ]
      } else {
        base_country_df <- base_country_df[
          order(base_country_df$status, base_country_df$dtt),
        ]
      }

      unique_indicators = base_country_df %>%
        distinct(var_name) %>%
        pull(var_name)

      #Issue 283 - Remove Institutions keywork in y axis for plots
      data$var_name <- gsub("Institutions", "", data$var_name)
      unique_indicators <- gsub("Institutions", "", unique_indicators)

      data$var_name <-
        factor(
          data$var_name,
          levels = unique_indicators,
          ordered = TRUE
        )
    } else {
      data <- data %>%
        left_join(
          .,
          db_variables %>% select(variable, rank_id),
          by = "variable"
        )

      # ===================OVERVIEW
      if (tab_name == "Overview") {
        unique_indicators <- family_order %>%
          arrange(family_order) %>%
          pull(family_name)
      } else {
        unique_indicators <- data %>%
          distinct(var_name, rank_id) %>%
          arrange(desc(rank_id)) %>%
          pull(var_name)
      }

      #Change old name to new one for factoring
      #data$var_name[data$var_name == "Public Finance Institutions"] <- "Public Financial Management Institutions"

      #Issue 283 - Remove Institutions keyword in y axis for plots

      data$var_name <- gsub("Institutions", "", data$var_name)
      unique_indicators <- gsub("Institutions", "", unique_indicators)

      data$var_name <-
        factor(
          data$var_name,
          levels = unique_indicators,
          ordered = TRUE
        )
    }

    vars <-
      data %>%
      select(var_name) %>%
      unique() %>%
      unlist()
    if (cutoff[[1]] == 25) {
      colors <-
        c(
          "Weak\n(bottom 25%)" = "#D2222D",
          "Emerging\n(25% - 50%)" = "#FFBF00",
          "Strong\n(top 50%)" = "#238823"
        )
    } else if (cutoff[[1]] == 33) {
      colors <- c(
        "Weak\n(bottom 33%)" = "#D2222D",
        "Emerging\n(33% - 66%)" = "#FFBF00",
        "Strong\n(top 34%)" = "#238823"
      )
    }

    if (rank == FALSE) {
      x_lab <- "Closeness to frontier"

      # data <-
      #   data %>%
      #   mutate(
      #     var = dtf,
      #     text = paste(
      #       "Country:", country_name, "<br>",
      #       "Closeness to frontier:", round(dtf, 3)
      #     )
      #   )

      data <- data %>%
        group_by(dtf) %>%
        mutate(
          var = dtf,
          text = paste(
            "Closeness to frontier:",
            round(dtf, 3),
            "<br>",
            "Country:",
            paste(country_name, collapse = ", ")
          )
        ) %>%
        ungroup()
    } else {
      data <-
        data %>%
        group_by(variable, nrank) %>%
        mutate(
          q_cutoff1 = cutoff[[1]] / 100,
          q_cutoff2 = cutoff[[2]] / 100,
          var = dtt,
          text = paste(
            "Rank:",
            nrank,
            "<br>",
            "Country:",
            paste(country_name, collapse = ", "),
            "<br>",
            "Closeness to frontier:",
            round(dtf, 3)
          )
        ) %>%
        ungroup()

      x_lab <- "Rank"
    }

    if (report == FALSE) {
      aspect_ratio = 1.6 / 1
    } else {
      aspect_ratio = 1
    }

    #====================

    plot <-
      ggplot() +
      geom_segment(
        data = data,
        aes(
          y = var_name,
          yend = var_name,
          x = 0,
          xend = q_cutoff1
        ),
        color = "#e47a81",
        size = 2,
        alpha = .1
      ) +
      geom_vline(
        xintercept = 1,
        linetype = "dashed",
        color = colors["Advanced"],
        size = 1
      ) +
      geom_segment(
        data = data,
        aes(
          y = var_name,
          yend = var_name,
          x = q_cutoff1,
          xend = q_cutoff2
        ),
        color = "#ffd966",
        size = 2,
        alpha = .3
      ) +
      geom_segment(
        data = data,
        aes(
          y = var_name,
          yend = var_name,
          x = q_cutoff2,
          xend = 1
        ),
        color = "#8ec18e",
        size = 2,
        alpha = .3
      ) +
      theme_minimal() +
      theme(
        #aspect.ratio = aspect_ratio,
        legend.position = "top",
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 11),
        legend.box = "vertical",
        plot.caption = element_text(size = 8, hjust = 0),
        plot.caption.position = "plot"
      ) +
      labs(
        y = '',
        x = x_lab,
        fill = NULL,
        shape = NULL,
        caption = note
      ) +
      scale_fill_manual(
        values = colors
      )

    if (report == FALSE) {
      plot <- plot +

        scale_y_discrete(labels = function(x) str_wrap(x, width = 40))
      scale_y_discrete(labels = function(x) str_wrap(x, width = 35))
    }

    if (rank) {
      plot <-
        plot +
        scale_x_continuous(
          breaks = c(0, 0.5, 1),
          labels = c("Worst ranked", "Middle of ranking", "Top ranked")
        )
    }

    if (title) {
      plot <-
        plot +
        labs(title = paste0("<b>", tab_name, "</b>"))
    }

    if (dots) {
      plot <-
        plot +
        suppressWarnings(geom_point(
          data = data,
          aes(
            y = var_name,
            x = var,
            text = text
          ),
          shape = 21,
          size = 2,
          color = "gray30",
          fill = "white",
          alpha = .5
        ))
    }

    if (!is.null(group_median) & !rank) {
      median_data <-
        ctf_long %>%
        filter(
          var_name %in% vars,
          country_name %in% group_median
        ) %>%
        select(
          var_name,
          value,
          country_name
        )

      ## ------------------------------------------------------------------------------------
      ## This is how custom median groups are calculated, only if custom_df exists and is not null

      if (!is.null(custom_df)) {
        ## If any of the benchmark medians is a custom group
        if (any(group_median %in% custom_df$Grp)) {
          ## create a place holder that will hold the medians for all the groups
          # custom_grp_median_data <- list()

          ## create a vector of these groups
          selected_custom_grps <- unique(custom_df$Grp)

          custom_grp_median_data_func <- function(selected_custom_grp) {
            custom_df_per_group <- custom_df %>%
              filter(Grp == selected_custom_grp)

            ## calculate medians for each group
            custom_grp_median_data <-
              ctf_long %>%
              filter(
                var_name %in% vars,
                country_name %in% custom_df_per_group$Countries ## extract countries that fall in this group
              ) %>%
              mutate(
                country_name = unique(custom_df_per_group$Grp), ## the country name will be the
                ## name of the group.
                group = NA
              ) %>%
              unique %>%
              group_by(
                country_name,
                var_name
              ) %>%
              summarise(value = median(value, na.rm = TRUE)) %>%
              ungroup
          }

          custom_grp_median_data_df <- purrr::map_df(
            selected_custom_grps,
            custom_grp_median_data_func
          )

          median_data <- median_data %>%
            bind_rows(custom_grp_median_data_df)
        }
      }

      ## ------------------------------------------------------------------------------------

      if ("Comparison countries" %in% group_median) {
        countries <-
          ctf_long %>%
          filter(
            var_name %in% vars,
            country_name %in% data$country_name,
            !(country_name %in% unlist(base_country))
          ) %>%
          mutate(
            country_name = "Comparison countries",
            group = NA
          ) %>%
          unique %>%
          group_by(
            country_name,
            var_name
          ) %>%
          summarise(value = median(value, na.rm = TRUE)) %>%
          ungroup

        median_data <-
          median_data %>%
          bind_rows(countries)
      }
      # Custom Group Point needs to be excluded in the plot . Issue #294

      # plot <-
      #   plot +
      #   suppressWarnings(geom_point(
      #     data = median_data,
      #     aes(
      #       y = var_name,
      #       x = value,
      #       shape = country_name,
      #       text = paste(
      #         "Group:", country_name,"<br>",
      #         "Median closeness to frontier:", round(value, 3)
      #       )
      #     ),
      #     alpha = .5,
      #     color = "black",
      #     fill = "white",
      #     size = 3
      #   )) +
      #   scale_shape_manual(
      #     values = 22:25 #,
      #     #lab = NULL
      #   )
    }
    if (length(base_country) == 1) {
      #Alex

      plot <-
        plot +
        suppressWarnings(geom_point(
          data = data %>% filter(country_name %in% base_country),
          aes(
            y = var_name,
            x = var,
            fill = status,
            text = text
          ),
          shape = 21,
          size = 3,
          color = "gray0",
          show.legend = TRUE
        ))
    } else {
      if (report == TRUE) {
        data$status <- factor(
          data$status,
          levels = custom_levels,
          ordered = TRUE
        )

        plot <-
          plot +
          suppressWarnings(geom_point(
            data = data %>% filter(country_name %in% base_country),
            aes(
              y = var_name,
              x = var,
              shape = country_name,
              fill = status,
              text = text
            ),
            size = 3,
            color = "gray0",
            show.legend = TRUE
          )) +
          guides(fill = guide_legend(override.aes = list(shape = 21))) +
          scale_shape_manual(values = 21:25) +
          guides(
            colour = guide_legend(order = 1),
            shape = guide_legend(order = 2)
          )
      } else {
        plot <-
          plot +
          suppressWarnings(geom_point(
            data = data %>% filter(country_name %in% base_country),
            aes(
              y = var_name,
              x = var,
              shape = country_name,
              fill = status,
              text = text
            ),
            size = 3,
            color = "gray0",
            show.legend = TRUE
          )) +
          scale_shape_manual(values = 21:25)
      }

      #guides(fill=guide_legend(override.aes=list(shape=21)))#+
      #scale_shape_manual(values = 21:25)
      # scale_fill_manual(values= c("Weak\n(bottom 25%)" = "#D2222D",
      #                            "Emerging\n(25% - 50%)" = "#FFBF00",
      #                            "Strong\n(top 50%)" = "#238823"))+
      # guides(fill=guide_legend(override.aes=list(shape=21)))
    }

    return(plot)
  }

# Dynamic benchmark static plot ##############################################################

#' Dynamic (year-by-year) benchmarking plot
#'
#' The dynamic-dataset counterpart of [static_plot()]: same figure, but each
#' indicator is faceted/labelled by `indicator : year` so the user can see how
#' the base country's position moved over time. Consumes the output of
#' [def_quantiles_dyn()].
#'
#' @details
#' Indicators with only one year of data for the base country are dropped (a
#' trend needs at least two points). `ctf_long_dyn` is joined to `db_variables`
#' for display names and filtered to the same `indicator : year` combinations
#' present for the base country.
#'
#' @inheritParams static_plot
#' @param data Long tibble from [def_quantiles_dyn()] (carries a `year` column).
#' @param ctf_long_dyn Long dynamic closeness-to-frontier table
#'   (`app_data$ctf_long_dyn`) used for the group-median markers.
#'
#' @return A `ggplot` object.
#'
#' @seealso [static_plot()], [def_quantiles_dyn()], [interactive_plot()].
#' @export
static_plot_dyn <-
  function(
    data,
    base_country,
    tab_name,
    rank,
    group_median = NULL,
    custom_df = NULL, ## New addition made by Shel in August 2023 to accommodate custom groups
    title = TRUE,
    dots = FALSE,
    note = NULL,
    threshold,
    preset_order = FALSE,
    db_variables,
    ctf_long_dyn
  ) {
    if (threshold == "Default") {
      cutoff <- c(25, 50)
      custom_levels <- c(
        "Weak\n(bottom 25%)",
        "Emerging\n(25% - 50%)",
        "Strong\n(top 50%)"
      )
    } else if (threshold == "Terciles") {
      cutoff <- c(33, 66)
      custom_levels <- c(
        "Weak\n(bottom 33%)",
        "Emerging\n(33% - 66%)",
        "Strong\n(top 34%)"
      )
    }

    if (preset_order == TRUE) {
      ## temporary placeholder
      data$var_name <-
        factor(
          data$var_name,
          levels = sort(unique(data$var_name), decreasing = TRUE),
          ordered = TRUE
        )
    } else {
      data$var_name <-
        factor(
          data$var_name,
          levels = sort(unique(data$var_name), decreasing = TRUE),
          ordered = TRUE
        )
    }

    data <- data %>%
      rowwise() %>%
      mutate(var_name2 = paste(var_name, year, sep = " : ")) %>%
      arrange(var_name2)

    base_country_vars <- data %>%
      filter(country_name %in% base_country) %>%
      distinct(var_name2) %>%
      pull()

    data <- data %>%
      filter(var_name2 %in% base_country_vars) %>%
      ## if we only have one year worth of data for a particular indicator, drop it
      group_by(var_name) %>%
      mutate(counter = length(unique(year))) %>%
      filter(counter > 1) %>%
      select(-counter) %>%
      ungroup()

    ctf_long_dyn <- ctf_long_dyn %>%
      left_join(
        db_variables %>% select(variable, var_name),
        by = "variable"
      ) %>%
      rowwise() %>%
      mutate(var_name2 = paste(var_name, year, sep = " : ")) %>%
      arrange(var_name2) %>%
      filter(var_name2 %in% base_country_vars)

    vars <-
      data %>%
      select(var_name) %>%
      unique %>%
      unlist %>%
      unname

    if (cutoff[[1]] == 25) {
      colors <-
        c(
          "Weak\n(bottom 25%)" = "#D2222D",
          "Emerging\n(25% - 50%)" = "#FFBF00",
          "Strong\n(top 50%)" = "#238823"
        )
    } else if (cutoff[[1]] == 33) {
      colors <- c(
        "Weak\n(bottom 33%)" = "#D2222D",
        "Emerging\n(33% - 66%)" = "#FFBF00",
        "Strong\n(top 34%)" = "#238823"
      )
    }

    if (rank == FALSE) {
      y_lab <- "Closeness to frontier"

      data <-
        data %>%
        mutate(
          var = dtf,
          text = paste(
            "Country:",
            country_name,
            "<br>",
            "Year: ",
            year,
            "<br>",
            "Closeness to frontier:",
            round(dtf, 3)
          )
        )
    } else {
      data <-
        data %>%
        mutate(
          q_cutoff1 = cutoff[[1]] / 100,
          q_cutoff2 = cutoff[[2]] / 100,
          var = dtt,
          text = paste(
            "Country:",
            country_name,
            "<br>",
            "Year: ",
            year,
            "<br>",
            "Closeness to frontier:",
            round(dtf, 3),
            "<br>",
            "Rank:",
            nrank
          )
        )

      y_lab <- "Rank"
    }

    ## calculate the delta and the new facet labels that will contain it.

    data <- data %>%
      group_by(family_name, var_name) %>%
      mutate(
        n_countries_min = length(country_name[
          year == min(as.numeric(year), na.rm = TRUE)
        ]),
        n_countries_max = length(country_name[
          year == max(as.numeric(year), na.rm = TRUE)
        ])
      ) %>%
      ungroup() %>%
      group_by(country_name, var_name) %>%
      mutate(
        earliest_value_ctf = var[year == min(as.numeric(year), na.rm = TRUE)],
        latest_value_ctf = var[year == max(as.numeric(year), na.rm = TRUE)]
      ) %>%
      mutate(
        earliest_value_rank = nrank[
          year == min(as.numeric(year), na.rm = TRUE)
        ],
        latest_value_rank = nrank[year == max(as.numeric(year), na.rm = TRUE)]
      ) %>%
      ungroup() %>%
      rowwise() %>%
      mutate(delta = round((latest_value_ctf - earliest_value_ctf), 3)) %>%
      mutate(
        new_labels = ifelse(
          earliest_value_rank != latest_value_rank,
          paste0(
            var_name,
            "\n\n(Change in CTF: ",
            delta,
            ")",
            "\n(Change in Percentile Rank: ",
            "from ",
            earliest_value_rank,
            " out of ",
            n_countries_min,
            " to ",
            latest_value_rank,
            " out of ",
            n_countries_max,
            ")"
          ),
          paste0(
            var_name,
            "\n\n(Change in CTF: ",
            delta,
            ")",
            "\n(No significant change in Percentile Rank)"
          )
        )
      ) %>%
      ungroup()

    ## The year var should be character or factor
    data <- data %>%
      mutate(year = as.character(year))

    ## Percentile segments
    plot <-
      ggplot() +
      geom_segment(
        data = data,
        aes(
          x = year,
          xend = year,
          y = 0,
          yend = q_cutoff1
        ),
        color = "#e47a81",
        size = 2,
        alpha = .1
      ) +
      geom_segment(
        data = data,
        aes(
          x = year,
          xend = year,
          y = q_cutoff1,
          yend = q_cutoff2
        ),
        color = "#ffd966",
        size = 2,
        alpha = .3
      ) +
      geom_segment(
        data = data,
        aes(
          x = year,
          xend = year,
          y = q_cutoff2,
          yend = 1
        ),
        color = "#8ec18e",
        size = 2,
        alpha = .3
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        legend.box = "vertical",
        plot.caption = element_text(size = 8, hjust = 0),
        plot.caption.position = "plot"
      ) +
      labs(
        y = y_lab,
        x = NULL,
        fill = NULL,
        shape = NULL,
        caption = note
      ) +
      scale_fill_manual(
        values = colors
      )

    if (rank) {
      plot <-
        plot +
        scale_y_continuous(
          breaks = c(0, 0.5, 1),
          labels = c("Worst ranked", "Middle of ranking", "Top ranked")
        )
    }

    if (title) {
      plot <-
        plot +
        labs(title = paste0("<b>", tab_name, "</b>"))
    }

    if (dots) {
      plot <-
        plot +
        suppressWarnings(geom_point(
          data = data,
          aes(
            x = year,
            y = var,
            text = text
          ),
          shape = 21,
          size = 2,
          color = "gray30",
          fill = "white",
          alpha = .5
        ))
    }

    if (!is.null(group_median) & !rank) {
      median_data <-
        ctf_long_dyn %>%
        filter(
          var_name %in% vars,
          country_name %in% group_median
        ) %>%
        select(
          var_name,
          year,
          value,
          country_name
        )

      ## ------------------------------------------------------------------------------------
      ## This is how custom median groups are calculated, only if custom_df exists and is not null

      if (!is.null(custom_df)) {
        ## If any of the benchmark medians is a custom group
        if (any(group_median %in% custom_df$Grp)) {
          ## create a place holder that will hold the medians for all the groups
          # custom_grp_median_data <- list()
          #
          ## create a vector of these groups
          selected_custom_grps <- unique(custom_df$Grp)

          custom_grp_median_data_func <- function(selected_custom_grp) {
            custom_df_per_group <- custom_df %>%
              filter(Grp == selected_custom_grp)

            ## calculate medians for each group
            custom_grp_median_data <-
              ctf_long_dyn %>%
              filter(
                var_name %in% vars,
                country_name %in% custom_df_per_group$Countries ## extract countries that fall in this group
              ) %>%
              mutate(
                country_name = unique(custom_df_per_group$Grp), ## the country name will be the
                ## name of the group.
                group = NA
              ) %>%
              unique %>%
              group_by(
                country_name,
                year,
                var_name
              ) %>%
              mutate(value = median(value, na.rm = TRUE)) %>%
              distinct(var_name, year, value, country_name) %>%
              ungroup

            return(custom_grp_median_data)
          }

          custom_grp_median_data_df <- purrr::map_df(
            selected_custom_grps,
            custom_grp_median_data_func
          )

          ## and append this to median data generated for pre-determined groups
          median_data <- median_data %>%
            bind_rows(custom_grp_median_data_df)
        }
      }

      ## ------------------------------------------------------------------------------------

      if ("Comparison countries" %in% group_median) {
        countries <-
          ctf_long_dyn %>%
          filter(
            var_name %in% vars,
            country_name %in% data$country_name,
            country_name != base_country
          ) %>%
          mutate(
            country_name = "Comparison countries",
            group = NA
          ) %>%
          unique %>%
          group_by(
            country_name,
            year,
            var_name
          ) %>%
          mutate(value = median(value, na.rm = TRUE)) %>%
          distinct(country_name, year, var_name, value) %>%
          ungroup

        median_data <-
          median_data %>%
          bind_rows(countries)
      }

      plot <-
        plot +
        suppressWarnings(geom_point(
          data = median_data %>% ungroup() %>% filter(!is.na(value)),
          aes(
            y = value,
            x = as.character(year),
            shape = country_name,
            text = paste(
              "Group:",
              country_name,
              "<br>",
              "Year: ",
              year,
              "<br>",
              "Median closeness to frontier:",
              round(value, 3)
            )
          ),
          alpha = .5,
          color = "black",
          fill = "white",
          size = 2
        )) +
        scale_shape_manual(
          values = 22:25
        )
    }

    data$status <- factor(data$status, levels = custom_levels, ordered = TRUE)

    ## add base country
    plot <-
      plot +
      suppressWarnings(
        geom_point(
          data = data %>% filter(country_name == base_country),
          aes(
            y = var,
            x = year,
            fill = status,
            text = text
          ),
          size = 2,
          shape = 21,
          color = "gray0"
        )
      ) +
      geom_line(
        data = data %>% filter(country_name == base_country),
        aes(
          y = var,
          x = year,
          group = 1
        )
      )

    ## Facet the plot

    ### number of columns will depend on the number of variables
    n_col = ifelse(length(unique(data$var_name)) <= 3, 1, 2)

    # sc = ifelse(length(unique(data$var_name)) <= 6, "free_x", "fixed")
    sc = "free"

    ### instead of having the var name as the titles, we want to append the delta on it.
    ### Delta was calculated at the beginning before any plot was generated

    plot_titles_df <- data %>%
      filter(var_name %in% vars & country_name == base_country) %>%
      distinct(var_name, delta, new_labels)

    plot_titles <- unique(plot_titles_df$new_labels)
    names(plot_titles) <- unique(plot_titles_df$var_name)

    ### create the plot

    plot <- plot +
      facet_wrap(
        ~var_name,
        ncol = 2,
        labeller = labeller(var_name = plot_titles),
        shrink = FALSE,
        scales = sc
      ) +
      theme(
        strip.text = element_text(face = "bold", size = 8),
        panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(3, "lines")
      )

    ## fix facets
    # plot <- fixfacets(figure = plot, facets = names(plot_titles), domain_offset = 0.16)

    return(plot)
  }


#' Build the HTML notes block shown under a benchmarking plot
#'
#' Assembles the "Notes:" paragraph that appears beneath the Country
#' Benchmarking figure: who is being compared to whom, the membership of any
#' custom comparison groups, and the list of indicators that were excluded
#' because the base country lacks data or the indicator has low variance.
#'
#' @details
#' Indicator names that failed to resolve to a display label (a `NA` after the
#' dictionary join) are dropped rather than printed as the literal `"NA"`. For
#' `plot_type == "dynamic"` the function returns `NULL` -- the dynamic plot
#' carries its own annotations.
#'
#' @param y Character. Base-country name(s).
#' @param z Character vector. Comparison-country / group names.
#' @param tab_name Character. Institutional family, or `"Overview"` (adds a
#'   sentence about cluster-level aggregation).
#' @param miss_var Character vector of excluded-indicator display names --
#'   typically `c(missing_var(...), low_variance-resolved names)` from
#'   `mod_benchmark_server()`.
#' @param plot_type One of `"static"` or `"dynamic"`.
#' @param custom_df Custom-group data frame (`Grp` / `Countries`), or `NULL`.
#'
#' @return A `shiny::HTML` string, or `NULL` when `plot_type == "dynamic"`.
#'
#' @seealso [missing_var()], [low_variance()], [static_plot()].
#' @export
plot_notes_function <-
  function(y, z, tab_name, miss_var, plot_type, custom_df) {
    # Some indicator codes returned by missing_var()/low_variance() have no
    # matching row in variable_names (the code -> display-name dictionary),
    # so the left_join that resolves them to a human-readable name yields NA
    # -- without this, paste() below renders that NA as the literal string
    # "NA" in the notes text instead of just omitting the indicator.
    miss_var <- miss_var[!is.na(miss_var)]

    if (!is.null(custom_df)) {
      custom_grp_notes <- custom_df %>%
        group_by(Grp) %>%
        arrange(desc(Grp)) %>%
        mutate(
          note = paste0(Grp, " (", paste0(Countries, collapse = " , "), ")")
        ) %>%
        distinct(note) %>%
        pull() %>%
        paste0(., collapse = " ; ")

      custom_grp_notes <-
        paste(
          "<br>",
          str_wrap(custom_grp_notes, note_chars)
        )
    } else {
      custom_grp_notes <- ""
    }

    if (length(miss_var) > 0) {
      ## Shel added "<br>" to include line breaks in the notes
      notes <-
        paste0(
          "Notes:<br><br>",
          paste(y, collapse = ","),
          " compared to ",
          str_wrap(paste(z, collapse = ", "), note_chars),
          ".",
          custom_grp_notes,
          "<br><br>The following indicators are not considered because the base country (or countries) has no information or because of low variance: ",
          str_wrap(paste(miss_var, collapse = ", "), note_chars),
          "."
        )
    }

    if (length(miss_var) == 0) {
      notes <-
        paste0(
          "Notes:<br> ",

          str_wrap(
            paste0(
              y,
              " compared to ",
              str_wrap(paste(z, collapse = ", "), note_chars),
              "."
            ),
            note_chars
          ),
          custom_grp_notes
        )
    }

    if (tab_name == "Overview") {
      notes <-
        paste(
          notes,
          "<br><br>Cluster-level closeness to frontier is calculated by taking the average closeness to frontier for the estimated indicators within each cluster."
        )
    }

    if (plot_type == "dynamic") {
      notes = NULL
    }

    return(shiny::HTML(notes))
  }


#' Convert a benchmarking ggplot to an interactive plotly
#'
#' Takes a [static_plot()] / [static_plot_dyn()] ggplot and turns it into the
#' plotly widget rendered on the tab: sets a tab-appropriate pixel height,
#' trims the modebar, names the PNG export after the tab, fixes ggplotly's
#' mangled legend entries, and (dynamic only) moves the legend to a horizontal
#' strip.
#'
#' @param x A `ggplot` object from [static_plot()] or [static_plot_dyn()].
#' @param tab_name Character. Institutional family / `"Overview"` -- selects the
#'   plot height and the export filename.
#' @param buttons Character vector of plotly modebar button ids to remove
#'   (typically `plotly_remove_buttons`).
#' @param plot_type One of `"static"` or `"dynamic"`.
#'
#' @return A `plotly` htmlwidget.
#'
#' @seealso [static_plot()], [clean_plotly_legend()].
#' @export
interactive_plot <-
  function(x, tab_name, buttons, plot_type) {
    if (tab_name == 'Justice Institutions' & plot_type == 'dynamic') {
      plt_height = 3000
    } else if (plot_type == 'dynamic') {
      plt_height = 1200
    } else if (
      tab_name == 'Service Delivery Institutions' |
        tab_name == 'Justice Institutions'
    ) {
      plt_height = 1200
    } else if (tab_name == 'Overview') {
      plt_height = 900
    } else {
      plt_height = 750
    }

    x <- x +
      theme(
        legend.position = "top"
      )

    int_plot <- x %>%
      ggplotly(tooltip = "text", height = plt_height) %>%
      layout(
        margin = list(l = 50, r = 50, t = 150, b = 150) #,
      ) %>%
      config(
        modeBarButtonsToRemove = buttons,
        toImageButtonOptions = list(
          filename = paste0(tolower(stringr::str_replace_all(
            tab_name,
            "\\s",
            "_"
          ))),
          width = 1100,
          height = 1000
        )
      )

    if (plot_type == "dynamic") {
      int_plot <- int_plot %>%
        layout(
          legend = list(
            orientation = "h",
            xanchor = "center",
            x = 0.5
          )
        )
    }

    ## Solution to remove ",1" that appears on the legend
    ## https://stackoverflow.com/questions/49133395/strange-formatting-of-legend-in-ggplotly-in-r

    for (i in 1:length(int_plot$x$data)) {
      if (!is.null(int_plot$x$data[[i]]$name)) {
        int_plot$x$data[[i]]$name = gsub(
          "^\\(",
          "",
          str_split(int_plot$x$data[[i]]$name, ",")[[1]][1]
        )
      }
    }

    int_plot <- clean_plotly_legend(int_plot)

    # names_lst <- names(int_plot$x$layout)
    # names_lst <- names_lst[grep("yaxis",names_lst)]
    # if(length(names_lst)>2){
    #   nrows = ceiling(length(names_lst)/2)
    #   height_gap = (nrows-1)*0.1182804
    #   height_plt = (1-height_gap)/nrows
    #   height_start = 1
    #   for(i in seq(1,length(names_lst),1)){
    #     int_plot$x$layout[[names_lst[i]]][['domain']] <-c(max(0,height_start-height_plt),height_start)
    #     int_plot$x$layout$annotations[[i+1]][['y']] <- height_start
    #     if((i+1)%%2 ==1){
    #       height_start <-height_start-height_plt-0.1182804
    #     }
    #   }
    # }

    return(int_plot)
  }

# Maps #########################################################################

## Static map ===================================================================

#' Static choropleth world map for one indicator (World Map tab)
#'
#' Builds the ggplot choropleth behind the World Map tab: every country shaded
#' by its value (or closeness-to-frontier) for a single indicator, with the base
#' and comparison countries outlined. Piped into [interactive_map()].
#'
#' @param source One of `"raw"` (latest observed value, with the observation
#'   year in the tooltip) or `"ctf"` (2020-2024 closeness-to-frontier average).
#' @param var Character. Indicator code (the `variable` column) -- used to pick
#'   the `value_*` / `ctf_*` / `year_*` columns out of `spatial_data`.
#' @param title Character. Plot title (usually the indicator display name).
#' @param selected Currently-selected indicator display name (passed through for
#'   labelling).
#' @param base_country Character vector of base-country name(s) to outline.
#' @param comparison_countries Character vector of comparison-country names to
#'   outline.
#' @param spatial_data An `sf` polygon layer (`app_data$spatial_data`) carrying
#'   one `value_<var>` / `ctf_<var>` / `year_<var>` column set per indicator.
#'
#' @return A `ggplot` object.
#'
#' @seealso [interactive_map()], [check_spatial_data()].
#' @export
static_map <-
  function(source, var, title, selected, base_country, comparison_countries, spatial_data) {
    spatial_data <- spatial_data %>%
      st_cast("MULTIPOLYGON")

    if (source == "raw") {
      color <- paste0("value_", var)

      data <-
        spatial_data %>%
        mutate(
          text = paste0(
            "Latest value (",
            get(paste0("year_", var)),
            "): ",
            get(color) %>% round(3),
            "<br>",
            "Closeness to frontier (2020-2024): ",
            get(paste0("ctf_", var)) %>% round(3)
          )
        )

      plot <-
        data %>%
        ggplot() +
        geom_sf(
          aes(
            fill = as.numeric(get(color)),
            text = paste0(
              "<b>",
              country_name,
              "</b><br>",
              text
            )
          ),
          color = "black",
          size = 0.1
        ) +
        labs(title = paste0("<b>", title, "</b>")) +
        theme_void()
    } else if (source == "ctf") {
      color <- paste0("bin_", var)
      value <- paste0("value_", var)

      data <-
        spatial_data %>%
        mutate(
          text = paste0(
            "Closeness to frontier: ",
            get(paste0("ctf_", var)) %>% round(3)
          )
        )

      plot <-
        data %>%
        ggplot() +
        geom_sf(
          aes(
            fill = get(color),
            text = paste0(
              "<b>",
              country_name,
              "</b><br>",
              text
            )
          ),
          color = "black",
          size = 0.1
        ) +
        labs(title = paste0("<b>", title, "</b>")) +
        theme_void()
    }

    if (
      selected == "TRUE" &
        !is.null(base_country) &
        !is.null(comparison_countries)
    ) {
      plot <-
        plot +
        geom_sf(
          data = spatial_data %>%
            filter(!country_name %in% c(base_country, comparison_countries)),
          fill = "white"
        )
    }

    if (source == "raw") {
      plot <-
        plot +
        scale_fill_gradientn(
          colours = c(
            "#D55E00",
            "#DD7C00",
            "#E69F00",
            "#579E47",
            "#009E73"
          ),
          name = "Original Scale",
          na.value = "#808080"
        )
    } else if (source == "ctf") {
      plot <-
        plot +
        scale_fill_manual(
          name = "CTF",
          values = c(
            "0.0 - 0.2" = "#D55E00",
            "0.2 - 0.4" = "#DD7C00",
            "0.4 - 0.6" = "#E69F00",
            "0.6 - 0.8" = "#579E47",
            "0.8 - 1.0" = "#009E73",
            "Not available" = "#808080"
          ),
          na.value = "#808080",
          drop = FALSE
        )
    }

    return(plot)
  }


## Interactive map =============================================================

#' Convert a world-map ggplot to an interactive plotly
#'
#' Turns a [static_map()] ggplot into the plotly widget shown on the World Map
#' tab: hides the axes, adds the standard World Bank map disclaimer plus the
#' indicator definition/source/note as a footnote annotation, trims the modebar
#' and names the PNG export `<var>_map`.
#'
#' @param x A `ggplot` object from [static_map()].
#' @param var Character. Indicator code, used both to look up the definition row
#'   in `definitions` and to name the export file.
#' @param definitions Indicator dictionary (`app_data$db_variables`) with
#'   `variable`, `description`, `source` columns.
#' @param buttons Character vector of plotly modebar button ids to remove.
#' @param source One of `"raw"` or `"ctf"` (kept for parity with [static_map()];
#'   currently only affects commented-out legend styling).
#'
#' @return A `plotly` htmlwidget.
#'
#' @seealso [static_map()].
#' @export
interactive_map <-
  function(x, var, definitions, buttons, source) {
    def <-
      definitions %>%
      filter(variable == var)

    # if (source == "ctf") {
    #   leg_title <- "Closeness to\nfrontier"
    # }else{
    #   leg_title <-NULL
    # }

    x %>%
      ggplotly(tooltip = "text") %>%
      layout(
        # legend = list(
        #   title = list(text = paste("<b>", leg_title, "</b>")),
        #   y = 0.2
        # ),

        margin = list(t = 75, b = 200),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(
          x = 0,
          y = -0.8,
          text = HTML(
            paste(
              str_wrap(
                "<b>Disclaimer:</b> Country borders or names do not necessarily reflect the World Bank Group's official position.
                     This map is for illustrative purposes and does not imply the expression of any opinion on the part of the World Bank,
                     concerning the legal status of any country or territory or concerning the delimitation of frontiers or boundaries.",
                note_chars
              ),
              str_wrap(
                paste(
                  "<b>Definition:</b>",
                  def$description
                ),
                note_chars
              ),
              str_wrap(
                paste(
                  "<b>Source:</b>",
                  def$source
                ),
                note_chars
              ),

              str_wrap(
                "<b>Note:</b> The color illustrates the latest value of the indicator available for each country.The data presented here for CTF is obtained by taking the average of the indicator for the period 2019-2023 and for original indicator, it is latest datapoint available.",
                note_chars
              ),
              sep = "<br>"
            )
          ),
          showarrow = F,
          xref = 'paper',
          yref = 'paper',
          align = 'left',
          font = list(size = note_size)
        )
      ) %>%
      config(
        modeBarButtonsToRemove = buttons,
        toImageButtonOptions = list(
          filename = paste0(
            tolower(stringr::str_replace_all(var, "\\s", "_")),
            "_map"
          ),
          width = 1050,
          height = 675
        )
      )
  }

# Time series ###################################################################

#' Time-series plot for one indicator (Time Trends tab)
#'
#' Builds the interactive plotly line chart on the Time Trends tab: the base
#' country's value of one indicator year by year, overlaid with individual
#' comparison countries and comparison-group (or custom-group) medians. Unlike
#' the other plot builders this one returns plotly directly -- there is no
#' separate `interactive_*` step.
#'
#' @details
#' The x-range is clipped to the years the base country actually has data for.
#' Group medians are computed across the group's members for each year;
#' custom groups (from `custom_df`) are handled the same way. Colours are taken
#' from the three `colourInput()` controls on the tab.
#'
#' @param raw_data Wide indicator panel with a `Year` column
#'   (`app_data$raw_data`).
#' @param indicator Character. Indicator code (column name in `raw_data`).
#' @param indicator_name Character. Indicator display name (for the title and
#'   the `definitions` lookup).
#' @param base_country Character. Single base-country name.
#' @param comparison_countries Character vector of individual comparison-country
#'   names.
#' @param country_list Country-to-group lookup (`app_data$country_list`) used to
#'   resolve `groups` to their member countries.
#' @param groups Character vector of comparison-group names whose medians to
#'   draw.
#' @param definitions Indicator dictionary (`app_data$db_variables`).
#' @param custom_df Custom-group data frame (`Grp` / `Countries`), or `NULL`.
#' @param base_color,comp_color,groups_color Hex colour strings for the base
#'   country line, comparison-country lines, and group-median lines.
#'
#' @return A `plotly` htmlwidget.
#'
#' @seealso [trends_check_data()], [mod_trends_server()].
#' @export
trends_plot <- function(
  raw_data,
  indicator,
  indicator_name,
  base_country,
  comparison_countries,
  country_list,
  groups,
  definitions,
  custom_df = NULL,
  base_color,
  comp_color,
  groups_color
) {
  def <-
    definitions %>%
    filter(var_name == indicator_name)

  indicator_data <-
    raw_data %>%
    select(Year, country_name, all_of(indicator))

  years <-
    indicator_data %>%
    filter(
      country_name == base_country,
      !is.na(get(indicator))
    ) %>%
    summarise(
      min = min(Year, na.rm = TRUE),
      max = max(Year, na.rm = TRUE)
    )

  indicator_data <-
    indicator_data %>%
    filter(
      Year >= years$min,
      Year <= years$max
    )

  data_groups <-
    if (!is.null(groups)) {
      avg_df <- country_list %>%
        filter(group %in% groups)

      if (!is.null(custom_df) & any(groups %in% custom_df$Grp)) {
        avg_df <- avg_df %>%
          bind_rows(
            .,
            custom_df %>%
              rename(group = Grp, country_name = Countries)
          ) %>%
          select(-Category)
      }

      avg_df <- avg_df %>%
        inner_join(indicator_data) %>%
        group_by(Year, group) %>%
        summarise(
          across(
            all_of(indicator),
            ~ mean(., na.rm = TRUE)
          )
        ) %>%
        rename(country_name = group) %>%
        mutate(country_name = paste(country_name, "average"))

      avg_df <- avg_df %>%
        arrange(country_name, Year)
    } else {
      NULL
    }

  data <-
    indicator_data %>%
    filter(
      country_name == base_country |
        country_name %in% comparison_countries
    ) %>%
    bind_rows(data_groups) %>%
    mutate(
      alpha = ifelse(country_name == base_country, .8, .5),
      color = case_when(
        country_name == base_country ~ base_color,
        country_name %in% comparison_countries ~ comp_color,
        #This is how we assign country group color
        grepl("average", country_name) ~ groups_color,

        TRUE ~ '#000000' # Adding default black color
      ),
      legend_label = country_name
    ) %>%
    rename(Country = country_name) %>%
    mutate(Year = as.factor(Year))

  #==================PLOTTING: TIME TRENDS
  static_plot <-
    ggplot(
      data,
      aes(
        x = Year,
        y = get(indicator),
        color = Country,
        group = Country,
        alpha = alpha
      )
    ) +
    geom_line(aes(y = na.approx(get(indicator)), color = Country)) +
    geom_point(
      aes(
        text = paste(
          "Country:",
          Country,
          "<br>",
          "Year:",
          Year,
          "<br>",
          "Value:",
          get(indicator) %>% round(3)
        )
      ),
      #Alex change 3 to 1
      size = 3
    ) +
    #scale_alpha_identity() +
    theme_ipsum() +
    labs(
      x = "Year",
      y = "Indicator value",
      title = paste0("<b>", indicator_name, "</b>")
    ) +
    scale_color_manual(
      values = setNames(data$color, data$Country), # Map each Country to its color
      name = "Country" # Set the legend title
    ) +
    scale_alpha_identity() +
    theme(
      axis.text.x = element_text(angle = 90)
    )

  ggplotly(
    static_plot,
    tooltip = "text"
  ) %>%
    layout(
      legend = list(
        title = list(text = '<b>Country:</b>'),
        y = 0.5
      ),
      margin = list(l = 50, r = 150, t = 100, b = 300),
      annotations = list(
        x = 0,
        y = -0.5,
        text = HTML(
          paste(
            str_wrap(
              paste(
                "<b>Note:</b>,Data displayed in based on the original indicators<br>",
                "<b>Definition:</b>",
                def$description
              ),
              note_chars
            ),
            str_wrap(
              paste(
                "<b>Source:</b>",
                def$source
              ),
              note_chars
            ),
            sep = "<br>"
          )
        ),
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        align = 'left',
        font = list(size = note_size)
      )
    ) %>%
    config(
      modeBarButtonsToRemove = plotly_remove_buttons,
      toImageButtonOptions = list(
        filename = paste(
          tolower(base_country),
          "- trends",
          tolower(indicator_name)
        )
      )
    )
}

# Cross-country comparison #####################################################

#' Horizontal bar chart for one indicator (Cross-Country Comparison tab)
#'
#' Builds the ggplot behind the Cross-Country Comparison tab: one horizontal bar
#' per country for a single indicator's closeness-to-frontier, plus a
#' "Comparison countries median" bar when more than one comparison country is
#' selected and one bar per selected group / custom group. Piped into
#' [interactive_bar()].
#'
#' @param data Wide closeness-to-frontier table (`app_data$global_data`) with
#'   `country_name` plus one column per indicator code.
#' @param base_country Character. Base-country name.
#' @param comparison_countries Character vector of comparison-country names.
#' @param groups Character vector of comparison-group / custom-group names.
#' @param var Character. Indicator *display name*; resolved to a code via
#'   `variable_names`.
#' @param variable_names Indicator dictionary (`app_data$variable_names`).
#' @param custom_df Custom-group data frame (`Grp` / `Countries`), or `NULL`.
#' @param color_base_bar,color_comp_bar,color_groups_bar Hex colour strings for
#'   the base-country bar, comparison-country bars, and group bars.
#' @param ctf_long_dyn Long dynamic closeness-to-frontier table
#'   (`app_data$ctf_long_dyn`) used to compute custom-group medians.
#'
#' @return A `ggplot` object.
#'
#' @seealso [interactive_bar()], [mod_country_comparison_server()].
#' @export
static_bar <-
  function(
    data,
    base_country,
    comparison_countries,
    groups,
    var,
    variable_names,
    custom_df,
    color_base_bar,
    color_comp_bar,
    color_groups_bar,
    ctf_long_dyn
  ) {
    varname <-
      variable_names %>%
      filter(var_name == var) %>%
      select(variable) %>%
      unlist %>%
      unname

    data <-
      data %>%
      filter(
        country_name %in% c(base_country, comparison_countries, groups)
      )

    if ((!is.null(comparison_countries)) & (length(comparison_countries) > 1)) {
      median <-
        data %>%
        filter(
          country_name %in% comparison_countries
        ) %>%
        ungroup %>%
        summarise(
          across(
            all_of(varname),
            ~ median(., na.rm = TRUE)
          )
        ) %>%
        mutate(country_name = "Comparison countries median")

      data <-
        data %>%
        bind_rows(median)
    }

    if (!is.null(custom_df)) {
      ## If any of the benchmark medians is a custom group
      if (any(groups %in% custom_df$Grp)) {
        ## create a vector of these groups
        selected_custom_grps <- unique(custom_df$Grp)[
          unique(custom_df$Grp) %in% groups
        ]

        custom_grp_median_data_func <- function(selected_custom_grp) {
          custom_df_per_group <- custom_df %>%
            filter(Grp == selected_custom_grp)

          ## calculate medians for each group
          custom_grp_median_data <-
            ctf_long_dyn %>%
            filter(
              var_name %in% var,
              country_name %in% custom_df_per_group$Countries ## extract countries that fall in this group
            ) %>%
            mutate(
              country_name = unique(custom_df_per_group$Grp), ## the country name will be the
              ## name of the group.
              group = NA
            ) %>%
            unique %>%
            group_by(
              country_name,
              var_name
            ) %>%
            mutate(value = median(value, na.rm = TRUE)) %>%
            distinct(var_name, value, country_name) %>%
            ungroup

          return(custom_grp_median_data)
        }

        custom_grp_median_data_df <- purrr::map_df(
          selected_custom_grps,
          custom_grp_median_data_func
        )

        custom_grp_median_data_df <- custom_grp_median_data_df %>%
          left_join(
            .,
            variable_names %>% select(var_name, variable),
            by = "var_name"
          ) %>%
          relocate(variable, .before = var_name) %>%
          select(-var_name) %>%
          spread(variable, value) %>%
          mutate(country_group = 1)

        ## and append this to median data generated for pre-determined groups
        data <- data %>%
          bind_rows(custom_grp_median_data_df)
      }
    }
    #NEW PIPELINE START=============
    data <- data %>%
      ungroup() %>%
      mutate(
        # Initial color assignment
        color = case_when(
          country_name == base_country ~ color_base_bar,
          country_name %in% comparison_countries ~ color_comp_bar,
          country_name %in% groups ~ color_groups_bar,
          TRUE ~ '#7a7d7b' # Default gray color for median
        ),
        country_name = fct_reorder(country_name, get(varname), min)
      ) %>%
      select(all_of(varname), country_name, color) %>%
      ungroup()
    #NEW PIPELINE END============
    #PLOTTING:
    ggplot(
      data = data,
      aes(
        x = get(varname),
        y = country_name
      )
    ) +
      geom_col(
        aes(
          fill = color
          # fill = factor(color)
        )
      ) +

      scale_fill_identity() +

      geom_text(
        aes(
          x = get(varname) + .03,
          label = round(get(varname), 3)
        )
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        plot.caption = element_text(size = 8, hjust = 0),
        plot.caption.position = "plot"
      ) +
      labs(
        y = NULL,
        x = "Closeness to Frontier",
        fill = NULL,
        title = paste0("<b>", var, "</b>")
      )
  }


#' Convert a cross-country bar ggplot to an interactive plotly
#'
#' Turns a [static_bar()] ggplot into the plotly shown on the Cross-Country
#' Comparison tab: titles the legend "Closeness to frontier", adds the indicator
#' definition and source as a footnote annotation, trims the modebar and names
#' the PNG export `<var>_bar`.
#'
#' @param x A `ggplot` object from [static_bar()].
#' @param var Character. Indicator display name -- looked up in `definitions`
#'   and used for the export filename.
#' @param definitions Indicator dictionary (`app_data$db_variables`).
#' @param buttons Character vector of plotly modebar button ids to remove.
#'
#' @return A `plotly` htmlwidget.
#'
#' @seealso [static_bar()].
#' @export
interactive_bar <-
  function(x, var, definitions, buttons) {
    def <-
      definitions %>%
      filter(var_name == var)

    x %>%
      ggplotly(tooltip = "text") %>%
      layout(
        legend = list(
          title = list(text = '<b>Closeness to\nfrontier:</b>'),
          y = 0.5
        ),
        margin = list(t = 75, b = 250),
        annotations = list(
          x = -0.1,
          y = -0.1,
          text = paste(
            "<b>Note:</b> Data displayed is based on the CTF data.<br>",
            "<b>Definition:</b>",
            str_replace_all(
              str_wrap(def$description, 120),
              "\n",
              "<br>"
            ),
            "<br>",
            "<b>Source:</b>",
            def$source
          ),
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "top",
          align = "left",
          font = list(size = note_size)
        )
      ) %>%
      config(
        modeBarButtonsToRemove = buttons,
        toImageButtonOptions = list(
          filename = paste0(
            tolower(stringr::str_replace_all(var, "\\s", "_")),
            "_bar"
          ),
          width = 1050,
          height = 675
        )
      )
  }

# Bivariate correlation #####################################################

#' Scatter plot of two indicators across countries (Bivariate Correlation tab)
#'
#' Builds the ggplot behind the Bivariate Correlation tab: one point per country
#' positioned by its closeness-to-frontier on the chosen x and y indicators
#' (either axis may instead be `"Log GDP per capita, PPP"`), with the base and
#' comparison countries highlighted and an optional linear fit. Returns both the
#' plot and the plotted data (the latter is offered as a CSV download).
#'
#' @param data Wide indicator table (`app_data$global_data`).
#' @param base_country Character. Base-country name.
#' @param comparison_countries Character vector of comparison-country names.
#' @param high_group Data frame of countries to highlight (a group), possibly
#'   zero-row; from `mod_bivariate_server()`'s `high_group` reactive.
#' @param y_scatter,x_scatter Character. Indicator display names for the two
#'   axes, or the literal `"Log GDP per capita, PPP"`.
#' @param variable_names Indicator dictionary (`app_data$variable_names`).
#' @param country_list Country/group metadata (`app_data$country_list`).
#' @param linear_fit Logical. Whether to add an OLS fit line.
#' @param color_base_scatter,color_comp_scatter Hex colour strings for the
#'   base-country and comparison-country points.
#'
#' @return A named list: `sc_plot` (a `ggplot`) and `sc_data` (the tibble
#'   plotted).
#'
#' @seealso [interactive_scatter()], [x_scatter_choices()],
#'   [mod_bivariate_server()].
#' @export
static_scatter <-
  function(
    data,
    base_country,
    comparison_countries,
    high_group,
    y_scatter,
    x_scatter,
    variable_names,
    country_list,
    linear_fit,
    color_base_scatter,
    color_comp_scatter
  ) {
    y <-
      ifelse(
        y_scatter == "Log GDP per capita, PPP",
        "log",
        variable_names %>%
          filter(var_name == y_scatter) %>%
          select(variable) %>%
          unlist %>%
          unname
      )

    x <-
      ifelse(
        x_scatter == "Log GDP per capita, PPP",
        "log",
        variable_names %>%
          filter(var_name == x_scatter) %>%
          select(variable) %>%
          unlist %>%
          unname
      )

    data <- data %>%
      mutate(
        # label = paste0(
        #   "Country: ", country_name, "<br>"
        # ),
        log = log(wdi_nygdppcapppkd),
        type = case_when(
          country_name == base_country ~ "Base country",
          country_name %in% comparison_countries ~ "Comparison countries",
          TRUE ~ "Others"
        )
      ) %>%
      left_join(
        high_group,
        by = "country_name"
      )

    ## generate a different label, one that is a combination of country, x axis and y axis variable

    xvar <- sym(x) # sym() enables us to use a string variable as is, as long as we wrap them in {{...}}
    yvar <- sym(y)

    data <- data %>%
      dplyr::rowwise() %>%
      mutate(
        label = paste0(
          "Country: ",
          country_name,
          "<br>",
          "<br>",
          "x: ",
          {{ xvar }},
          "<br>",
          "<br>",
          "y: ",
          {{ yvar }},
          "<br>",
          "<br>"
        )
      )

    sc_data <- data %>%
      select(
        country_code,
        country_name,
        income_group,
        region,
        country_group,
        x,
        y
      )

    sc_data <- sc_data %>%
      rename(
        !!x_scatter := x, # Rename 'x' to the value in x_scatter
        !!y_scatter := y # Rename 'y' to the value in y_scatter
      )

    #PLOTTING THE SCATTER PLOT
    sc_plot <- ggplot(
      data,
      aes(
        x = {{ xvar }}, ## see how xvar is defined above
        y = {{ yvar }},
        text = label
      )
    ) +
      geom_point(
        data = data %>% filter(group %in% high_group$group),
        size = 4,
        shape = 1,
        color = "#60C2F7"
      ) +
      geom_point(
        aes(
          color = type,
          shape = type
        ),
        size = 2
      ) +

      scale_color_manual(
        values = c(
          "Base country" = color_base_scatter,
          "Comparison countries" = color_comp_scatter,
          group_name = "#60C2F7"
        )
      ) +
      scale_shape_manual(
        values = c(
          "Base country" = 16,
          "Comparison countries" = 16,
          "Others" = 1
        )
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        axis.ticks = element_blank(),
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        plot.caption = element_text(size = 8, hjust = 0),
        plot.caption.position = "plot"
      ) +
      labs(
        y = ifelse(
          y_scatter == "Log GDP per capita, PPP",
          "<b>Log GDP per capita, PPP</b>",
          paste0("<b>", y_scatter, "<br>(closeness to frontier)</b>")
        ),
        x = ifelse(
          x_scatter == "Log GDP per capita, PPP",
          "<b>Log GDP per capita, PPP</b>",
          paste0("<b>", x_scatter, "<br>(closeness to frontier)</b>")
        )
      )

    ## linear fit line
    if (linear_fit == TRUE) {
      sc_plot <- sc_plot +
        geom_smooth(
          aes(
            x = {{ xvar }},
            y = {{ yvar }}
          ),
          method = "lm",
          color = "#e94152",
          formula = 'y ~ x',
          linewidth = 0.5,
          se = FALSE,
          inherit.aes = FALSE
        )
    } else {
      sc_plot <- sc_plot
    }

    return(list(sc_plot = sc_plot, sc_data = sc_data))
  }

#' Convert a bivariate scatter ggplot to an interactive plotly
#'
#' Turns the `sc_plot` from [static_scatter()] into the plotly shown on the
#' Bivariate Correlation tab: adds both indicators' definitions and sources (and,
#' when a highlight group is active, a note naming it) as a footnote annotation,
#' trims the modebar and names the PNG export after the axis pair.
#'
#' @param plot The `sc_plot` `ggplot` element returned by [static_scatter()].
#' @param y_scatter,x_scatter Character. Axis indicator display names, or
#'   `"Log GDP per capita, PPP"`.
#' @param definitions Indicator dictionary (`app_data$db_variables`).
#' @param high_group Data frame of highlighted countries (possibly zero-row).
#' @param buttons Character vector of plotly modebar button ids to remove.
#'
#' @return A `plotly` htmlwidget.
#'
#' @seealso [static_scatter()].
#' @export
interactive_scatter <-
  function(plot, y_scatter, x_scatter, definitions, high_group, buttons) {
    y <-
      definitions %>%
      filter(var_name == y_scatter)

    x <-
      definitions %>%
      filter(var_name == x_scatter)

    if (x_scatter == "Log GDP per capita, PPP") {
      x <- definitions %>%
        filter(variable == "wdi_nygdppcapppkd")
    }

    if (y_scatter == "Log GDP per capita, PPP") {
      y <- definitions %>%
        filter(variable == "wdi_nygdppcapppkd")
    }

    def_note <-
      paste(
        "<b>Definitions:</b>",
        str_wrap(
          paste0(
            "<br>",
            "<em>",
            x$var_name,
            ":</em> ",
            x$description,
            " (Source: ",
            x$source,
            ")"
          ),
          note_chars
        ),
        str_wrap(
          paste0(
            "<br>",
            "<em>",
            y$var_name,
            ":</em> ",
            y$description,
            " (Source: ",
            y$source,
            ")"
          ),
          note_chars
        )
      )

    if (nrow(high_group) > 0) {
      def_note <-
        paste0(
          "<b>Note:</b> ",
          str_wrap(
            paste(
              unique(high_group$group),
              "countries highlighted in light blue."
            ),
            note_chars
          ),
          "<br><br>",
          def_note
        )
    }

    plot %>%
      ggplotly(tooltip = c("text")) %>%
      layout(
        margin = list(
          t = 50,
          b = 300
        ),
        legend = list(
          title = list(text = ''),
          y = 0.5
        ),
        annotations = list(
          x = -0.03,
          y = -0.5,
          text = HTML(def_note),
          showarrow = F,
          xref = 'paper',
          yref = 'paper',
          align = 'left',
          font = list(size = note_size)
        )
      ) %>%
      config(
        modeBarButtonsToRemove = buttons,
        toImageButtonOptions = list(
          filename = paste("GDP per capita x", y_scatter),
          width = 1050,
          height = 675
        )
      )
  }

#' De-duplicate the legend of a facetted ggplotly
#'
#' When a facetted ggplot is converted with `plotly::ggplotly()` every facet
#' contributes its own copy of each legend entry, so the legend repeats itself
#' N times. `clean_plotly_legend()` walks the plotly object's traces, strips the
#' `"(group,1)"` position suffixes ggplotly adds, collapses the duplicates, and
#' sets `showlegend = FALSE` on every trace after the first of each group.
#'
#' @details
#' Adapted from
#' <https://stackoverflow.com/questions/69289623/>. Defines three local
#' helpers -- `assign_leg_grp()`, `parse_leg_nms()` and `simplify_leg_grps()` --
#' that are implementation detail and not exported.
#'
#' @param .pltly_obj A `plotly` object (typically straight out of
#'   `plotly::ggplotly()`).
#' @param .new_legend Optional character vector of replacement legend-group
#'   names, in order. Default `c()` keeps the cleaned original names.
#'
#' @return The `.pltly_obj`, with its `x$data` traces rewritten.
#'
#' @seealso [interactive_plot()], [fixfacets()].
#' @export
clean_plotly_legend <- function(.pltly_obj, .new_legend = c()) {
  # Cleans up a plotly object legend, particularly when ggplot is facetted
  
  assign_leg_grp <- function(.legend_group, .leg_nms) {
    # Assigns a legend group from the list of possible entries
    # Used to modify the legend settings for a plotly object
    
    leg_nms_rem <- .leg_nms
    
    parse_leg_nms <- function(.leg_options) {
      # Assigns a .leg_name, if possible
      # .leg_options is a 2-element list: 1 = original value; 2 = remaining options
      
      if (is.na(.leg_options)) {
        .leg_options
      } else if(length(leg_nms_rem) == 0) {
        # No more legend names to assign
        .leg_options
      } else {
        # Transfer the first element of the remaining options
        leg_nm_new <- leg_nms_rem[[1]]
        leg_nms_rem <<- leg_nms_rem[-1]
        
        leg_nm_new
      }
      
    }
    
    .legend_group %>% 
      map(~ parse_leg_nms(.))
    
  }
  
  simplify_leg_grps <- function(.legendgroup_vec) {
    # Simplifies legend groups by removing brackets, position numbers and then de-duplicating
    
    leg_grp_cln <-
      map_chr(.legendgroup_vec, ~ str_replace_all(., c("^\\(" = "", ",\\d+\\)$" = "")))
    
    modify_if(leg_grp_cln, duplicated(leg_grp_cln), ~ NA_character_)
    
  }
  
  pltly_obj_data <-
    .pltly_obj$x$data
  
  pltly_leg_grp <-
    # pltly_leg_grp is a character vector where each element represents a legend group. Element is NA if legend group not required or doesn't exist
    pltly_obj_data%>% 
    map(~ pluck(., "legendgroup")) %>% 
    map_chr(~ if (is.null(.)) {NA_character_} else {.}) %>%
    # Elements where showlegend = FALSE have legendgroup = NULL. 
    
    simplify_leg_grps() %>% 
    
    assign_leg_grp(.new_legend) 
  
  pltly_obj_data_new <-
    pltly_obj_data %>% 
    map2(pltly_leg_grp, ~ list_modify(.x, legendgroup = .y)) %>%
    map2(pltly_leg_grp, ~ list_modify(.x, name = .y)) %>%
    map2(pltly_leg_grp, ~ list_modify(.x, showlegend = !is.na(.y)))
  # i.e. showlegend set to FALSE when is.na(pltly_leg_grp), TRUE when not is.na(pltly_leg_grp)
  
  .pltly_obj$x$data <- pltly_obj_data_new
  
  .pltly_obj
  
}

#' Equalise facet widths in a facetted ggplotly
#'
#' `plotly::ggplotly()` renders the first and last panels of a `facet_wrap()`
#' wider than the middle ones. `fixfacets()` rewrites the panel domains, the
#' grey strip-label background shapes, and the strip-label annotation positions
#' so every facet is the same width.
#'
#' @details
#' Adapted from
#' <https://stackoverflow.com/questions/61580973/>. Strip backgrounds are
#' identified by their fill colour `"rgba(217,217,217,1)"`; strip labels by
#' matching their text against `facets`. Currently called only from
#' commented-out code, but kept as a utility for future facetted plotly output.
#'
#' @param figure A `plotly` object from `plotly::ggplotly()` of a facetted
#'   ggplot.
#' @param facets Character vector of facet (strip) labels, in display order.
#' @param domain_offset Numeric. Width of the strip-label shape as a fraction of
#'   the plot width (e.g. `0.16`).
#'
#' @return The `figure`, with panel domains and strip positions rewritten.
#'
#' @seealso [clean_plotly_legend()].
#' @export
fixfacets <- function(figure, facets, domain_offset){
  
  # split x ranges from 0 to 1 into
  # intervals corresponding to number of facets
  # xHi = highest x for shape
  n_facets <- length(facets)
  xHi <- seq(0, 1, len = n_facets+1)
  xHi <- xHi[2:length(xHi)]
  
  xOs <- domain_offset
  
  # Shape manipulations, identified by dark grey backround: "rgba(217,217,217,1)"
  # structure: p$x$layout$shapes[[2]]$
  shp <- figure$x$layout$shapes
  j <- 1
  for (i in seq_along(shp)){
    if (shp[[i]]$fillcolor=="rgba(217,217,217,1)" & (!is.na(shp[[i]]$fillcolor))){
      #$x$layout$shapes[[i]]$fillcolor <- 'rgba(0,0,255,0.5)' # optionally change color for each label shape
      figure$x$layout$shapes[[i]]$x1 <- xHi[j]
      figure$x$layout$shapes[[i]]$x0 <- (xHi[j] - xOs)
      #figure$x$layout$shapes[[i]]$y <- -0.05
      j<-j+1
    }
  }
  
  # annotation manipulations, identified by label name
  # structure: p$x$layout$annotations[[2]]
  ann <- figure$x$layout$annotations
  annos <- facets
  j <- 1
  for (i in seq_along(ann)){
    if (ann[[i]]$text %in% annos){
      # but each annotation between high and low x,
      # and set adjustment to center
      figure$x$layout$annotations[[i]]$x <- (((xHi[j]-xOs)+xHi[j])/2)
      figure$x$layout$annotations[[i]]$xanchor <- 'center'
      #print(figure$x$layout$annotations[[i]]$y)
      #figure$x$layout$annotations[[i]]$y <- -0.05
      j<-j+1
    }
  }
  
  # domain manipulations
  # set high and low x for each facet domain
  xax <- names(figure$x$layout)
  j <- 1
  for (i in seq_along(xax)){
    if (!is.na(pmatch('xaxis', lot[i]))){
      #print(p[['x']][['layout']][[lot[i]]][['domain']][2])
      figure[['x']][['layout']][[xax[i]]][['domain']][2] <- xHi[j]
      figure[['x']][['layout']][[xax[i]]][['domain']][1] <- xHi[j] - xOs
      j<-j+1
    }
  }
  
  return(figure)
}