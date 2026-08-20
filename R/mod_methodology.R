#' methodology module UI
#'
#' @param id a unique identifier for this module.
#' @param app_data Shared data list from [build_app_data()].
#'
#' @return a `tagList` of UI elements
#' @export
mod_methodology_ui <- function(id, app_data) {
  ns <- NS(id)

  tagList(
    box(
      width = 12,
      status = "navy",
      title = "User Guide",

      p('Here is a Downloadable User Guide Meant to Demonstrate the Capabilities of the CLIAR Dashboard'),
      downloadButton(ns("download_user_guide"),
                     "Download CLIAR User Guide",
                     style = "background-color: #204d74; color: white")
    ),
    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "Institutional families",

      p("The CLIAR Benchmarking uses a set of curated and validated institutional indicators, clustered into 13 institutional clusters:",

        tags$ul(
          tags$li("Political institutions"),
          tags$li("Social institutions"),
          tags$li("Absence of Corruption"),
          tags$li("Transparency and Accountability institutions"),
          tags$li("Justice institutions"),
          tags$li("Public Finance Institutions"),
          tags$li("Public Human Resource Management institutions"),
          tags$li("Digital and Data institutions"),
          tags$li("Business environment institutions"),
          tags$li("SOE Corporate Governance"),
          tags$li("Labor and Social Protection institutions"),
          tags$li("Service Delivery institutions"),
          tags$li("Climate Change and Environment institutions")
        )

      ),
      p("The proposed clusters are based on an effort to capture key functions that different institutions perform. In so doing, the categorization process faces a trade-off between aggregation and narrowness, where the categories ought to be broad enough to capture enough indicators and policy spaces, but narrow enough to guide a deep qualitative analysis as well as a fruitful and engaged conversation with the country. In addition, the categorization also faces the limitations of data availability."),
      p('All country-level indicators can be downloaded in the “Data” tab.')
    ),

    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "Closeness to frontier",

      p('The dashboard uses a “Closeness to Frontier"(CTF) methodology. The CTF methodology allows to assess country’s performance across institutional indicators by comparing it with the “global frontier”, where the global frontier is the world’s best performer. For each indicator, a country’s performance is rescaled on a 0-1 scale using the linear transformation (worst–y)/(worst–frontier), where 1 represents the best performer and 0 the worst performer. The higher the score, the closer a country is to the best performer and the lower the score, the closer a country is to the worst performer, and more distant to the frontier. The best and worst performers are identified using available data from the global sample (i.e., considering all countries for which data is available), and using the relevant time period according to the benchmarking approach –i.e., whether it estimates the static (default) CTF benchmarking scores or dynamic CTF scores. In the static case, the average of the 2019-2023 period is used.'),
      p('For each institutional family, the CTF scores obtained for each indicator are aggregated through simple averaging into one CTF score at family level. This captures the overall performance for an institutional family relatively to the “global frontier”, while the performance across the indicators will help identify the most challenging areas for institutional strengthening.')

    ),

    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "Percentile analysis and comparator countries",

      p('The CTF scores compare the country’s performance with the best and worst performers at global level. However, how does the country compare relatively to a set of chosen comparators?'),
      p('The dashboard uses percentile distribution and traffic light coloring to capture the areas where the largest institutional gaps exist, ',
        HTML('<b>relative to the set of country comparators</b>'),
        '. Relative institutional weaknesses and strengths are defined based on the percentile in which each country indicator belongs. This methodology requires teams to make an informed decision on the set of comparator countries used for the benchmarking, since institutional weaknesses and strengths are identified relatively to those comparator countries.'),
      p('The “Closeness to Frontier” (length of the bar) and the percentile analysis (color of the bar) capture two related but different performance dimensions. The CTF compares the country’s performance with the best and worst performers. The percentile analysis benchmarks the country’s performance with all the set of other comparator countries. For example, it could be that for one indicator or institutional cluster the CTF score is relatively high and close to 1 (indicating in fact ‘closeness to the frontier’) but, at the same time, this dimension is marked as an institutional weakness (red coloring) because the country’s performance is still worse than the majority of comparator countries.'),
      p('The percentile analysis requires indicators to be available for the base country, while it also effectively drops those indicators whose distribution precludes this percentile classification (i.e., low variance).')
    ),

    box(
      width = 12,
      status = "navy",
      collapsed = TRUE,
      title = "Country group definitions",

      p(
        "Country group definitions are extracted from the",
        a(
          "World Bank Country and Lending Groups.",
          href = "https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups"
        ),
        "which classifies all 218 World Bank member countries and economies.",
        "Income classifications for FY24 is based on 2022 gross national income (GNI) per capita,",
        "calculated using the World Bank Atlas method."
      ),
      p(
        "The groups are:",
        tags$ul(
          tags$li(HTML("<b>Low income:</b> $1,135 or less")),
          tags$li(HTML("<b>Lower middle income:</b> $1,136 - 4,465")),
          tags$li(HTML("<b>Upper middle income:</b> $4,466 - 13,845")),
          tags$li(HTML("<b>High income:</b> more than $13,845"))
        )
      ),
      p(
        HTML("The term <i>country</i>, used interchangeably with <i>economy,</i>"),
        "does not imply political independence but refers to any territory for which authorities report separate social or economic statistics.",
        "Income classifications set on 1 July 2023 remain in effect until 1 July 2024.",
      ),
      p(
        "OECD members are: ", paste0(paste(app_data$country_list %>% filter(group_code == "OED") %>% .$country_name, collapse = ", "), ".")
      )
    ),


    box(
      width = 12,
      status = "navy",
      title = "List of indicators",

      p("The indicators used to benchmark the institutional families are extracted from multiple public data sources.
                      For a full list of the indicators used, their sources, and their definitions, download the metadata below."),
      downloadButton(ns("download_indicators"),
                     "Download indicator definitions",
                     style = "background-color: #204d74; color: white")
    ),

    box(
      width = 12,
      status = "navy",
      title = "Where can I find additional information on the methodology?",
      downloadButton(ns("download_metho"),
                     "Download complete methodology",
                     style = "background-color: #204d74; color: white")
    )
  )
}

#' methodology module server
#'
#' @param id a unique identifier for this module.
#' @param app_data Shared data list from [build_app_data()].
#'
#' @return NULL; called for its side effects.
#' @export
mod_methodology_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {

    output$download_user_guide <- downloadHandler(
      filename = 'CLIAR_User_Guide.docx',
      content = function(file) {
        file.copy(app_sys("app/www/dashboard_userguide_outline_v5.2.docx"), file)
      }
    )

    # Download csv with definitions
    output$download_indicators <-
      downloadHandler(
        filename = "CLIAR Indicators.csv",
        content = function(file) {
          write_csv(
            app_data$db_variables %>%
              select(
                indicator = var_name,
                family = family_name,
                description,
                description_short,
                source
              ),
            file,
            na = ""
          )
        }
      )

    # Full methodology --------------------------------------------------------
    output$download_metho <-
      downloadHandler(
        filename = "CLIAR Benchmarking.pdf",
        content = function(file) {
          file.copy(app_sys("app/www/CLIAR Benchmarking.pdf"), file)
        }
      )
  })
}
