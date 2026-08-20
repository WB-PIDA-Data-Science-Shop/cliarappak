app_ui <- function(request) {
  app_data <- golem::get_golem_options("app_data")

  tagList(
    golem_add_external_resources(),
    dashboardPage(
      freshTheme = fresh::create_theme(fresh::bs4dash_layout(sidebar_width = "350px")),

      dashboardHeader(
        title = dashboardBrand(title = "CLIAR Benchmarking Dashboard"),
        status = "white",
        border = TRUE,
        sidebarIcon = icon("bars"),
        controlbarIcon = icon("th"),
        fixed = FALSE
      ),

      dashboardSidebar(
        status = "info",
        skin = "light",
        elevation = 5,
        sidebarMenu(
          menuItem("Home", tabName = "home", icon = icon("home")),
          menuItem("Country benchmarking", tabName = "benchmark", icon = icon("sort-amount-up")),
          menuItem("Cross-country comparison", tabName = "country", icon = icon("chart-bar")),
          menuItem("Bivariate correlation", tabName = "scatter", icon = icon("search-dollar")),
          menuItem("World map", tabName = "world_map", icon = icon("globe-americas")),
          menuItem("Time trends", tabName = "trends", icon = icon("chart-line")),
          menuItem("Data", tabName = "data", icon = icon("table")),
          menuItem("Methodology & User Guide", tabName = "methodology_ug", icon = icon("book")),
          menuItem("Publications", tabName = "pubs", icon = icon("list")),
          menuItem("Terms of use and Disclaimers", tabName = "terms", icon = icon("handshake")),
          menuItem("FAQ", tabName = "faq", icon = icon("question")),
          menuItem("Contact Us",
                   icon = icon("comments", lib = "font-awesome"),
                   href = "mailto:CLIAR@worldbank.org"),
          menuItem("Source code",
                   icon = icon("github", lib = "font-awesome"),
                   href = "https://github.com/WB-PIDA-Data-Science-Shop/cliarapp")
        )
      ),

      dashboardBody(
        cicerone::use_cicerone(),
        tabItems(
          tabItem("home", mod_home_ui("home")),
          tabItem("benchmark", mod_benchmark_ui("benchmark", app_data)),
          tabItem("country", mod_country_comparison_ui("country", app_data)),
          tabItem("scatter", mod_bivariate_ui("scatter", app_data)),
          tabItem("world_map", mod_world_map_ui("world_map", app_data)),
          tabItem("trends", mod_trends_ui("trends", app_data)),
          tabItem("data", mod_data_ui("data", app_data)),
          tabItem("methodology_ug", mod_methodology_ui("methodology_ug", app_data)),
          tabItem("pubs", mod_publications_ui("publications", app_data$countries)),
          tabItem("terms", mod_terms_ui("terms")),
          tabItem("faq", mod_faq_ui("faq"))
        )
      )
    )
  )
}

golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  tags$head(
    favicon(),
    bundle_resources(path = app_sys("app/www"), app_title = "cliarappak"),
    tags$head(includeCSS(app_sys("app/www/styles.css")))
  )
}
