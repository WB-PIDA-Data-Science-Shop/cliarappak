# cliarapp → cliarappak migration guide

Execution guide for turning `cliarapp` (WB-PIDA-Data-Science-Shop/cliarapp) into a golem-style
R package inside this repo. Companion to the architecture plan already agreed — this document
gives literal commands and file contents to paste in, phase by phase. Work through phases in
order; each ends with something you can `devtools::load_all()` successfully.

Assumes you have both repos checked out locally, e.g.:
```
C:\Users\ifean\Documents\WorldBankWork\GitProjects\cliarappak\   (this repo — target)
C:\...\cliarapp\                                                 (source — clone if needed)
```

---

## Phase 1 — Scaffold + DESCRIPTION

### 1a. Create directories

```powershell
cd C:\Users\ifean\Documents\WorldBankWork\GitProjects\cliarappak
New-Item -ItemType Directory -Force R, inst\app\www, inst\rmd, inst\extdata, dev, tests\testthat
```

### 1b. Replace `DESCRIPTION`

```
Package: cliarappak
Type: Package
Title: CLIAR Benchmarking Dashboard
Version: 0.1.0
Authors@R: c(
    person("Ifeanyi", "Edochie", email = "iedochie@worldbank.org", role = c("aut", "cre")),
    person("Ileana", "Marroquin", email = "imarroquinmartin@worldbank.org", role = "aut"),
    person("Galileu", "Kim", email = "galileukim@worldbank.org", role = "aut"))
Description: Shiny dashboard for the Country-Level Institutional Assessment and
    Review (CLIAR), packaged as an installable golem application. Benchmarks
    institutional indicators across countries using closeness-to-frontier (CTF)
    scores computed by the cliaretl package.
License: MIT + file LICENSE
Encoding: UTF-8
LazyData: true
Depends: R (>= 4.5)
Imports:
    shiny,
    golem,
    bs4Dash,
    fresh,
    plotly,
    DT,
    shinyjs,
    shinyBS,
    shinycssloaders,
    shinybusy,
    shinyWidgets,
    shinyhelper,
    shinyFeedback,
    cicerone,
    colourpicker,
    bsplus,
    sf,
    haven,
    zoo,
    formattable,
    data.table,
    hrbrthemes,
    htmltools,
    officer,
    rvg,
    openxlsx,
    countrycode,
    readxl,
    rappdirs,
    fs,
    dplyr,
    tidyr,
    ggplot2,
    stringr,
    purrr,
    tibble,
    readr,
    rmarkdown,
    knitr,
    cliaretl
Suggests:
    testthat (>= 3.0.0),
    flextable,
    Hmisc,
    janitor,
    RColorBrewer,
    config
Remotes:
    WB-PIDA-Data-Science-Shop/cliaretl
Config/testthat/edition: 3
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.3.3
```

Note vs. the original `cliarapp`: `library(tidyverse)` is unpacked into the individual packages
actually used (`dplyr`, `tidyr`, `ggplot2`, `stringr`, `purrr`, `tibble`, `readr`) — CRAN/package
best practice is to depend on the specific packages, not the meta-package. `here` is dropped —
Phase 3 replaces every `here(...)` call with `app_sys(...)`/`system.file(...)`, which is the
correct way to reference package-internal files. `shinyFeedback` is added — it's used by
`toast_messages_func()` in the original `auxiliary/fun_loadInputs.R` via `shinyFeedback::showToast`
but was never in `global.R`'s `library()` list (would have errored the moment that code path ran —
worth testing in Phase 6).

### 1c. `.Rbuildignore` additions

Append to the existing file:
```
^dev$
^MIGRATION_GUIDE\.md$
```

### 1d. `R/run_app.R`

```r

#' Run the CLIAR Shiny Application
#'
#' @details
#' `run_app()` wraps its call to [shiny::shinyApp()] in [golem::with_golem_options()].
#' This solves a plumbing problem: `shiny::shinyApp()` doesn't have a way for
#' `run_app()` to pass arbitrary custom parameters through to `app_server()`,
#' because `app_server` has to keep the exact signature Shiny expects --
#' `function(input, output, session)`. There's no room in that signature for
#' extra arguments.
#'
#' What `with_golem_options()` actually does: it takes the `shinyApp()` object
#' and a list of arbitrary values (`golem_opts`), and stashes that list
#' somewhere Shiny's internals can retrieve later -- via
#' [golem::get_golem_options()], callable from inside `app_server()` or any
#' module server, even though it was never passed as a formal argument. It's
#' essentially a side-channel for configuration that bypasses the
#' fixed-signature constraint.
#'
#' Any arguments passed to `run_app(...)` beyond `onStart`, `options`,
#' `enableBookmarking`, and `uiPattern` are collected into `...` and threaded
#' into `golem_opts` this way.
#'
#' @param ... arguments to pass to golem_opts.
#' @param onStart,options,enableBookmarking,uiPattern See `?shiny::shinyApp`.
#' @export
run_app <- function(onStart = NULL,
                    options = list(),
                    enableBookmarking = NULL,
                    uiPattern = "/",
                    ...) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...) 
  )
}
```

### 1e. `R/app_config.R`

```r
#' Access files in the current app
#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "cliarappak")
}

#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#'
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv("GOLEM_CONFIG_ACTIVE", Sys.getenv("R_CONFIG_ACTIVE", "default")),
  use_parent = TRUE
) {
  config::get(
    value = value,
    config = config,
    file = app_sys("golem-config.yml"),
    use_parent = use_parent
  )
}
```

### 1f. Placeholder `R/app_ui.R` and `R/app_server.R`

These get filled in for real in Phase 4. For now, stub them so the package loads:

```r
# R/app_ui.R
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    fluidPage(
      h1("cliarappak scaffold — replace in Phase 4")
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
```

```r
# R/app_server.R
app_server <- function(input, output, session) {
  # replaced in Phase 4
}
```

### 1g. `dev/02_dev.R` (optional but handy while you work)

```r
devtools::load_all()
devtools::document()
devtools::check()
golem::run_dev()
```

### Checkpoint

```r
install.packages(c("golem", "devtools"))
devtools::document()
devtools::load_all()
```

Should load without error (ignore roxygen warnings about missing `@export` on stubs for now).

**Troubleshooting — `HTTP error 401 / Bad credentials`**: this comes from installing `cliaretl`
via the `Remotes:` field (the only GitHub-sourced dependency) and means a `GITHUB_PAT` is set but
stale/expired, not that one is missing.
```r
Sys.getenv("GITHUB_PAT")        # confirm one is set
usethis::create_github_token()  # generate a fresh classic PAT, repo scope
gitcreds::gitcreds_set()        # paste the new token
usethis::git_sitrep()           # verify
```
Restart R, then retry. To isolate just this install: `remotes::install_github("WB-PIDA-Data-Science-Shop/cliaretl")`.

---

## Phase 2 — Port helper/pure functions

Straight file-for-file port from `cliarapp/auxiliary/` into `R/fct_*.R`, **excluding the 5 dead
files** and **de-duplicating the 3 triple-defined functions**.

### 2a. Files to port as-is (copy content, add roxygen `@export`/`@import` tags, wrap in one file each)

| Target | Source file(s) |
|---|---|
| `R/fct_plots.R` | `auxiliary/plots.R` (the live, sourced-last version — **not** `dynamic_benchmarking.R`) |
| `R/fct_plots.R` (append) | `auxiliary/clean_plotly_legend.R`, `auxiliary/fixfacets.R` |
| `R/fct_quantiles.R` | `auxiliary/fun_quantiles.R`, `auxiliary/fun_low_variance.R`, `auxiliary/fun_missing_var.R` |
| `R/fct_family.R` | `auxiliary/fun_family_data.R` — **but see 2c below, do not port verbatim** |
| `R/fct_downloads.R` | `auxiliary/fun_download_prep.R`, `auxiliary/fun_check_data.R` |
| `R/fct_helpers.R` | `auxiliary/fun_loadInputs.R`, `auxiliary/useBs4Dash.R` |
| `R/fct_plot_prep.R` | `auxiliary/fun_plot_prep.R` — **but see 2b, this file duplicates `global.R`** |
| `R/fct_publications.R` | `auxiliary/fun_publications.R` (the `pub_function()` builder only — the `pubList <- readxl::read_excel("data/publicationsList.xlsx")` line moves into the module server in Phase 4, once the file lives in `inst/extdata/`) |
| `R/fct_remove_avg.R` | `auxiliary/fun_remove_avg.R` |
| `R/fct_extract_var.R` | `auxiliary/fun_extract_var.R` |
| `R/guides.R` | `auxiliary/guides.R` (as-is) |

### 2b. Do NOT port (dead code — confirmed unreferenced by `global.R` or anywhere else)

- `auxiliary/dynamic_benchmarking.R`
- `auxiliary/new_dynamic.R`
- `auxiliary/new_dynamic2.R`
- `auxiliary/testScript.R`
- `auxiliary/testScript2.R`

### 2c. De-duplicate the triple-defined functions

`customItem()`, `x_scatter_choices()`, and `remove_average_items()` are each defined twice in the
original app: once inline in `global.R`, once in an `auxiliary/*.R` file sourced afterward (the
second silently wins at runtime). Port **one** definition of each:

- `customItem()` — use the version from `auxiliary/fun_plot_prep.R` (identical to `global.R`'s
  copy, but this is the one that was actually active). Put it in `R/fct_helpers.R`.
- `x_scatter_choices()` — same story; `auxiliary/fun_plot_prep.R`'s version is the active one.
  Put it in `R/fct_helpers.R`.
- `remove_average_items()` — `auxiliary/fun_remove_avg.R`'s version is the active one (`global.R`'s
  inline copy is identical anyway). Put it in `R/fct_remove_avg.R`.

Delete the inline copies that lived in `global.R` — that file doesn't get ported (its logic moves
into `app_server.R`/module servers in Phase 4).

### 2d. Resolve the `cliaretl` drift — `compute_family_average()`

`auxiliary/fun_family_data.R` currently defines its own `compute_family_average()`:
```r
compute_family_average <- function(cliar_data, vars, type = "static", db_variables, base_country, comparison_countries)
```
`cliaretl` already exports a version with a different signature:
```r
cliaretl::compute_family_average(cliar_data, vars, type = c("static","dynamic"), db_variables, require_complete = TRUE, exclude_pattern = "gdp")
```
Key behavioral difference: the app-local version manually pre-filters `na_indicators` /
`lv_indicators` (missing + low-variance columns for the base country) *before* calling
`pivot_longer`/`summarise`, whereas `cliaretl`'s version relies on `require_complete` to decide
whether to `NA`-out an incomplete family average. **These are not drop-in equivalent** — the
app-local version also drops low-variance columns, which `cliaretl`'s does not.

Recommended resolution: keep a thin app-local wrapper in `R/fct_family.R` that does the
missing/low-variance filtering `cliaretl`'s version doesn't do, then delegates the actual
averaging math to `cliaretl::compute_family_average()`:

```r
# R/fct_family.R
compute_family_average_app <- function(cliar_data, vars, type = "static", db_variables,
                                        base_country, comparison_countries) {
  na_indicators <- cliar_data |>
    dplyr::ungroup() |>
    dplyr::filter(country_name %in% base_country) |>
    dplyr::select(-(1:5)) |>
    dplyr::summarise(dplyr::across(dplyr::everything(),
      ~ if_else(any(is.na(.)), NA, sum(., na.rm = TRUE)))) |>
    dplyr::select(where(is.na)) |>
    dplyr::distinct() |>
    names()

  lv_data <- cliar_data |>
    dplyr::filter(country_name %in% c(base_country, comparison_countries)) |>
    dplyr::select(-(1:5))
  lv_indicators <- names(Filter(check_quantiles, lv_data))

  cliar_data <- cliar_data |>
    dplyr::select(-c(union(na_indicators, lv_indicators))) |>
    dplyr::ungroup() |>
    dplyr::select(country_name, dplyr::everything())
  vars <- setdiff(vars, union(na_indicators, lv_indicators))

  cliaretl::compute_family_average(
    cliar_data, vars, type = type, db_variables = db_variables,
    require_complete = FALSE, exclude_pattern = "gdp"
  )
}
```

Then update every `server.R` call site (`data_avg`, `data_dyn_avg` reactives) from
`compute_family_average(...)` to `compute_family_average_app(...)`. Also keep `check_quantiles()`
(currently defined in `auxiliary/fun_quantiles.R`) — it's used by this wrapper.

**Verify this in Phase 6**: run both the old app and the new package side-by-side for the same
country/comparator selection and diff the family-average columns — this is the one piece of logic
that's genuinely being rewritten, not just relocated.

### 2e. Add tests

`fct_plots.R`, `fct_quantiles.R`, and `fct_downloads.R` are the most testable (mostly pure
data-in/data-out). Minimal starting point, e.g. `tests/testthat/test-fct-downloads.R`:

```r
test_that("dta_prep truncates and de-duplicates column names", {
  df <- data.frame(`a very long column name that exceeds thirty chars` = 1, check.names = FALSE)
  out <- dta_prep(df)
  expect_true(all(nchar(names(out)) <= 30))
})
```

---

## Phase 3 — Move static assets + data

| From (cliarapp) | To (cliarappak) |
|---|---|
| `www/*` | `inst/app/www/*` |
| `report.Rmd` | `inst/rmd/report.Rmd` |
| `coverage-report.Rmd` | `inst/rmd/coverage-report.Rmd` |
| `data/publicationsList.xlsx` | `inst/extdata/publicationsList.xlsx` |

Do **not** port `data/compiled_indicators.rds`, `data/coverage_ctf_for_analysis.rds`,
`data/indicators_map.rds` — these are all sourced from `cliaretl` at runtime already
(`cliaretl::compiled_indicators`, `fs::path_package("extdata", ..., package = "cliaretl")`, etc.),
they don't need to be duplicated into this package. `data/coverage_ctf_for_analysis.csv` is
produced by the unwired `source/data_coverage_processing.R` script — treat it the same way
(regenerate via that script against `cliaretl` when needed, don't check in a static copy).

### Path reference updates

Search-and-replace pattern across every ported file:

| Old | New |
|---|---|
| `includeCSS("www/styles.css")` | `includeCSS(app_sys("app/www/styles.css"))` (already handled in `golem_add_external_resources()` from Phase 1) |
| `img(src = "cliar.png", ...)` | unchanged — `src=` paths are resolved via `addResourcePath("www", ...)`, already wired in Phase 1 |
| `read_pptx("www/CLIAR_template.pptx")` | `read_pptx(app_sys("app/www/CLIAR_template.pptx"))` |
| `file.copy("www/", tmp_dir, ...)` + `file.copy("report.Rmd", tempReport, ...)` | `file.copy(app_sys("rmd/report.Rmd"), tempReport, overwrite = TRUE)` — the `www/` copy step becomes unnecessary once `report.Rmd`'s own `knitr::include_graphics()` calls are updated (see below) |
| `knitr::include_graphics("www/cliar.png")` (inside `report.Rmd`) | `knitr::include_graphics(system.file("app/www/cliar.png", package = "cliarappak"))` |
| `paste(here(), 'www', 'dashboard_userguide_outline_v5.2.docx', sep = "/")` | `app_sys("app/www/dashboard_userguide_outline_v5.2.docx")` |
| `file.copy("www/CLIAR Benchmarking.pdf", file)` | `file.copy(app_sys("app/www/CLIAR Benchmarking.pdf"), file)` |
| `pubList <- readxl::read_excel("data/publicationsList.xlsx")` | `pubList <- readxl::read_excel(app_sys("extdata/publicationsList.xlsx"))` — move this line from file-load-time into `mod_publications_server()`'s body (Phase 4) |
| `read_csv(here("data", "coverage_ctf_for_analysis.csv"))` (in `global.R`, → becomes `app_server.R`) | Point at wherever you decide to regenerate this — simplest is to keep computing it at package-load/session-start by calling the ported coverage functions directly against `cliaretl` data, rather than reading a static file. Flag this as a design call for Phase 4, not a mechanical rename. |

---

## Phase 4 — Modularize UI/server into `mod_*.R`

This is the one phase that's a genuine rewrite, not a mechanical port — full code for all ~11
tabs would be thousands of lines and isn't something to blindly copy without testing against a
live R session as you go. What follows is the wiring pattern plus two fully worked examples
(the trivial case and the "owns shared state" case) and a line-range map so you can extract the
rest of `server.R`/`ui.R` yourself, tab by tab, using the same pattern.

### 4a. The sharing problem, concretely

Almost every tab reads reactive state that lives in the "Country benchmarking" tab's server code
today (because it's all one flat `server()` function). The fix: `mod_benchmark_server()` returns
a named list of reactives; `app_server()` passes that list into every module that needs it.

```r
# R/app_server.R
app_server <- function(input, output, session) {
  bench <- mod_benchmark_server("benchmark")

  mod_country_comparison_server("country", bench)
  mod_bivariate_server("scatter", bench)
  mod_world_map_server("world_map", bench)
  mod_trends_server("trends", bench)
  mod_data_server("data", bench)
  mod_reports_server("reports", bench)
  mod_publications_server("publications")
}
```

```r
# R/app_ui.R (dashboardBody portion)


```

### 4b. Worked example 1 — trivial port (`mod_publications.R`)

`modules/mod_publications.R` is already module-shaped. Port near-verbatim; the only change is the
`pubList` load path from Phase 3:

```r
# R/mod_publications.R
mod_publications_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # ...unchanged from modules/mod_publications.R, just wrap ns() around every inputId/outputId...
  )
}

mod_publications_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    pubList <- readxl::read_excel(app_sys("extdata/publicationsList.xlsx"))
    sel_country <- reactive(input$country)
    output$publications <- renderUI({
      # ...unchanged body from publicationsServer()...
    })
  })
}
```

Call site in `app_server.R`: `mod_publications_server("publications")` (no `bench` argument needed
— this tab is genuinely independent).

### 4c. Worked example 2 — owns shared state (`mod_benchmark.R` skeleton)

```r
# R/mod_benchmark.R
mod_benchmark_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # ...port everything inside ui.R's tabItem(tabName = "benchmark", ...) here,
    # wrapping every inputId/outputId in ns(), e.g.:
    # pickerInput(ns("country"), ...) instead of pickerInput("country", ...)
    # plotlyOutput(ns("plot"), ...) instead of plotlyOutput("plot", ...)
  )
}

mod_benchmark_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # --- port server.R lines ~47-350 here (tour observers, load_inputs modal,
    #     base_country eventReactive) — replace bare `input$x` with same
    #     (moduleServer already scopes `input` to this module's namespace) ---

    base_country <- eventReactive(input$select, input$country, ignoreNULL = FALSE)

    # --- port server.R lines ~353-980 here (custom_group_fields_reactive,
    #     custom_grps_df, select_button renderUI, related observeEvents) ---

    # --- port server.R lines ~984-1250 here (vars, note_compare,
    #     low_variance_indicators(_dyn), data_avg, data, data_dyn_avg, data_dyn,
    #     data_family, data_family_dyn reactives) — call compute_family_average_app()
    #     per Phase 2d instead of the old local compute_family_average() ---

    # --- port server.R lines ~1325-1655 here (output$plot, output$plot_notes,
    #     output$dynamic_benchmark_plot) ---

    # --- port server.R lines ~2870-2880 here (output$definition) ---

    # --- port server.R lines ~2959-3095 here (cliar_inputs, output$save_inputs,
    #     download_data_1, output$download_data_1) ---

    list(
      base_country    = base_country,
      countries       = reactive(input$countries),
      groups          = reactive(input$groups),
      family          = reactive(input$family),
      threshold       = reactive(input$threshold),
      rank            = reactive(input$rank),
      benchmark_dots  = reactive(input$benchmark_dots),
      preset_order    = reactive(input$preset_order),
      custom_df       = custom_df,          # from the ported custom_grps_df logic
      data_avg        = data_avg,
      data            = data,
      data_dyn_avg    = data_dyn_avg,
      data_dyn        = data_dyn,
      data_family     = data_family,
      data_family_dyn = data_family_dyn
    )
  })
}
```

Consuming modules read `bench$base_country()`, `bench$countries()`, `bench$data_avg()`, etc.
wherever the old code referenced `base_country()`, `input$countries`, `data_avg()` from the shared
top-level `server()` scope. E.g. inside `mod_bivariate_server`:

```r
mod_bivariate_server <- function(id, bench) {
  moduleServer(id, function(input, output, session) {
    output$scatter_plot <- renderPlotly({
      static_scatter(global_data, bench$base_country(), input$countries_scatter, ...)
    })
  })
}
```

### 4d. Line-range map — VERIFIED against the live checkout (2026-08-19)

Every boundary below was checked directly against `ui.R`/`server.R` as they exist today (not
assumed from this guide's earlier draft). Result: the map is accurate. `ui.R`'s `tabItem(` /
`tabName` lines match exactly. `server.R`'s section-comment headers (`# Bar plot ===`, `# Scatter
plot ===`, `# Map ===`, `# Trends plot ===`, `#=== DATA DOWNLOAD`, `# Report ===`, `# Definitions
===`, etc.) and every named reactive/output referenced in 4c (`base_country` L73, `custom_grps_df`
L487, `data_avg` L1091, `data` L1122, `data_dyn_avg` L1140, `data_dyn` L1174, `data_family` L1191,
`data_family_dyn` L1214, `output$definition_bar` L2897) all fall inside their claimed ranges. The
only drift found was cosmetic (the "Scatter plot" comment header sits at L1962, not L1960 as
originally guessed) and needs no correction to the ranges themselves.

| Target module | `ui.R` tabItem lines | `server.R` lines |
|---|---|---|
| `mod_home.R` | 125–208 | — (no server logic) |
| `mod_benchmark.R` | 211–810 | 47–350, 353–980, 984–1250, 1325–1655, 2870–2880, 2959–3095 |
| `mod_country_comparison.R` | 814–953 | 1869–1958, 2897–2911 (`definition_bar`) |
| `mod_bivariate.R` | 957–1116 | 1960–2046, 2490–2543 (`download_bivariate_data`) |
| `mod_world_map.R` | 1254–1331 | 2048–2075 |
| `mod_trends.R` | 1122–1249 | 2077–2170 |
| `mod_data.R` | 1335–1643 | 2185–2489 (`pre_download_data` + `benchmark_datatable` + all `down_*`/`download_global_*` handlers) |
| `mod_reports.R` | (download buttons live inside `mod_benchmark.R`'s UI — this module has no `tabItem` of its own, just downloadHandlers) | 2548–2866 (`report`/`advreport`/`download_Coverage`/`pptreport`) |
| `mod_methodology.R` | 1647–1790 | 2915–2953 (`download_user_guide`/`download_indicators`/`download_metho`) |
| `mod_publications.R` | 1793–1796 | 2955–2956 (`publicationsServer("publications")` call — the module body itself is `modules/mod_publications.R`) |
| `mod_terms.R` | 1800–1822 | — (no server logic) |
| `mod_faq.R` | 1826–2041 | — (no server logic) |

Also port `server.R` lines 1657–1867 (the cross-tab country/group sync observers, e.g.
`country_bar` following `input$country`) — split these: the half that updates *this* module's own
inputs from `bench` stays in each consuming module's server (e.g. `mod_country_comparison_server`
observes `bench$base_country()` and calls `updatePickerInput(session, "country_bar", ...)`); you
no longer need the reverse-direction syncs back into the benchmark tab's own inputs, since
`mod_benchmark_server` is the single source of truth now (those existed originally only because
everything lived in one flat scope).

### 4d-1. Landmine found during verification — the `data` reactive name collision

**Not in the original guide. Read this before porting `mod_benchmark.R` and
`mod_country_comparison.R` together, and before porting `download_data_1`.**

`server.R` defines **two different reactives both literally named `data`** in the same flat
`server()` scope, because everything lives in one function today:

- **`server.R:1122`** — inside the "Reactive objects" block (984–1250, → `mod_benchmark.R`):
  ```r
  data <- eventReactive(input$select, {
    global_data %>% def_quantiles(base_country(), country_list, input$countries, vars_all,
                                   variable_names, input$threshold)
  })
  ```
  This is the benchmark tab's own quantile-transformed dataset — same shape/role as its siblings
  `data_avg`, `data_dyn`, `data_family`.

- **`server.R:1890`** — inside "Bar plot" (1869–1958, → `mod_country_comparison.R`):
  ```r
  data <- reactive({
    if (input$value_bar == "ctf") global_data
    else raw_data %>% select(-Year) %>% group_by(...) %>% fill(everything()) %>% slice(n())
  })
  ```
  This is a completely different, unrelated dataset used only for the cross-country bar chart.

Because R evaluates `server()` top to bottom in one scope, the **second definition at L1890
silently shadows the first for the rest of the function.** Any code below L1890 that calls
bare `data()` gets the *bar-chart* version, not the benchmark version — even if it was written
expecting the benchmark one.

**This is exactly what happens at `server.R:3027–3045` (`download_data_1`, inside the
2959–3095 range that ports into `mod_benchmark.R`):**

```r
download_data_1 <- eventReactive(input$select, {
  data1 <- data_family()   %>% filter(country_name == base_country())
  data2 <- data()          %>% filter(country_name == base_country())   # <- shadowed!
  data3 <- data_family_dyn() %>% filter(country_name == base_country())
  data4 <- data_dyn_avg()  %>% filter(country_name == base_country()) %>% filter(...)
  list(data1 = data1, data2 = data2, data3 = data3, data4 = data4)
})
```

`data1`/`data3`/`data4` all pull from sibling quantile-transformed reactives
(`data_family`/`data_family_dyn`/`data_dyn_avg`). By that same pattern, `data2` almost certainly
*meant* to reference the benchmark tab's own `data` (L1122) or `data_avg()`. Instead, at runtime
today it silently reads the bar-chart's `data()` (L1890) — whatever `global_data` or the
`raw_data`-derived grouped/sliced frame happens to be, filtered by `country_name`. The
"CTF-plot-data.xlsx" download's `data2` sheet has therefore likely never contained what its
sibling sheets' naming pattern implies it should.

**This is a decision for you, not something to silently "fix" during the mechanical port:**

1. **Preserve current (buggy) behavior** — once modules are split, `mod_benchmark_server`'s
   `data` reactive (L1122) will no longer be shadowed by anything (each module gets its own
   scope), so if you want bit-for-bit output parity with the shipped app, `download_data_1`'s
   `data2` line must be deliberately changed to call `bench_country$bar_data()` (see naming fix
   below) or equivalent — pointing it at the *country-comparison* tab's data, sourced from the
   module that now owns it — to reproduce what today's shadowed call actually returns.
2. **Fix it** — point `data2` at the benchmark tab's own `data()` (L1122) or `data_avg()`, matching
   the sibling-line pattern (`data1`, `data3`, `data4`), which is almost certainly what was
   intended. This changes the content of the `data2` sheet in the downloaded `.xlsx` going forward.

Recommend (2) unless you have a reason to believe downstream consumers depend on the current
(likely accidental) output — flag it to whoever owns report validation either way.

A scan for every other top-level `name <-` assignment in `server.R` (regardless of what follows —
`reactive(`, `eventReactive(`, or something else, single- or multi-line) found `data` is the
*only* name defined twice at the top level of the flat `server()` scope. So this is a one-off, not
a pattern to keep hunting for elsewhere in the port.

**Update, found while porting `mod_benchmark.R`: the sheet-naming pattern in `download_data_1`'s
own output resolves this with much higher confidence than "flag it and ask."** The four sheets in
the exported `CTF-plot-data.xlsx` are:

| Sheet label | Reactive | Reactive's own name pattern |
|---|---|---|
| "Static Overview" | `data1` = `data_family()` | `*_family` |
| "Static Family" | `data2` = `data()` (the shadowed one) | — breaks the pattern |
| "Dynamic Overview" | `data3` = `data_family_dyn()` | `*_family_dyn` |
| "Dynamic Family" | `data4` = `data_dyn_avg()` | `*_dyn_avg` |

Every row pairs a sheet labeled **"Overview"** with a `*_family*`-named reactive, and every row
pairs a sheet labeled **"Family"** with a `*_avg`-named reactive — except `data2`, which should by
this pattern be the static counterpart to `data4` (`data_dyn_avg`), i.e. **`data_avg()`**. That
lines up exactly with `output$plot`'s own two branches too: `input$family == "Overview"` renders
from `data_family()`, and the specific-family branch renders from `data_avg()`. This is no longer
just plausible — `data2` is the one sheet that breaks an otherwise-exact pattern across the other
three, and it breaks it in precisely the way a `data`/`data_avg` name collision would.

**Recommendation upgraded to a near-certainty: use option (2) from above.** In `mod_benchmark.R`,
`download_data_1`'s `data2` line is implemented as:
```r
data2 <- data_avg() %>% dplyr::filter(country_name == base_country())
```
not a reference to any `data`-named reactive. This sidesteps the whole collision rather than
picking a side of it -- there's no `data` reactive of either the L1122 or L1890 kind anywhere in
the ported module, so the question of which one `download_data_1` "should" mean doesn't arise.

**Regardless of which you choose, avoid recreating the collision:** rename the
`mod_country_comparison.R`-local reactive at L1890 to something distinct, e.g. `bar_data`, and
update its three in-module call sites (L1906 `check_data(data(), ...)`, L1941, and anywhere else
inside 1869–1958 that calls bare `data()`) to match. Once modules have their own scopes this
collision can no longer happen silently, but keeping the names distinct makes the code legible
and stops it from recurring if someone later merges logic back.

### 4e. Report module note

`mod_reports.R`'s `downloadHandler`s take the same `params` list they do today, just sourced from
`bench$...()` calls instead of bare reactives, and rendering `app_sys("rmd/report.Rmd")` /
`app_sys("rmd/coverage-report.Rmd")` per Phase 3.

---

## Phase 4 — EXECUTED (2026-08-19)

All 11 tabs ported to `mod_*.R`, wired through `app_ui.R`/`app_server.R`. Deviations from this
guide's plan, found only by actually executing the code (not just `load_all()`, which never calls
UI/server functions, only defines them):

- **`R/fct_app_data.R`** (new, not in this guide) — `build_app_data()` centralizes `global.R`'s
  data section into one function computed once in `run_app()` and threaded through
  `golem::get_golem_options("app_data")` into every module, per the Tier-1 explicit-argument rule.
  Two things `global.R` computed were dead code and dropped: the `family_order` join into
  `db_variables` (immediately overwritten by `vars-control.R`'s reassignment) and the 14 per-family
  `vars_*` vectors (grepped — zero references outside the file that defined them). `family_order`
  itself, unjoined, is kept — `server.R:1358` uses it directly.
- **`custom_df_bar` bug** (`server.R:1875-1885`, Cross-Country Comparison tab): checked
  `input$group_trends` — a different tab's input — instead of its own `groups_bar`, a copy-paste
  leftover from the Trends tab's identical `custom_df_trend`. Fixed on port.
- **Cross-module dependency beyond `bench`**: `mod_bivariate`'s `high_group` reactive
  (`server.R:1971-1979`) reads `mod_country_comparison`'s `custom_df_bar()` directly. Neither this
  guide nor a `bench`-only architecture anticipated that — `mod_country_comparison_server()` now
  returns a small list of its own, threaded into `mod_bivariate_server()` as a fourth argument.
- **`mod_reports_server` must share `mod_benchmark`'s "benchmark" namespace**, not use its own
  `"reports"` id as this guide's 4a example showed — its `downloadHandler`s render into buttons
  defined by `mod_benchmark_ui()`, and Shiny module namespacing is per-id, not per-function. Calling
  `moduleServer("benchmark", ...)` a second time from a different function is how Shiny supports
  splitting one module's server across files.
- **`server.R:2173-2184`** (populates `x_scatter` choices from `y_scatter`) sits physically between
  the Trends and Data Download sections but is logically part of Bivariate — missed by this guide's
  line-range map entirely.
- **Package-wide NAMESPACE gap, found by actually calling the ported functions**: `NAMESPACE` never
  declared `import(shiny)`/`import(golem)`/`import(bs4Dash)`/etc. `devtools::load_all()` +
  `devtools::document()` both stayed clean regardless, because neither one *calls* a UI/server
  function -- only defines it, so a missing import is invisible until something actually executes
  that code path. Added `R/cliarappak-package.R` with the needed `@import`/`@importFrom` tags, added
  the missing `waiter` dependency to `DESCRIPTION` (used via `waiter::` calls but never declared),
  and explicitly namespaced two call sites (`bs4Dash::actionButton`, `DT::dataTableOutput`) whose
  bare forms resolve ambiguously across imported packages -- for `actionButton` specifically,
  `bs4Dash`'s version supports a `status` arg for Bootstrap coloring that `shiny`'s does not, so the
  wrong resolution would have silently dropped the "Apply selection" button's success/warning
  styling. Verified past what `load_all()` can see: `build_app_data()`, every `mod_*_ui()`, `app_ui()`
  itself, and every `mod_*_server()` via `shiny::testServer()` all now execute cleanly end to end.
- Also found, not yet acted on: the guided-tour (`cicerone`) step selectors in `guides.R` target
  bare ids like `"show_countries_column"`/`"custom_grps_column"` and `[data-id='country']` written
  for the original's flat, unnamespaced ids. Left as-is since there's only one `mod_benchmark`
  instance, so it happens to still work, but worth a manual click-through check.

**Not yet done — needs a real browser session, which `testServer()` doesn't cover**: `golem::run_dev()`
and click through the Phase 6 checklist below. `testServer()` confirms every module's reactive/observer
*setup* code runs without error, but doesn't simulate a user driving inputs through a live session end
to end (e.g. `downloadHandler` content functions, which only run when a download is actually
triggered).

## Phase 4 — post-hoc bugfix round (2026-08-20)

The gap above ("needs a real browser session") turned out to matter: `testServer()`'s setup-only
coverage missed every bug in this section, because none of them live in code that runs before an
`output$*` is actually *rendered* -- and nothing renders until a real session sets enough inputs to
satisfy a `conditionalPanel`/`bindEvent` gate. First real user test (base country + a comparison
group + "Overview" + Apply Selection) hit a wall immediately: nothing rendered.

**Scaffolding gaps** (`golem::run_dev()` itself wouldn't start):
- `inst/golem-config.yml` and `dev/run_dev.R` don't exist -- `golem::create_golem()` generates both
  automatically; this repo's hand-rolled Phase 1 never did. Added both (standard golem defaults).

**`fct_plots.R`'s Tier-1 audit was never finished.** Phase 2 ported this file "as-is" without the
Tier-1 explicit-argument pass the other `fct_*.R` files got, and it was flagged as a known risk
during Phase 4 execution but not resolved for lack of time. It's the direct cause of the reported
bug: `static_plot()` referenced bare `db_variables`, `family_order`, and `ctf_long`; `static_plot_dyn()`
referenced bare `db_variables` and `ctf_long_dyn`; `static_bar()` referenced bare `ctf_long_dyn`. All
three now take these as explicit parameters, and every call site (`mod_benchmark.R`,
`mod_country_comparison.R`, `mod_reports.R`) passes the matching `app_data$...`/`bench$...` value.

**The NAMESPACE import fix from the first Phase 4 pass was incomplete.** That pass verified UI
construction and every module's reactive/observer *setup* code (via direct calls and
`shiny::testServer()`), which was real progress, but never actually triggered a plot/table
*render* -- exactly the code path bare-global bugs and missing imports hide in. Fixed by
actually rendering every tab's primary output (`output$plot`, `bar_plot`, `scatter_plot`, `map`,
`time_series`, `benchmark_datatable`) via `testServer()` with real inputs set, iterating on each
`could not find function` error:
- `ggplot2`, `tidyr`, `stringr`, `purrr`, `readr`, `sf` upgraded from narrow `@importFrom` lists (which
  kept missing individual functions one at a time) to full `@import`, matching the treatment `shiny`/
  `bs4Dash`/`dplyr` already got.
- `hrbrthemes::theme_ipsum` and `zoo::na.approx` added (`trends_plot()`) -- both packages were already
  in `DESCRIPTION` but never actually imported.
- **`forcats` was a genuinely undeclared dependency** (`fct_reorder()`, `static_bar()`) -- not in
  `DESCRIPTION` at all despite being a real, installed dependency of the original app (part of the
  `tidyverse` meta-package `library(tidyverse)` pulled in). Added.

## Phase 4 — download verification round (2026-08-20)

Every download button was actually triggered via `shiny::testServer()` (which, unlike a plain
`output$x` read, really does invoke a `downloadHandler`'s `content` function and hand back the
generated file's path) -- not just assumed to work because the surrounding module loaded. Data tab
(9/9 CSV/rds/dta downloads), Methodology tab (3/3), Bivariate chart data export, and both Word
reports (`report`/`advreport`) all now produce real, non-empty files. Along the way:

- **A genuine `data.table::setnames()` by-reference mutation bug** in `rds_prep()`/`csv_prep()`/
  `dta_prep()` (`fct_downloads.R`): `setnames()` mutates its input in place even though it's a
  plain `data.frame`, so downloading RDS then CSV then DTA in the same session (all with
  "descriptive column names" on) silently corrupted the shared, cached `pre_download_data()`
  reactive for the second and third downloads. Fixed by having each `*_prep()` function
  `data.table::copy()` its input first. Pre-existing in the original app (identical code), not a
  Phase 4 regression -- just never noticed because most people download one format at a time.
- **A real porting error**: `mod_benchmark.R`'s `data_family_dyn` reactive called
  `family_data_dyn()` with 4 args (copying the pattern from its sibling `family_data()`, which
  really does take 4), but `family_data_dyn()`'s actual signature only takes 3 -- confirmed against
  `server.R:1220-1224`. This one broke both Word reports outright.
- **`report.Rmd` had a leftover `source(file.path("auxiliary", "plots.R"))`** from the original
  script app -- doesn't exist in the package, and wasn't needed anyway once the fix below landed.
  Also two bare `variable_names` references (should be `params$variable_names`) and all four
  `static_plot()`/`static_plot_dyn()` calls needed the new `db_variables`/`family_order`/
  `ctf_long`/`ctf_long_dyn` arguments threaded through as `params$...` (added to the Rmd's YAML
  `params:` block and to `mod_reports.R`'s `params` lists).
- **The structural fix underneath all of the above**: `rmarkdown::render(..., envir =
  new.env(parent = globalenv()))` gives the Rmd's chunks NO access to `cliarappak`'s own internal
  functions (`static_plot()` etc. aren't exported) or even its own dependencies (`dplyr`,
  `stringr`, ...) unless the *end user's* session happens to have them separately attached -- the
  original script app never hit this because `global.R` had already `library()`'d everything into
  one shared session. Changed to `envir = new.env(parent = asNamespace("cliarappak"))` in all four
  `rmarkdown::render()` calls, which gives the Rmd the exact same name resolution the package's own
  R code has, with nothing to export and nothing left to the end user's session.
- **`reference_docx: www/template.docx`** in `report.Rmd`'s YAML header is a relative path that
  pandoc (not R) resolves against the temp directory -- Phase 3's removal of the blanket
  `file.copy("www/", tmp_dir, recursive = TRUE)` was correct for the `include_graphics()` calls but
  broke this one. Fixed with a *targeted* copy of just `template.docx` into `tmp_dir` (not the
  whole `www/` folder) in `mod_reports.R`, and changed the YAML path to `template.docx` to match.

**Where `coverage_ctf_for_analysis.csv` actually came from (traced via `cliarapp/source/`, not
`cliaretl`)**: it was never generated by any `cliaretl` function -- `cliarapp/source/
data_coverage_processing.R` is a one-off script, run manually each data-release cycle, that reads
`compiled_indicators.rds`, filters to a hardcoded 5-year window ("2025 release, should take the
static 2020 to 2024 period" -- the same class of yearly-manual-edit magic number flagged earlier
for `fct_app_data.R`'s `year <= 2024`), pivots long, and joins in indicator metadata from
`cliaretl::db_variables`. Ported as a new exported function, **`prepare_app_data_coverage()`**
(`fct_app_data.R`), called lazily from `mod_reports.R`'s Coverage Report handler instead of reading
a static file. Its `year_window` argument defaults to `NULL`, in which case it's derived from
`db_variables`'s own `ref_year` attribute (`ref_year = 2025` reproduces `2020:2024` exactly) --
self-updating every data cycle instead of a number a person has to remember to bump. The static
`inst/extdata/coverage_ctf_for_analysis.csv` (79MB) is deleted; nothing in `cliarappak` reads a
coverage CSV anymore.
- **`coverage-report.Rmd`'s own setup chunk also had dead weight**: `library(tidyverse)` plus nine
  more `library()` calls inherited from the original script app. `tidyverse` isn't even installed
  here (deliberately -- Phase 1 unpacked it into individual packages), so this failed outright once
  the `source()`-removal above stopped masking it. Of the rest, only `flextable`'s functions are
  actually called anywhere in the file (confirmed by grep) -- `janitor`, `RColorBrewer`, `here`,
  `readxl`, `haven`, `tibble`, `countrycode`, `sf`, `dplyr` (redundant, already covered by the
  `asNamespace()` fix) were all unused. Trimmed to just `library(flextable)` (a real, declared
  `Suggests` dependency -- installed and confirmed working, not just assumed).

**PPT report and Benchmark's `download_data_1`/`save_inputs`, tested separately**: all three passed
with no further fixes needed -- `pptreport` doesn't use `rmarkdown::render()` at all (it calls
`static_plot()`/`static_plot_dyn()` directly), so it was already covered by the earlier signature
fixes. Every download button in the app is now verified end-to-end via `shiny::testServer()`, not
just loaded.

**Verified, not just loaded**: every tab's primary output was actually rendered end to end via
`shiny::testServer()` with realistic inputs (not just defined/parsed) -- Benchmark (`Overview` and a
specific-family branch), Cross-Country Comparison, Bivariate, World Map (both data sources), Time
Trends, and the Data tab's table. All pass except one narrow, pre-existing issue found along the
way, not a Phase 4 regression:

**Known issue, not fixed (flagging, not guessing)**: `static_bar()`'s `fct_reorder(country_name,
get(varname), min)` (unchanged from the original `plots.R`) can throw
`` `idx` must contain one integer for each level of `f` `` when the selected Cross-Country
Comparison indicator has heavy `NA` coverage across the selected comparison countries -- reproduced
with a family-average indicator where 7 of 15 test countries were `NA`. `output$bar_plot`'s
`validate(need(check_data(...)))` guard only checks the *base country*, not the full comparison set,
so a sparse indicator can still reach this. Since the line is byte-for-byte unchanged from the
original app, this is either a latent bug that predates the migration or not actually reachable in
normal usage (real users may avoid sparse indicators via `check_data`'s per-country signal
elsewhere) -- needs a real click-through in the Cross-Country Comparison tab with a few different
indicators to know which, not a speculative fix.

---

## Phase 5 — (folded into 4e above)

---

## Phase 6 — Verification checklist

```r
devtools::document()
devtools::load_all()
golem::run_dev()
```

Click through, in order:

- [ ] Home tab renders
- [ ] Benchmark tab: select base country + ≥10 comparators → "Apply selection" enables →
      static plot renders, notes render, dynamic plot renders (single base country only)
- [ ] Custom groups: create 1–3 custom groups, save, appear in comparison-group picker and
      group-median picker
- [ ] Save Selection of Countries → download `.rds` → Load Selection of Countries → re-upload →
      inputs repopulate correctly
- [ ] Cross-Country Comparison bar chart renders and follows benchmark tab's base country
- [ ] Bivariate Correlation scatter renders, linear fit toggle works, chart-data download works
- [ ] World Map renders for both "Closeness to frontier" and "Original indicator" sources
- [ ] Time Trends renders and follows benchmark tab's base country
- [ ] Data tab: table renders, all format downloads work (rds/csv/dta), all `down_*` CSV buttons
      work
- [ ] Reports: "Editable report" (Word), "Advanced Report" (Word), "PPT report", "Coverage report"
      all render and download without error
- [ ] Methodology tab downloads (user guide docx, indicator CSV, methodology PDF) work
- [ ] Publications tab: country filter works, cards render with images/links

```r
devtools::check()
testthat::test_dir("tests/testthat")
```

`devtools::check()` should be clean of `R CMD check` NOTES about undefined globals — use
`utils::globalVariables(c(...))` at the top of each `fct_*.R`/`mod_*.R` file for the
dplyr/tidyeval column names referenced via non-standard evaluation (same pattern `cliaretl`
already uses in its `R/*.R` files — see e.g. `cliaretl/R/ctf_funs.R` line 5).

**Behavioral diff check** (per Phase 2d): for one country/comparator selection, generate the
static benchmark plot data (`data_avg()` / `bench$data_avg()`) from both the original `cliarapp`
and the new package, and diff the family-average (`*_avg`) columns — this is the one piece of
logic that changed, not just moved.

---

## Post-migration — the `year <= 2024` cutoff resolved (2026-08-20)

The last flagged manual step (Part 1/Part 3 of the Field Guide artifact) is fixed. `fct_app_data.R`
gains a new exported function, **`resolve_dynamic_year_cutoff(dynamic_year_cutoff = NULL)`**: `NULL`
derives a year from `cliaretl::db_variables`'s `ref_year` attribute (`ref_year - 1`, matching the
same self-updating pattern already used for `prepare_app_data_coverage()`'s window); an explicit
year overrides it. `build_app_data()` gained a `dynamic_year_cutoff = NULL` parameter that calls
this resolver and uses the result in place of the old hardcoded `filter(year <= 2024)` on
`global_data_dyn` -- and now returns the resolved value as `app_data$dynamic_year_cutoff` too, so
any module (or a future admin/about page) can display which vintage is live. `run_app()` gained the
same named `dynamic_year_cutoff = NULL` argument, threaded straight through to `build_app_data()`.

This was a deliberate design choice, not the first one proposed: an environment variable or a
required deploy-time argument would each still leave a human in the loop remembering to set the
right value every cycle -- just relocated to infra config instead of source code, and less visible
there than a line in a diff. The chosen shape auto-updates with zero action in the common case,
while still exposing an explicit override for deliberate use -- the team noted they *frequently*
want to preview different cutoffs, which a purely-automatic derivation with no override would have
made harder, not easier.

**Why `run_app()` specifically, not buried in `...`**: `deploy_app()` (below) needed to bake a
specific, resolved cutoff into whatever entry file actually ships -- Connect restarts/scales the
deployed process on its own schedule, so an argument passed once at deploy time doesn't survive
unless the deployed code itself contains it. `resolve_dynamic_year_cutoff()` is exported and kept
separate from `build_app_data()` specifically so `deploy_app()` can call it directly (to know what
value it's about to write into a generated entry file) without needing to run the full data-loading
pipeline just to resolve one integer.

## `deploy_app()` built (2026-08-20)

`R/deploy_app.R`, following the same shape as an existing `deploy_govhrapp()` in a sibling
World Bank package (the user's own org convention, not invented here) -- `type = c("dev", "prod")`
resolving a Posit Connect app GUID from an environment variable (`cliarappak_dev_guid` /
`cliarappak_prod_guid`), `appDir = "."`, `forceUpdate = TRUE`, deploying via
`rsconnect::deployApp()`. Two departures from that template, both deliberate:

- **No `suite` argument** -- `cliarappak` is one app, not several, so `deploy_govhrapp()`'s
  suite/GUID matrix collapses to a single `type` dimension.
- **The entry file is generated, not static.** `deploy_govhrapp()` points `appPrimaryDoc` at a
  pre-existing, hand-written file. `cliarappak`'s `inst/app/cliarappak_app.R` doesn't exist as
  source -- `write_deploy_entrypoint()` (internal, `@noRd`, in the same file) regenerates it on
  every `deploy_app()` call, deparsing every `run_app()`-shaped argument (`onStart`, `options`,
  `enableBookmarking`, `uiPattern`, the resolved `dynamic_year_cutoff`, and any `...` extras) into
  a literal `cliarappak::run_app(...)` call. This is the mechanism that makes a chosen cutoff
  survive Connect's own restart/scale schedule, not just the one deploy that set it. The file is
  `.gitignore`d and `.Rbuildignore`d as a build artifact, the same way `rsconnect/`'s own
  bookkeeping folder is.

**Verified without a live Connect connection** (none available here): `write_deploy_entrypoint()`
produces a file that actually parses as valid R with the correct argument values; `deploy_app()`
errors clearly when the relevant GUID environment variable isn't set; and, with
`rsconnect::deployApp()` mocked via `testthat::with_mocked_bindings()`, `deploy_app(type = "dev",
dynamic_year_cutoff = 2021)` resolves the right GUID, generates a correct entry file with `2021L`
baked in, and calls `deployApp()` with exactly the expected `appDir`/`appId`/`appPrimaryDoc`/
`server`/`forceUpdate` arguments. The actual network call to Posit Connect itself is the one thing
that couldn't be exercised here.

---

## `renv` adopted, entry file simplified, and a version-upgrade regression check (2026-08-21)

The project now has `renv` set up (`renv.lock` + `renv/`) for reproducible dependency management —
`renv::restore()` installed all 179 locked packages cleanly, including `cliaretl` itself (built from
source successfully despite an initial "GitHub authentication credentials are not available"
warning during dependency resolution). `.Rbuildignore` gained
`^renv\.lock$` entries so `R CMD build` doesn't try to bundle them. Two things `renv::status()`
flags are expected, not bugs: `cliarappak` itself shows as "used but not installed" (we develop it
via `devtools::load_all()`, never `R CMD INSTALL` it into the renv library), and the lockfile was
generated against R 4.6.0 while this machine runs 4.5.2 (worth reconciling eventually, but
`restore()` succeeded despite it).

**`deploy_app()`'s generated entry file simplified**: `write_deploy_entrypoint()` now writes bare
`run_app(...)` instead of `cliarappak::run_app(...)`, since `library(cliarappak)` is already the
line above it in the generated file. The fully-qualified form was kept initially for robustness
against a same-named function from another attached package ever shadowing it, and for a reader
not having to trace back to the `library()` line to know whose `run_app` is running -- both still
true in principle, but the team preferred the shorter, more standard-looking form for a file this
size, and the masking risk is negligible for a single-purpose deployment entry file.

**Restoring `renv` pulled in materially newer versions of several core dependencies** (`ggplot2`
3.x -> 4.0.3, `shiny` -> 1.14.0, `dplyr` -> 1.2.1, among others) -- worth a real regression check,
not an assumption that semver held. Re-ran the full multi-tab `shiny::testServer()` suite (every
tab's primary output, plus `build_app_data()` and `deploy_app()`'s entry-file generation) against
the restored environment. Everything still passes, **including the already-known
`fct_reorder()`/sparse-indicator issue in `static_bar()`** (still present, unchanged, confirming
it's the same pre-existing issue and not a new regression from the version bump) --
**one new, non-fatal finding**: rendering the benchmark plot now emits `Using \`size\` aesthetic
for lines was deprecated in ggplot2 3.4.0. Please use \`linewidth\` instead.` `fct_plots.R` uses the
now-deprecated `size` aesthetic for line geoms somewhere -- still functional today (a warning, not
a removed feature, in 4.0.3), but worth fixing before some future `ggplot2` release actually drops
it. Not yet located or fixed; flagging for whoever's next in `fct_plots.R`.

---

## Phase 7 — Docs

- `DESCRIPTION` Title/Description already set correctly in Phase 1 — no placeholder text remains.
- Add a `README.Rmd`/`README.md` describing: what CLIAR is, the golem module structure, how to
  run (`cliarappak::run_app()`), and the dependency on `cliaretl` for data.
