# `cliarappak` documentation guide

How to write roxygen2 documentation for every function in this package.

**Audience for the docs you write:** the CLIAR development team. Assume they are
comfortable R users (dplyr, the tidyverse, writing functions) but **not** Shiny
experts. Anything Shiny-, golem-, or plotly-specific needs a sentence of plain
explanation, not just a link to those packages' docs.

**Why we're doing this:** most of these functions are genuinely reusable and we
expect to call them from sibling packages in the same GitHub org
(`cgjrapp`, `cgjrdata`). Documentation + a stable export surface is what makes
that safe.

---

## 1. The target: what "well documented" means here

Every function gets a roxygen block with, in this order:

```r
#' <Title — one line, sentence case, no trailing period>
#'
#' <Description — 1–3 sentences: what it does and when you'd reach for it.>
#'
#' @details
#' <Optional but encouraged for anything non-obvious. This is where the house
#' style lives: narrative prose explaining *why* the function is shaped the way
#' it is, what upstream data it assumes, what the tricky edge cases are. See
#' `R/fct_app_data.R` and `R/run_app.R` for the tone we're matching.>
#'
#' @param arg1 <Type. Role. Where it comes from / what produces it.>
#' @param arg2 <...>
#'
#' @return <Type and shape. For Shiny module servers that return nothing
#'   useful, write: "NULL, invisibly; called for its side effects.">
#'
#' @seealso [related_function()], [other_pkg::function()]
#'
#' @export
```

### Rules

- **Title**: one line, no period, describes the function not the return value.
  For module halves use `"<tab name> module UI"` / `"<tab name> module server"`
  (matches what's already in `R/mod_*.R`).
- **Description**: always present. Even one sentence beats nothing.
- **`@param`**: **document every argument**, in signature order. Say the
  expected **type**, the **role** it plays, and — this is the part reviewers
  keep asking for — **what produces the value** (e.g. "the reactive returned by
  `mod_benchmark_server()`", "a column from `cliaretl::db_variables_final`",
  "one of `\"Default\"` or `\"Terciles\"`"). If several functions share a
  signature, still document each one (or use `@inheritParams`, see §4).
- **`@return`**: always present. Give the class and the columns/elements a
  caller can rely on. `invisible(NULL)` side-effect functions still get a
  `@return` line saying so.
- **`@details`**: use it whenever the function has non-obvious assumptions,
  historical baggage, or a design decision worth recording. This package
  already has a strong house style for this (long, plain-English, explains the
  migration history) — keep it up.
- **`@examples`**: optional. Most functions here need a fully built `app_data`
  list or a `cliaretl` dataset to run, so a runnable example is often more
  noise than signal. When an example genuinely helps, wrap it in
  `\dontrun{}` or `@examplesIf interactive()`. Do not add fake examples just
  to fill the tag.
- **`@keywords internal`**: add it (together with `@noRd` **only** when you do
  *not* want an `.Rd` page at all — see §3) for helpers that are exported for
  technical reasons but shouldn't show up in the package index.

---

## 2. Export policy

The decision we made: **export the reusable surface, keep golem's plumbing
internal.**

| Category | Files | Tag |
|---|---|---|
| Analytical / data / download helpers | `fct_quantiles.R`, `fct_family.R`, `fct_app_data.R`, `fct_downloads.R`, `fct_extract_var.R`, `fct_remove_avg.R`, `fct_helpers.R`, `fct_publications.R` | `@export` |
| Plot + map + legend builders | `fct_plots.R` | `@export` |
| Public entry points | `run_app.R`, `deploy_app.R` | `@export` |
| Shiny modules (both halves) | every `mod_*_ui`, `mod_*_server` | `@export` |
| golem plumbing | `app_ui()`, `app_server()`, `app_sys()`, `get_golem_config()`, `golem_add_external_resources()` | `@noRd` (document with `#'` but no `@export`, no `.Rd`) |
| Package-level doc | `cliarappak-package.R` | `@keywords internal` (already correct) |

Notes:

- **Modules are exported even though it's unconventional for golem.** That's a
  deliberate call so `cgjrapp` can reuse them. Because of this, be extra
  careful that each module's `@param` docs state exactly what shape of
  `app_data` / `bench` it needs — a reuser has no `global.R` to read.
- **Watch for generic names in the exported surface.** `fct_plots.R` exports
  `static_plot()`, `static_bar()`, `static_map()`, `interactive_plot()`, etc.
  Those are broad names to put in a shared namespace. We are shipping them
  as-is for now; if a collision ever bites in `cgjr*`, the fix is a package
  prefix (`cliar_static_plot()`), not un-exporting. Flag it, don't silently
  rename.
- **`compute_family_average()` must not be exported** — see §6.

---

## 3. `@export` vs `@noRd` vs `@keywords internal`

Three states, don't mix them up:

| You want... | Tags | Result |
|---|---|---|
| Public API | `@export` | `.Rd` page + entry in NAMESPACE + shows in index |
| Documented but not public, still worth an `.Rd` page | `@keywords internal` | `.Rd` page built, hidden from index, **not** exported |
| Not public, no `.Rd` page at all | `@noRd` | roxygen block is for the source reader only |

For this package:
- golem plumbing (`app_ui`, `app_sys`, …) → `@noRd`. Write a real roxygen
  block so the next developer understands it, but don't generate a manual page.
- Nothing else should need `@keywords internal` alone unless you decide a
  helper is "exported by policy but don't advertise it".

---

## 4. Shared signatures: `@inheritParams` and `@rdname`

Lots of functions here come in `_static` / `_dyn` pairs or `mod_*_ui` /
`mod_*_server` pairs with near-identical arguments.

**Same arguments, want one man page for both:** use `@rdname`.

```r
#' Family-level closeness-to-frontier aggregates
#'
#' `family_data()` works on the static (single latest period) dataset;
#' `family_data_dyn()` does the same per year for the dynamic dataset.
#'
#' @param data ...
#' @param base_country ...
#' @return A wide tibble, one row per country (per year for `_dyn`), one
#'   column per institutional family.
#' @export
family_data <- function(data, base_country, variable_names, comparison_countries) { ... }

#' @rdname family_data
#' @export
family_data_dyn <- function(data, base_country, variable_names) { ... }
```

**Same arguments, want separate pages but no copy-paste:** use
`@inheritParams`.

```r
#' @inheritParams def_quantiles
#' @param threshold ignored by this variant
#' @export
low_variance <- function(data, base_country, country_list, comparison_countries, vars, variable_names) { ... }
```

Rule of thumb: `_static`/`_dyn` pairs → `@rdname` (one page, describe the
difference in the shared description). `mod_*_ui`/`mod_*_server` → keep the two
pages we already have (they're conceptually different things), use
`@inheritParams` for the `id` / `app_data` args if it saves repetition.

---

## 5. Documenting Shiny modules

Each `mod_*.R` file has a `_ui` and a `_server`. The baseline block (title,
`@param id`, `@param app_data`, `@return`, `@export`) is already in place for
all of them. What's missing and what you should add:

1. **A description that says what the tab does for the end user** — one or two
   sentences. "Cross-Country Comparison tab: horizontal bar chart of one
   indicator's closeness-to-frontier for the base country against selected
   comparison countries and group medians."
2. **The cross-module contract, in `@details`.** Which reactives does this
   server read from `bench`? Which does it expose for others? `mod_benchmark`
   owns the shared selection state; every other server receives it as `bench`.
   Spell out the specific elements used (`bench$country()`,
   `bench$custom_grps_df()`, …) so a reuser knows what to supply.
3. **`@param bench`** — "Named list of reactives returned by
   `mod_benchmark_server()`. This module reads `bench$country()`,
   `bench$groups()`, `bench$select_trigger()`."
4. **Internal reactives stay as plain `#` comments**, not roxygen. Don't try to
   `@section` every `reactive()` inside the server. A one-line `#` comment
   above each non-obvious reactive is the right level — see the cross-tab-sync
   comments already in `mod_trends.R` and `mod_country_comparison.R`.
5. **`@return` for servers:** `"NULL, invisibly. Called for its side effects
   (registers outputs and observers on `session`)."` — unless the server
   returns a list of reactives (like `mod_benchmark_server()` does), in which
   case document that list element by element.

`R/mod_trends.R` is the worked example — see §7.

---

## 6. Code issues to fix while you document

These surfaced during the doc pass. Fix them in the same PR as the docs for the
file they live in.

### `R/fct_family.R`

- **`compute_family_average()` (≈line 90) is dead code and must go.** It is a
  standalone reimplementation of family averaging that **nothing in the package
  calls**, and its name shadows `cliaretl::compute_family_average()` — which
  *is* the real implementation. The live wrapper is
  **`compute_family_average_app()`**: it does the `na_indicators` /
  low-variance pre-filtering, then delegates to
  `cliaretl::compute_family_average(..., type = type, require_complete = FALSE,
  exclude_pattern = "gdp")`. The `type = "static" | "dynamic"` switch you
  remembered lives in `compute_family_average_app()` and is passed straight
  through to cliaretl. **Action:** delete the local `compute_family_average()`;
  keep and document `compute_family_average_app()`; never `@export` a
  `compute_family_average` from this package.
- **`check_quantiles()` is defined twice, identically** (≈line 1 and ≈line
  165). Delete the second copy. Document the survivor (`@export`,
  `@keywords internal`). Note there's a third copy in
  `tests/testthat/test-fct-downloads.R` — once `check_quantiles()` is exported,
  that test copy can be deleted too.

### `R/fct_plots.R`

- **`annotations` (≈line 2065)** is a stray top-level `list(...)` assignment
  that nothing reads. Delete it.
- **Top-of-file constants** (`note_size`, `note_chars`, `color_groups`,
  `color_countries`, `plotly_remove_buttons`) are not functions. Leave them as
  plain assignments with a `#` comment each. If you want them on a manual page,
  document `plotly_remove_buttons` as an internal data object with an explicit
  `@name` / `@keywords internal` block; the colours and sizes aren't worth it.
- **`custom_grp_median_data_func`** is defined three separate times as a local
  closure inside `static_plot()`, `static_plot_dyn()`, and `trends_plot()`.
  Don't document it (it's local). If you refactor it out to a real function
  later, that one gets `@keywords internal` + `@export`.

### `R/fct_helpers.R`

- **`buttons_func()`** has a malformed roxygen block: two `@param` lines, the
  second reads `@param id icon. Defaults to None` (wrong name, and the arg
  doesn't exist). Fix to just `@param id`, `@param lab`.
- `modal_function()` / `toast_messages_func()` are `@noRd` today. Under the new
  policy they should be `@export` + `@keywords internal` (they're the kind of
  helper `cgjrapp` would want).

---

## 7. Worked examples — copy the pattern from these three files

After this guide's PR, these are fully documented to the target standard:

| File | What it demonstrates |
|---|---|
| `R/fct_quantiles.R` | Converting existing `#` comment blocks to roxygen; `_static`/`_dyn` pairs via `@rdname`; documenting the `threshold` enum; `@return` for "a character vector of variable names" helpers. |
| `R/fct_plots.R` | Documenting large functions with 10–15 args; `@param` for args that are really "a column name string"; nested/local closures; generic-name exports; plotly post-processing helpers (`clean_plotly_legend()`, `fixfacets()`). |
| `R/mod_trends.R` | The Shiny module pattern: `@details` cross-module contract, documenting `bench`, `#`-comment level for internal reactives, `@return NULL` for the server. |

---

## 8. Per-file worklist

Order suggested: `fct_*` first (highest reuse value), then `mod_*`, then
`app_*`. `[done]` = brought to standard in the docs-guide PR.

### `fct_*`

| File | Functions | Status / notes |
|---|---|---|
| `fct_quantiles.R` | `def_quantiles`, `def_quantiles_dyn`, `low_variance`, `low_variance_dyn`, `missing_var`, `missing_var_dyn` | **[done]** — use as template |
| `fct_plots.R` | `static_plot`, `static_plot_dyn`, `plot_notes_function`, `interactive_plot`, `static_map`, `interactive_map`, `trends_plot`, `static_bar`, `interactive_bar`, `static_scatter`, `interactive_scatter`, `clean_plotly_legend`, `fixfacets` | **[done]** — use as template. Also delete stray `annotations` object. |
| `fct_family.R` | `check_quantiles`, `family_data`, `family_data_dyn`, `compute_family_average_app` | TODO. Delete dead `compute_family_average` + duplicate `check_quantiles` first (§6). `family_data` + `family_data_dyn` → `@rdname`. |
| `fct_extract_var.R` | `extract_variables`, `extract_variables_benchmarked` | Has good blocks already. Add `@return` (character vector of `var_name`s) and `@rdname` to pair them. |
| `fct_helpers.R` | `buttons_func`, `user_data_dir`, `check_input_file_exists`, `toast_messages_func`, `modal_function`, `useBs4Dash`, `customItem`, `x_scatter_choices` | Mostly has blocks. Fix `buttons_func` (§6). `customItem` has no block. Flip `@noRd` → `@export`+`@keywords internal` on the toast/modal helpers. `useBs4Dash` already `@export`. |
| `fct_downloads.R` | `make_colnames_unique`, `dta_prep`, `rds_prep`, `csv_prep`, `check_data`, `trends_check_data`, `check_spatial_data` | TODO — no roxygen. The `*_prep` trio share a signature → `@inheritParams` or `@rdname`. Document the `des_names` flag (labels vs codes). |
| `fct_app_data.R` | `build_app_data`, `resolve_dynamic_year_cutoff`, `prepare_app_data_coverage` | `build_app_data` + `resolve_dynamic_year_cutoff` **already excellent** — leave them, match their tone elsewhere. Check `prepare_app_data_coverage` has full `@param`/`@return`. |
| `fct_remove_avg.R` | `remove_average_items` | TODO — one function, trivial. `@export`. |
| `fct_publications.R` | `pub_function` | Has a block; add `@param link` (missing) and confirm `@return`. |

### `mod_*` (all have the baseline block; add description + `@details` contract)

`mod_benchmark.R` (**already the best-documented module — match it**),
`mod_bivariate.R`, `mod_country_comparison.R`, `mod_data.R`, `mod_faq.R`,
`mod_home.R`, `mod_methodology.R`, `mod_publications.R`, `mod_reports.R`
(server only — no `_ui`), `mod_terms.R`, `mod_trends.R` **[done]**,
`mod_world_map.R`.

For each: (1) one-line description of the tab's purpose, (2) `@details`
listing the `bench$*` reactives it consumes, (3) `@return` — `NULL` invisibly,
except `mod_benchmark_server()` which returns the shared-state list (document
every element).

### `app_*` / entry points

| File | Functions | Notes |
|---|---|---|
| `run_app.R` | `run_app` | **Already excellent.** |
| `deploy_app.R` | `write_deploy_entrypoint`, `deploy_app` | Check both have full blocks; `write_deploy_entrypoint` → `@noRd`. |
| `app_ui.R` | `app_ui`, `golem_add_external_resources` | `@noRd` both. Short block explaining `app_ui` reads `app_data` from `get_golem_options()`. |
| `app_server.R` | `app_server` | `@noRd`. Block should list the module servers it wires and the `bench` object it threads between them. |
| `app_config.R` | `app_sys`, `get_golem_config` | `@noRd` both (golem stock helpers). |
| `cliarappak-package.R` | — | Done. `@keywords internal`. |
| `zzz.R` | — | `utils::globalVariables()` only. No change; when you add NSE columns in new code, add them here (comment already explains why). |
| `guides.R` | `guide_*` cicerone objects | Not functions. One `#` header comment per guide is enough. |

---

## 9. Workflow

1. Edit the roxygen blocks in `R/`.
2. `devtools::document()` — regenerates `NAMESPACE` and `man/*.Rd`.
3. Commit the regenerated `man/*.Rd` and `NAMESPACE` **together with** the `R/`
   changes (this repo commits `man/`).
4. `devtools::check()` — must stay clean. New exports can surface new
   "undefined global" notes → add the offending NSE column names to
   `R/zzz.R`'s `globalVariables()` call (not per-function `@importFrom`).
5. Do **not** add per-function `@importFrom` for the packages already handled
   centrally in `R/cliarappak-package.R` — that file's long comment explains
   the policy. New imports for a genuinely new dependency go there too.

## 10. Checklist per function

- [ ] Title line, no period
- [ ] Description (≥1 sentence)
- [ ] `@details` if anything is non-obvious
- [ ] Every argument has `@param` — type, role, source
- [ ] `@return` — class + shape (even if `NULL`)
- [ ] `@export` **or** `@noRd`, per §2/§3
- [ ] `[func()]` cross-links resolve (`devtools::document()` warns if not)
- [ ] `devtools::check()` clean
- [ ] `man/` + `NAMESPACE` regenerated and committed
