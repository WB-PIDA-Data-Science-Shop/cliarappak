#' terms of use module UI
#'
#' Static content only -- no server logic.
#'
#' @param id a unique identifier for this module.
#'
#' @return a `tagList` of UI elements
#' @export
mod_terms_ui <- function(id) {
  ns <- NS(id)

  tagList(
    box(
      width = 12,
      status = "navy",
      collapsible = FALSE,
      title = "Terms of use and Disclaimer",
      solidHeader = TRUE,
      tags$ul(
        tags$li(
          'We ask that all users of the data to cite the data as follows:',
          HTML('"<em>Source: World Bank CLIAR Dashboard.</em>"')
        ),
        tags$li(
          "We kindly request that if users modify the methodology and the source code for their reports and analyses clearly state so and highlight the relevant departures from the CLIAR Benchmarking methodology."
        ),
        tags$li(
          "Disclaimer: The findings, interpretations, and conclusions expressed in CLIAR are a product of staff of the World Bank, but do not necessarily reflect the views of the World Bank and its affiliated organizations, or those of the Executive Directors of the World Bank or the governments they represent. Moreover, country borders or names used and available in this dashboard do not necessarily reflect the World Bank Group's official position, and do not imply the expression of any opinion on the part of the World Bank, concerning the legal status of any country or territory or concerning the delimitation of frontiers or boundaries. The term country, used interchangeably with economy, does not imply political independence but refers to any territory for which authorities report separate social or economic statistics."
        )
      )
    )
  )
}
