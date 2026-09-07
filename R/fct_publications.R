#' Build one publication card for the Publications tab
#'
#' Returns the HTML for a single publication card: a linked container
#' (`class = "pubs"`) holding an image block (`class = "pubs_image"`) and a
#' metadata block (`class = "pubs_content"`) with the title, country, year and
#' authors. The whole card is an anchor that opens `link` in a new tab.
#'
#' @details
#' The Publications tab calls this once per row of `data/publicationsList.xlsx`
#' (maintained in OneDrive and copied into `inst/app/data/` for each release).
#' Styling for the `.pubs*` classes lives in [mod_publications_ui()].
#'
#' @param image Path or URL to the publication's card image.
#' @param link URL the card links to (opened in a new tab).
#' @param title Publication title.
#' @param country Country the publication covers.
#' @param year Publication year.
#' @param authors Publication authors.
#'
#' @return A `shiny.tag` (`<a>`) publication card, linked to `link`.
#'
#' @seealso [mod_publications_ui()], [mod_publications_server()].
#' @export
pub_function <- function(image,
                         link,
                         title,
                         country,
                         year,
                         authors) {

  ## A link that redirects users to the publication site is embedded on the card  -----------
  tags$a(href = link,target="_blank",

  shiny::div(
    class = "pubs",
    
    ## image of publication -----------
    shiny::div(
      class = "pubs_image",
      shiny::img(
        src = image,
        style = "width: 80%; "
      )
    ),
    
    ## metadata -----------
    
    shiny::div(
      class = "pubs_content",
      shiny::span(paste0(title),
                  style = "font-size:16px; font-weight:bold; padding: 5px 0;"
      ),
      shiny::span(paste0(country),
                  style = "font-size:14px; font-weight:bold; padding: 5px 0;"
      ),
      shiny::span(paste0(year),
                  style = "font-size:12px; padding: 5px 0;"
      ),
      shiny::span(paste0(authors),
                  style = "font-size:12px; font-style:italic; padding: 5px 0;"
      )
    )
  )
  )
}