# This file's presence prevents shiny::runApp() from auto-sourcing the R/
# directory when Connect launches inst/app/cliarappak_app.R directly (rather
# than through install/library()). Without it, Connect emits:
#   "Loading R/ subdirectory for Shiny application, but this directory
#   appears to contain an R package."
# and double-sources every file in R/ alongside pkgload::load_all(). See
# ?shiny::loadSupport.
