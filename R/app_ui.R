#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # List the first level UI elements here
    navbarPage(
      title = div(
        id = "logo-id", "openCDISC"
      ),
      id = "navbarID",
      windowTitle = "openCDISC",
      tabPanel(
        title = "Data",
        mod_dataUpload_ui("dataUpload_ui_1")
      ),
      tabPanel(
        title = "Table Generator",
        div(mod_tableGen_ui("tableGen_ui_1"), id = "tableGen")
      ),
      tabPanel(
        title = "Population Explorer",
        # mod_selectData_ui("selectData_ui_1"),
        # mod_selectData_ui("popExp_ui_1"),
        mod_popExp_ui("popExp_ui_1")
      ),
      tabPanel(
        title = "Individual Explorer",
        mod_indvExp_ui("indvExp_ui_1")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjs extendShinyjs
#' @importFrom shinyjs inlineCSS
#' @importFrom cicerone use_cicerone
#' @noRd
golem_add_external_resources <- function() {
  # source("R/global.R")

  add_resource_path(
    "www", app_sys("app/www")
  )

  # golem tags$head start
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "openCDISC"
    ),
    tags$script(HTML(htmljs)),
    tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
    shinyjs::useShinyjs(),
    # waiter::use_waiter(),
    shinyjs::inlineCSS(css),
    shinyjs::extendShinyjs(
      text = jscode,
      functions = c("disableTab", "enableTab")
    ),
    cicerone::use_cicerone()
  )
}
