#' Kaplan-Meier UI
#'
#' This module contains the widgets needed to create
#' a KM Curve
#'
#' @param id module ID
#' @param label module label
#'
#' @import shiny
#' @import dplyr
#'
#' @family popExp Functions
#' @noRd
#'
km_ui <- function(id, label = "km") {
  ns <- NS(id)
  tagList(
    h4("Select axes:"),
    wellPanel(
      fluidRow(
        column(6, selectInput(ns("yvar"), "Select param", choices = NULL)),
        column(6, selectInput(ns("resp_var"), "Response Variable", choices = "AVAL", selected = "AVAL"))
      ),
      fluidRow(
        column(6, selectInput(ns("group"), "Group By", choices = "NONE", selected = "NONE")),
        column(6, selectInput(ns("cnsr_var"), "Censor Variable (0,1)", choices = "CNSR", selected = "CNSR"))
      ),
      shinyWidgets::radioGroupButtons(ns("timeval"), "Display duration as", c("Day", "Month", "Year"), selected = "Day"),
      numericInput(ns("timeby"), "Time by", value = 30),
      shinyWidgets::materialSwitch(ns("points"), h6("Mark censored observations?"),
        status = "primary", value = TRUE
      ),
      shinyWidgets::materialSwitch(ns("ci"), h6("Include 95% confidence interval?"),
        status = "primary", value = FALSE
      ),
      shinyWidgets::materialSwitch(ns("table"), h6("Show table?"),
        status = "primary", value = FALSE
      ),
      shinyWidgets::materialSwitch(ns("pval"), h6("Show P-value?"),
        status = "primary", value = FALSE
      )
    )
  )
}


#' Kaplan-Meier Curve Server Function
#'
#' Using the widgets from the km UI create
#' a ggplot object which is returned to the
#' parent Population Explorer module
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param data The combined dataframe from population explorer
#' @param run logical, TRUE if select code chunks in this module should execute
#'
#' @import shiny
#' @import dplyr
#'
#' @return ggplot object
#'
#' @family popExp Functions
#' @noRd
#'
km_srv <- function(input, output, session, data, run) {
  ns <- session$ns

  observe({
    req(run(), data())

    # get unique paramcd
    paramcd <- sort(na.omit(unique(data()$PARAMCD)))
    updateSelectInput(
      session,
      inputId = "yvar",
      choices = as.list(paramcd),
      selected = isolate(input$yvar)
    )
  })

  # update response variable options for user based on data filtered to a
  # certain param
  observeEvent(input$yvar, {
    req(run(), input$yvar != "")

    d <- data()
    my_vars <- d %>%
      dplyr::filter(PARAMCD == input$yvar) %>%
      dplyr::filter(data_from == "ADTTE") %>% # Numeric visit var has to exist in TTE data
      select(one_of("AVISITN", "VISITNUM"), ends_with("DY")) %>%
      select_if(~ !all(is.na(.))) %>% # remove NA cols
      colnames()

    updateSelectInput(
      session = session,
      inputId = "resp_var",
      choices = c("AVAL", my_vars),
      selected = isolate(input$resp_var)
    )
  })

  # update censor variable options for user based on data filtered to a
  # certain param
  observeEvent(input$yvar, {
    req(run(), input$yvar != "")

    d0 <- data()
    my_cvars <- d0 %>%
      dplyr::filter(PARAMCD == input$yvar) %>%
      dplyr::filter(data_from == "ADTTE") %>% # Numeric visit var has to exist in TTE data
      select(where(is.numeric)) %>%
      select(where(function(col) all(unique(col) %in% c(0, 1)))) %>%
      select_if(~ !all(is.na(.))) %>% # remove NA cols
      colnames()

    updateSelectInput(
      session = session,
      inputId = "cnsr_var",
      choices = c("CNSR", my_cvars[my_cvars != "CNSR"]),
      selected = isolate(input$cnsr_var)
    )
  })


  observeEvent(input$timeval, {
    req(run(), input$timeval != "")

    if (input$timeval == "Day") {
      updateNumericInput(session, "timeby",
        min = 1,
        value = 30
      )
    } else if (input$timeval == "Month") {
      updateNumericInput(session, "timeby",
        min = 1,
        value = 1
      )
    } else if (input$timeval == "Year") {
      updateNumericInput(session, "timeby",
        min = 1,
        value = 1
      )
    }
  })

  observeEvent(input$yvar, {
    req(run(), input$yvar != "")

    d <- data()
    my_vars <- d %>%
      dplyr::filter(PARAMCD == input$yvar) %>%
      dplyr::filter(data_from == "ADTTE") %>% # Numeric visit var has to exist in TTE data
      select(one_of("AVISITN", "VISITNUM"), ends_with("DY")) %>%
      select_if(~ !all(is.na(.))) %>% # remove NA cols
      colnames()

    updateSelectInput(
      session = session,
      inputId = "resp_var",
      choices = c("AVAL", my_vars),
      selected = isolate(input$resp_var)
    )
  })


  # create plot object using the numeric column on the yaxis
  # or by filtering the data by PARAMCD, then using AVAL or CHG for the yaxis
  p <- reactive({
    req(run(), data(), input$yvar)
    # , input$resp_var,input$group,input$points,input$ci) # can't include these in req
    app_km_curve( # mod_popExp_fct_km.R
      data(),
      input$yvar,
      input$resp_var,
      input$cnsr_var,
      input$group,
      input$points,
      input$ci,
      input$table,
      input$pval,
      input$timeval,
      input$timeby
    )
  })
  return(p)
}
