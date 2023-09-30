#' Box Plot UI
#'
#' This module contains the widgets needed to create
#' a box plot
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
boxPlot_ui <- function(id, label = "box") {
  ns <- NS(id)
  tagList(
    h4("Select axes:"),
    wellPanel(
      selectInput(ns("yvar"), "Select y-axis", choices = NULL),
      fluidRow(column(12, align = "center", uiOutput(ns("include_var")))),
      selectInput(inputId = ns("group"), label = "Group By", choices = NULL),
      checkboxInput(ns("isfacet"), "Divide charts?", value = FALSE), # Not use ns
      shinyjs::hidden(
        div(
          id = ns('facet-options'),
          column(
            width = 6,
            selectInput(
              inputId = ns('facet'), 
              label = 'Facet By', 
              choices = NULL
            )
          ),
          column(
            width = 6,
            sliderInput(
              inputId = ns('nrow'), 
              label = 'Number of Rows', 
              min = 1, max = 6,
              value = 2, step = 1
            )
          )
        )
      ),
      checkboxInput(ns("points"), "Add Points?")
    )
  )
}

#' Box Plot Server Function
#'
#' Using the widgets from the scatter plot UI
#' create a ggplot object which is returned to the 
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
boxPlot_srv <- function(input, output, session, data, run) {
  ns <- session$ns
  
  
  observeEvent(input$isfacet, {
    if(input$isfacet){
      shinyjs::show(id = 'facet-options')
    }
    else{
      shinyjs::hide(id = 'facet-options')
      updateSelectInput(session = session, inputId = 'facet', choices = character(0))
    }
    
  })
  
  # -------------------------------------------------
  # Update Inputs
  # -------------------------------------------------
  
  observe({
    req(run(), data())
    
    # numeric columns, remove aval, chg, base
    num_col <- subset_colclasses(data(), is.numeric)
    num_col <- sort(num_col[num_col != "AVAL" & num_col != "CHG" & num_col != "BASE"])
    
    # get unique paramcd
    paramcd <- sort(na.omit(unique(data()$PARAMCD)))
    
    updateSelectInput(session, "yvar",
                      choices = list(`Time Dependent` = paramcd, `Time Independent` = num_col),
                      selected = isolate(input$yvar))
    
    # Update grouping variable based on yvar selection
    if(input$yvar != "" && !(input$yvar %in% colnames(data()))){
      group_dat <- data() %>% dplyr::filter(PARAMCD == input$yvar)
    } else {
      group_dat <- data()
    }

    group_dat <- group_dat %>% select_if(~!all(is.na(.))) # remove NA cols
    group_fc <- subset_colclasses(group_dat, is.factor)
    group_ch <- subset_colclasses(group_dat, is.character)
    group <- c(group_fc, group_ch)
    group <- sort(group[group != "data_from"])
    
    updateSelectInput(session, "group", choices = group, selected = isolate(input$group))
    
    # update facet with same value as group
    updateSelectInput(session, "facet", choices = group, selected = isolate(input$group))
  })
  
  output$include_var <- renderUI({
    req(run(), input$yvar %in% data()$PARAMCD)
    shinyWidgets::radioGroupButtons(
      inputId = ns("value"), 
      label = "Value",
      choices = c("AVAL", "CHG", "BASE"),
      selected = isolate(input$value)
    )
  })
  
  # -------------------------------------------------
  # Create boxplot using inputs
  # -------------------------------------------------
  
  # create plot object using the numeric column on the yaxis
  # or by filtering the data by PARAMCD, then using AVAL or CHG for the yaxis
  
  p <- reactive({
    req(run(), data(), input$yvar, input$group)
    if(input$isfacet){ 
      app_boxplot(data(), input$yvar, input$group, input$value, input$points, facet = input$facet, nrow = input$nrow)
    }
    else{
      req(input$facet)
      app_boxplot(data(), input$yvar, input$group, input$value, input$points)
    }
  })
  
  
  
  # return the plot object to parent module
  return(p)
}
