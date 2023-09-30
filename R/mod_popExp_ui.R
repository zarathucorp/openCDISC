#' popExp UI Function
#'
#' @description UI for population explorer: the filtering widget
#' as well as radio buttons for different plots. These radio buttons
#' are used to toggle between the child modules of each plot type, and
#' a conditional panel of widgets based on the plot type
#'
#' @param id Internal parameters for {shiny}.
#' @param label Name of module
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom IDEAFilter shiny_data_filter_ui
#'
#' @family popExp Functions
#' @noRd
#'
mod_popExp_ui <- function(id, label = "Population Explorer") {
  ns <- NS(id)
  tagList(
    div(uiOutput(ns("study_pop_exp")), class = "studyid"),
    fluidRow(
      column(
        width = 3,
        div(
          id = "pop_cic_adv_filtering", 
          class = "filter-accordion",
          checkboxInput(
            inputId = ns("adv_filtering"),
            div(
              class = "filter-container", 
              span("Filter Data ", style = "float:left;"),
              span(icon("chevron-down", verify_fa = FALSE), style = "float:right;")
            ),
            value = FALSE
          )
        ),
        conditionalPanel(
          condition = "input.adv_filtering == true", 
          ns = ns,
          div(
            id = "custom_checkbox",
            div(
              id = "pop_cic_apply_filters", 
              materialSwitch(
                inputId = ns("apply_filters"),
                label = strong(em(h5("Apply Filters"))),
                status = "primary",
                value = FALSE
              )
            ),
            div(
              id = "pop_cic_filter_df", 
              selectInput(
                inputId = ns("filter_df"), 
                label = "Filter on Variable(s) in a loaded ADaM",
                multiple = TRUE, 
                choices = "ADSL", 
                selected = "ADSL"
              )
            ),
            div(
              id = "pop_cic_data_filter", 
              IDEAFilter::shiny_data_filter_ui(inputId = ns("data_filter"))
            )
          )
        ), # Conditional Panel
        div(
          id = "pop_cic_chart_type",
          h4("Type of Chart:"),
          wellPanel(
            br(),
            radioButtons(
              inputId = ns("plot_type"),
              label = NULL,
              choices = c(
                # "Kaplan-Meier Curve", open when data has adtte
                "Line plot - mean over time",
                "Heatmap - endpoint correlations",
                "Box Plot",
                "Scatter Plot",
                "Spaghetti Plot"
              )
            )
          )
        ),
        div(
          id = "pop_cic_chart_inputs",
          conditionalPanel("input.plot_type === 'Box Plot'", ns = ns, boxPlot_ui(ns("boxPlot"))),
          conditionalPanel("input.plot_type === 'Spaghetti Plot'", ns = ns, spaghettiPlot_ui(ns("spaghettiPlot"))),
          conditionalPanel("input.plot_type === 'Scatter Plot'", ns = ns, scatterPlot_ui(ns("scatterPlot"))),
          conditionalPanel("input.plot_type === 'Kaplan-Meier Curve'", ns = ns, km_ui(ns("km"))),
          conditionalPanel("input.plot_type === 'Line plot - mean over time'", ns = ns, linePlot_ui(ns("linePlot"))),
          conditionalPanel("input.plot_type === 'Heatmap - endpoint correlations'", ns = ns, heatmap_ui(ns("heatmap")))
        ),
        br(),
        br(),
        br(),
        br(),
        br()
      ),
      column(
        width = 9,
        div(
          id = "pop_cic_plot",
          wellPanel(
            conditionalPanel( # plot
              "input.plot_type === 'Box Plot' || 
              input.plot_type === 'Kaplan-Meier Curve' ||
              input.plot_type === 'Line plot - mean over time' ||
              input.plot_type === 'Heatmap - endpoint correlations'",
              ns = ns,
              plotOutput(ns("plot_output"), height = 700)
            ),
            conditionalPanel( # plotly
              "input.plot_type === 'Spaghetti Plot' || 
              input.plot_type === 'Scatter Plot'",
              ns = ns, 
              plotlyOutput(ns("plotly_output"), height = 700)
            ),
            uiOutput(outputId = ns("downloadControls")),
            fluidRow(
              column(
                width = 4,
                selectizeInput(
                  inputId = ns("file_ext"), 
                  "File extension (dpi = 300)",
                  choices = c("jpg", "pdf", "tiff", "svg", "pptx"), 
                  multiple = F,
                  selected = "pptx"
                )
              ),
              column(
                width = 4,
                sliderInput(
                  inputId = ns("fig_width"), 
                  label = "Width (in):",
                  min = 5, max = 15, value = 8
                )
              ),
              column(
                width = 4,
                sliderInput(
                  inputId = ns("fig_height"), 
                  label = "Height (in):",
                  min = 5, max = 15, value = 6
                )
              )
            ),
            downloadButton(outputId = ns("downloadButton"), label = "Download the plot"),
            div(style = "color: #0275d8; font-size: 12px;", htmlOutput(ns("applied_filters"))),
            br(), br(),
            DT::dataTableOutput(ns("plot_data"))
          )
        )
      )
    )
  )
}


