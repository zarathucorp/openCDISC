#' Kaplan-Meier Curve
#'
#' Create scatter plot where if the variables are numeric then they
#' are plotted, and if they are PARAMCD's then a week and value
#' must be selected for plotting.
#'
#' @param data Merged data to be used in plot
#' @param yvar Selected xy-axis
#' @param resp_var character, the response variable (paramcd)
#' @param cnsr_var character, the censor variable. Usually CNSR
#' @param group character, variable name of grouping variable (categorical or factor)
#' @param points logical, whether to plot + symbols when patients censored
#' @param ci logical, whether the curve(s) should be accompanied with a 95\% CI
#' @param table logical, whether the table should be shown
#' @param pval logical, whether the p-value should be shown
#' @param timeval character, Choice option to display duration(time) as Day , Month , Year
#' @param timeby Numeric, Set interval of duration(time)

#' @importFrom stats as.formula
#' @importFrom GGally ggsurv
#' @importFrom survival survfit Surv
#' @import jskm
#' @family popExp functions
#' @keywords popEx
#'
#' @return A ggplot object containing the KM curve plot
#'
#' @noRd
app_km_curve <- function(data, yvar, resp_var, cnsr_var, group = "NONE", points = TRUE, ci = FALSE, table = FALSE, pval = FALSE, timeval, timeby) {
  resp_var_sym <- rlang::sym(resp_var)

  # Filter data by param selected
  suppressWarnings(
    d <- data %>%
      dplyr::filter(PARAMCD == yvar) %>%
      dplyr::select(USUBJID, PARAM, PARAMCD, one_of(resp_var), one_of(cnsr_var), one_of(group)) %>%
      filter(!is.na(!!resp_var_sym)) %>%
      dplyr::mutate(
        cnsr_var = case_when(
          cnsr_var == 0 ~ 1,
          cnsr_var == 1 ~ 0
        )
      ) %>%
      dplyr::distinct()
  )


  if (timeval == "Month") {
    d[[resp_var]] <- d[[resp_var]] / 30.4
  } else if (timeval == "Year") {
    d[[resp_var]] <- d[[resp_var]] / 365
  }

  fit_formula <- as.formula(paste0(
    "survival::Surv(", resp_var, ", ", cnsr_var, ") ~ ",
    if (group != "NONE" & !rlang::is_empty(group)) group else 1
  ))

  fit <- survival::survfit(fit_formula, data = d)
  fit$call$formula <- fit_formula


  # Initialize title of variables plotted
  # if group used, include those "by" variables in title
  by_title <- dplyr::case_when(
    group != "NONE" ~ paste("\nby", best_lab(data, group)),
    TRUE ~ ""
  )

  # generate plot
  p <-
    jskm::jskm(fit,
      marks = points, ci = ci,
      table = table, surv.scale = "percent", data = d,
      cumhaz = T, pval = pval, pval.coord = c(15, 1.15),
      pval.size = 5, timeby = timeby, ylims = c(0, 1.2),
      main = paste(unique(d$PARAM), by_title),
      legendposition = c(0.15, 0.8), xlab = paste(unique(d$PARAM), paste0("(", timeval, ")"))
    )

  p <- p + ggplot2::theme(
    text = ggplot2::element_text(size = 12),
    axis.text = ggplot2::element_text(size = 12),
    plot.title = ggplot2::element_text(size = 16)
  )

  # Add in plot layers conditional upon user selection
  if (by_title != "") {
    p <- p + ggplot2::theme(plot.margin = ggplot2::margin(t = 1.2, unit = "cm"))
  }

  return(p)
}
