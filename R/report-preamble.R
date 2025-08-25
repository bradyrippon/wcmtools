
#' Set knitr defaults/fonts for WCM reports
#'
#' Override any knitr option via `...`, or set global defaults:
#' `options(wcmtools.knitr_defaults = list(fig.width = 8))`
#'
#' @param attach_html_font Logical; call [wcm_attach_font()] for HTML font dependencies.
#' @param register_plots Logical; call [wcm_register_fonts()] so plots use WCM Sans.
#' @param ... Optional overrides for `knitr::opts_chunk$set()`.
#' @return List of set knitr options.
#' @export

wcm_setup <- function(
    attach_html_font = FALSE, # wcm_html) handles this
    register_plots   = TRUE,
    ...
  ) {

  # check that knitr is installed
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("wcm_setup() requires the 'knitr' package.")
  }

  # Default device prefers ragg if available; falls back to png
  dev_default <- if (requireNamespace("ragg", quietly = TRUE)) "ragg_png" else "png"

  # Base defaults
  defaults <- list(
    echo = FALSE, message = FALSE, warning = FALSE,
    fig.align = "center", fig.width = 7, fig.height = 5,
    dev = dev_default
  )

  # allow user to set global defaults
  user_defaults <- getOption("wcmtools.knitr_defaults", NULL)
  if (!is.null(user_defaults) && !is.list(user_defaults)) {
    stop("Option 'wcmtools.knitr_defaults' must be a list.")
  }

  # establish per-document overrides
  overrides <- list(...)

  # merge everything: defaults <-- user_defaults <-- overrides
  merged <- defaults
  if (!is.null(user_defaults)) merged <- utils::modifyList(merged, user_defaults, keep.null = TRUE)
  if (length(overrides))       merged <- utils::modifyList(merged, overrides,     keep.null = TRUE)
  do.call(knitr::opts_chunk$set, merged)

  # only attach fonts for HTML output if requested
  if (isTRUE(attach_html_font) && knitr::is_html_output()) {
    wcm_attach_font()  # uses knit_meta_add internally
  }

  # attach fonts for plotting (graceful degradation)
  if (isTRUE(register_plots)) {
    if (!requireNamespace("showtext", quietly = TRUE)) {
      warning("register_plots = TRUE requires the 'showtext' package; skipping plot font registration.")
    } else {
      wcm_register_fonts()
    }
  }

  invisible(merged)

}
