
# ---------- helpers ----------

#' Create HTML dependency for packaged font "WCM Sans"
#' @keywords internal
#' @noRd

.dep_wcm_sans <- function() {
  htmltools::htmlDependency(
    name    = "wcm-sans",
    version = "1.0.0",
    src     = system.file("fonts", "wcm-sans", "otf", package = "wcmtools"),
    stylesheet = "wcm-sans.css"
  )
}


#' Create a style tag that sets a CSS variable for the template
#' @keywords internal
#' @noRd

.dep_font_var <- function(family_stack) {
  htmltools::htmlDependency(
    name    = "wcm-font-var",
    version = "1.0.0",
    src     = c(),  # no files
    head    = sprintf("<style>:root { --font-sans: %s; }</style>", family_stack)
  )
}



# ---------- public api ----------

#' Disable showtext for plotting (revert to default device fonts)
#' @export

wcm_disable_showtext <- function() {

  if (!requireNamespace("showtext", quietly = TRUE)) {
    stop("wcm_with_showtext() requires the 'showtext' package.")
  }

  showtext::showtext_auto(FALSE)
  invisible(TRUE)

  }


#' Wrapper to temporarily enable showtext (for `expr` only)
#' @export

wcm_with_showtext <- function(expr) {

  if (!requireNamespace("showtext", quietly = TRUE)) {
    stop("wcm_with_showtext() requires the 'showtext' package.")
  }

  showtext::showtext_auto(TRUE)
  on.exit(showtext::showtext_auto(FALSE), add = TRUE)
  eval.parent(substitute(expr))

}


#' Register WCM brand fonts for plotting
#'
#' Registers "WCM Sans" from the package and enables showtext for the session.
#'
#' @param family The family name to register (default "WCM Sans").
#' @param auto Logical; enable showtext automatically.
#' @export

wcm_register_fonts <- function(family = "WCM Sans", auto = TRUE) {

  # find .otf path within package
  base <- system.file("fonts", "wcm-sans", "otf", package = "wcmtools")

  regular    <- file.path(base, "1898Sans-Regular.otf")
  bold       <- file.path(base, "1898Sans-Bold.otf")
  italic     <- file.path(base, "1898Sans-Italic.otf")
  bolditalic <- file.path(base, "1898Sans-BoldItalic.otf")

  showtext::font_add(
    family     = family,
    regular    = regular,
    bold       = bold,
    italic     = italic,
    bolditalic = bolditalic
  )

  # enable showtext automatically
  if (isTRUE(auto)) showtext::showtext_auto()
  invisible(family)

}


#' Attach WCM Sans for HTML body text (and set CSS variable)
#'
#' Adds the packaged font as an HTML dependency and sets the CSS variable
#' `--font-sans` so the template CSS (`font-family: var(--font-sans)`) uses it.
#' Returns a tag so knitr can include it in the report
#' @export

wcm_attach_font <- function() {

  # check if knitr is installed
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("wcm_attach_font() requires the 'knitr' package.")
  }

  knitr::knit_meta_add(list(.dep_wcm_sans())) # make sure dependency is registered in head
  family_stack <- '"WCM Sans", system-ui, -apple-system, "Segoe UI", Roboto, Arial, sans-serif'
  knitr::knit_meta_add(list(.dep_font_var(family_stack)))

  invisible(TRUE)

}
