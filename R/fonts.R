
#' Register WCM brand fonts
#'
#' Registers "WCM Sans" (regular/bold/italic/bolditalic) from the package
#' and enables showtext for the session.
#'
#' @param family The family name to register (default "WCM Sans").
#' @param auto Logical; enable showtext automatically.
#' @export

wcm_register_fonts <- function(family = "WCM Sans", auto = TRUE) {

  # find .otf path within package
  base <- system.file("fonts", "wcm", "otf", package = "wcmtools")

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
