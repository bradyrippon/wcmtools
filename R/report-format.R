
# ---------- helpers ----------

#' Internal utils for font switching
#' @keywords internal
#' @noRd

.css_stack <- function(key) switch(
  key,
  "wcm-sans"          = '"WCM Sans", system-ui, -apple-system, "Segoe UI", Roboto, Arial, sans-serif',
  "system"            = 'system-ui, -apple-system, "Segoe UI", Roboto, Arial, sans-serif',
  "lato"              = '"Lato", system-ui, -apple-system, "Segoe UI", Roboto, Arial, sans-serif',
  "roboto"            = 'Roboto, system-ui, -apple-system, "Segoe UI", Arial, sans-serif',
  "libre-baskerville" = '"Libre Baskerville", Georgia, "Times New Roman", serif',
  # fallback
  '"WCM Sans", system-ui, -apple-system, "Segoe UI", Roboto, Arial, sans-serif'
)


.dep_google_font <- function(key) {
  href <- switch(key,
                 "lato" = "https://fonts.googleapis.com/css2?family=Lato:wght@400;700&display=swap",
                 "roboto" = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap",
                 "libre-baskerville" = "https://fonts.googleapis.com/css2?family=Libre+Baskerville:wght@400;700&display=swap",
                 NULL
  )
  if (is.null(href)) return(NULL)
  htmltools::htmlDependency(
    name = paste0("gf-", key), version = "1.0.0", src = c(),
    head = sprintf('<link rel="stylesheet" href="%s">', href)
  )
}



# ---------- public api ----------

#' WCM HTML report format
#'
#' A thin wrapper around rmarkdown::html_document with WCM defaults and
#' packaged font/CSS dependencies pre-attached.
#'
#' @inheritParams rmarkdown::html_document
#' @param font Chosen font (`"auto"`, `"wcm-sans"`, `"system"`, `"lato"`, `"roboto"`, `"libre-baskerville"`).
#' @export

wcm_html <- function(
    toc = TRUE,
    toc_depth = 3,
    toc_float = TRUE,
    number_sections = FALSE,
    df_print = "default",
    theme = "cosmo",
    font = c("auto","wcm-sans","system","lato","roboto","libre-baskerville"),
    code_folding = c("none","show","hide"),
    code_download = FALSE,
    self_contained = TRUE,
    fig_caption = TRUE,
    highlight = "default",
    mathjax = "default",
    anchor_sections = TRUE,
    ...
  ) {

  font <- match.arg(font) # validate argument
  if (identical(font, "auto")) {
    font <- getOption("wcmtools.font", "wcm-sans")
  }

  # use packaged CSS for template
  css_default <- system.file(
    "rmarkdown", "templates", "report", "resources", "wcm-report.css",
    package = "wcmtools"
  )

  code_folding <- match.arg(code_folding) # validate argument

  # create fail-safe for self-contained documents
  sc <- self_contained
  if (isTRUE(sc) && !(font %in% c("wcm-sans", "system"))) {
    warning("self_contained=TRUE conflicts with Google Fonts. Switching to FALSE.")
    sc <- FALSE
  }

  fmt <- rmarkdown::html_document(
    toc = toc,
    toc_depth = toc_depth,
    toc_float = toc_float,
    number_sections = number_sections,
    df_print = df_print,
    theme = theme,
    css = css_default,
    code_folding = code_folding,
    code_download = code_download,
    self_contained = sc,
    fig_caption = fig_caption,
    highlight = highlight,
    mathjax = mathjax,
    anchor_sections = anchor_sections,
    ...
  )

  # add info for including WCM logo
  logo_path <- system.file(
    "rmarkdown", "templates", "report", "resources", "wcmlogo.html",
    package = "wcmtools"
  )

  existing <- fmt$includes
  if (is.null(existing)) existing <- rmarkdown::includes()

  fmt$includes <- rmarkdown::includes(
    in_header   = existing$in_header,
    before_body = existing$before_body,
    after_body  = c(existing$after_body, logo_path)
  )

  # attach font dependencies
  deps <- list()
  if (identical(font, "wcm-sans")) {
    deps <- c(deps, list(.dep_wcm_sans()))
  } else if (!identical(font, "system")) {
    gf <- .dep_google_font(font)
    if (!is.null(gf)) deps <- c(deps, list(gf))
  }

  fmt$dependencies <- c(fmt$dependencies, deps)
  return(fmt)

}
