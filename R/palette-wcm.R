
# ---------- helpers ----------

#' WCM branding palettes
#'
#' Returns a list of WCM branding palettes.
#' @keywords internal
#' @noRd

.branding_wcm <- function() {
  list(
    primary   = c("#B31B1B", "#CF4520", "#E87722", "#FFC72C"),
    secondary = c("#000000", "#555555", "#777777", "#DDDDDD", "#F7F7F7", "#FFFFFF")
  )
}



# ---------- public api ----------

#' WCM color palette
#'
#' Generate WCM color palettes for plotting.
#'
#' @param palette One of `palette_wcm_names()` (`"primary"`, `"secondary"`).
#' @param n Number of colors. Returns full palette by default.
#' @param type Either `"discrete"` (subset) or `"continuous"` (interpolated).
#' @param reverse Logical; if `TRUE`, reverse the palette order.
#'
#' @return A character vector of hex colors with class `"wcm_palette"`.
#' @family palettes
#' @export
#' @examples
#'
#' palette_wcm()
#' palette_wcm(palette = "primary", n = 5)
#' palette_wcm(palette = "secondary", n = 5)

palette_wcm <- function(
    palette = "primary",
    n = NULL,
    type = c("discrete", "continuous"),
    reverse = FALSE
  ) {

  ## initial errors from user inputs ----------

  type <- match.arg(type) # validates argument

  # validate palette
  brand <- .branding_wcm()
  palette <- match.arg(palette, choices = names(brand))
  pal <- brand[[palette]]

  # validate n
  if (is.null(n)) n <- length(pal)
  if (!is.numeric(n) || length(n) != 1L || n <= 0 || n %% 1 != 0)
    stop("`n` must be a single positive integer.", call. = FALSE)
  n <- as.integer(n)

  # reverse palette if necessary
  if (reverse) pal <- rev(pal)

  # validate type
  cols <- if (type == "discrete") {
    if (n > length(pal)) {
      stop("Requested ", n, " colors but palette '", palette, "' has ", length(pal), ".", call. = FALSE)
    }
    pal[seq_len(n)]
  } else {
    # interpolate colors based on continuous size
    grDevices::colorRampPalette(pal)(n)
  }

  # change structure for color swatch printing
  structure(cols, class = c("wcmtools_palette", "character"), name = paste0("wcm:", palette))
}



#' Print a WCM palette
#'
#' Prints a color swatch for objects returned by [palette_wcm()].
#'
#' @param x A `"wcmtools_palette"` object.
#' @param ... Unused.
#' @return Invisibly returns `x`.
#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
#'

print.wcmtools_palette <- function(
    x, ...,
    family = getOption("wcmtools.font_family", "WCM Sans")
  ) {

  n <- length(x)
  old <- graphics::par(mar = c(0.5, 0.5, 0.5, 0.5), family = family) # set margins
  on.exit(graphics::par(old), add = TRUE)

  # draw one row of palette colors
  graphics::image(
    1:n, 1, matrix(1:n, nrow = 1), col = x,
    ylab = "", xaxt = "n", yaxt = "n", bty = "n"
  )

  # draw transparent rectangle
  graphics::rect(
    xleft = 0, ybottom = 0.9, xright = n +1, ytop = 1.1,
    col = grDevices::rgb(1, 1, 1, 0.8), border = NA
  )

  # print name of palette
  graphics::text(
    x = (n +1)/2, y = 1, labels = attr(x, "name"), cex = 1,
    family = family, font = 2
  )

  invisible(x)

}
