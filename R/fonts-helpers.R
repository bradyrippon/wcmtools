
# ---------- helpers ----------

# Create HTML dependency for packaged font "WCM Sans"

.wcm_dep_wcm_sans <- function() {
  htmltools::htmlDependency(
    name    = "wcm-sans",
    version = "1.0.0",
    src     = system.file("fonts", "wcm-sans", "otf", package = "wcmtools"),
    stylesheet = "wcm-sans.css"
  )
}
