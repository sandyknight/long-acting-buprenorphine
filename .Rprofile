options(
   repos = c(CRAN = "https://cran.ma.imperial.ac.uk/"),
   lintr.linters = lintr::linters_with_defaults(object_usage_linter = NULL),
   viewer = function(url) {
       if (grepl("html$", url)) {  # Only for HTML files
           if (!require(servr, quietly = TRUE)) {
               message("Installing servr package for HTML viewing...")
               install.packages("servr")
           }
           servr::httw(dirname(url))
       } else {
           # Fall back to default browser for non-HTML
           utils::browseURL(url)
       }
   }
)

if (interactive()) {
  cat("🐺 ")
}
