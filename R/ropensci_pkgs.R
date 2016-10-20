#' Get rOpenSci packages, actual R packages, via the ropensci registry
#'
#' @export
#' @param only_on_cran (logical). return only pkgs on CRAN. Default: \code{FALSE}
#' @examples \dontrun{
#' ropensci_pkgs()
#' ropensci_pkgs(TRUE)
#' }
ropensci_pkgs <- function(only_on_cran = FALSE) {
  df <- ropkgs::ro_pkgs()
  if (only_on_cran) {
    df$packages <- dplyr::filter(df$packages, on_cran)
  }
  dplyr::data_frame(
    name = df$packages$name,
    owner_repo = unlist(stringr::str_extract_all(df$packages$url, "ropensci/.+|ropenscilabs/.+|gmbecker/.+"))
  )
}
