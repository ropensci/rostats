#' Gather CRAN details
#'
#' @export
#' @param pkg (character) a package name
#' @details While \code{\link{gather_commits}} and \code{\link{gather_downloads}} cache
#' data because data can take a long time to download, this function shouldn't take that
#' long, even with hundreds of of pkgs.
#' @examples \dontrun{
#' gather_cran("taxize")
#'
#' library("ropkgs")
#' library("dplyr")
#' df <- ro_pkgs()
#' pkgs <- df$packages %>%
#'   filter(on_cran) %>%
#'   .$name
#'
#' gather_cran(pkgs[1])
#'
#' # get first date on CRAN for each pkg
#' res <- gather_crans(pkgs)
#' alldat <- dplyr::bind_rows(res)
#' cran_first_date(alldat)
#'
#' # gather_crans(pkgs$name)
#'
#' # get new packages on CRAN, arranged by date
#' alldat %>% cran_first_date() %>% arrange(desc(date)) %>% top_n(20)
#'
#' # get new packages & new versions on CRAN, arranged by date
#' alldat %>% arrange(desc(date)) %>% top_n(20)
#' }
gather_cran <- function(pkg) {
  urinew <- sprintf('https://cran.rstudio.com/web/packages/%s/index.html', pkg)
  res <- httr::GET(urinew)
  html2 <- httr::content(res, as = "text", encoding = "UTF-8")
  pubdate <- as.Date(stringr::str_extract(html2, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))
  if (is.na(pubdate)) {
    dfnew <- dplyr::data_frame(pkg = pkg, name = NA, date = pubdate)
  } else {
    dfnew <- dplyr::data_frame(pkg = pkg, name = 'current_version', date = pubdate)
  }

  df <- dplyr::data_frame()
  uri <- sprintf('https://cran.rstudio.com/src/contrib/Archive/%s/', pkg)
  out <- plyr::try_default(xml2::read_html(uri), default = "nope", quiet = TRUE)
  if (inherits(out, "xml_document")) {
    out <- plyr::try_default(rvest::html_table(xml2::read_html(uri))[[1]], default = "nope")
    if (inherits(out, "character")) {
      df <- tibble::data_frame(pkg = pkg, name = NA, date = NA)
    } else {
      df <- na.omit(out[,!names(out) %in% c('','Description')])[-1,]
      df <- df[, c('Name', 'Last modified')]
      names(df) <- c('name', 'date')
      df$pkg <- pkg
      df$date <- as.Date(df$date, "%d-%b-%Y")
    }
  }

  rbind(dfnew, df)
}

#' @export
#' @rdname gather_cran
gather_crans <- function(pkg) {
  out <- vector("list", length = length(pkg))
  for (i in seq_along(pkg)) {
    out[[i]] <- gather_cran(pkg[i])
  }
  return(out)
}

#' @export
#' @rdname gather_cran
cran_first_date <- function(x) {
  pkgs <- split(x, x$pkg)
  out <- vector("list", length = length(pkgs))
  for (i in seq_along(pkgs)) {
    out[[i]] <- setNames(pkgs[[i]][pkgs[[i]]$date == min(pkgs[[i]]$date), ],
                         c('pkg', 'version', 'date'))
  }
  dplyr::bind_rows(out)
}
