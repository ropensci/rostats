#' Gather CRAN details
#'
#' @export
#' @param pkg (character) a package name
#' @param x a data.frame
#' @details While \code{\link{gather_commits}} and \code{\link{gather_downloads}} cache
#' data because data can take a long time to download, this function shouldn't take that
#' long, even with hundreds of of pkgs.
#' @examples \dontrun{
#' library("rostats")
#' library("ropkgs")
#' library("dplyr")
#' url <- "https://raw.githubusercontent.com/ropensci/roregistry/gh-pages/registry.json"
#' df <- jsonlite::fromJSON(url)
#' (pkgs <- tbl_df(df$packages) %>%
#'   filter(on_cran | on_bioc) %>%
#'   .$name)
#' sort(pkgs)
#'
#' # get first date on CRAN for each pkg
#' res <- gather_crans(pkgs)
#' alldat <- dplyr::bind_rows(res)
#'
#' # get new packages on CRAN, arranged by date
#' alldat %>% cran_first_date() %>% arrange(desc(date)) %>% top_n(10) %>%
#'   readr::write_csv("../biweekly/data/newpkgs.csv")
#'
#' # get new packages & new versions on CRAN, arranged by date
#' alldat %>% arrange(desc(date)) %>% filter(date > '2019-04-14') %>% top_n(30) %>%
#'   readr::write_csv("../biweekly/data/newversions.csv")
#' }
gather_cran <- function(pkg) {
  urinew <- sprintf('https://cloud.r-project.org/web/packages/%s/index.html', pkg)
  res <- httr::GET(urinew)
  html2 <- httr::content(res, as = "text", encoding = "UTF-8")
  pubdate <- as.Date(stringr::str_extract(html2, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))
  if (is.na(pubdate)) {
    dfnew <- dplyr::data_frame(pkg = pkg, name = NA, date = pubdate)
  } else {
    html3 <- strsplit(html2, "\n")[[1]]
    pos <- grep("version:", html3, ignore.case = TRUE)
    if (length(pos) == 0) {
      name <- if (any(grepl("archived", html3, ignore.case = TRUE))) "archived" else NA_character_
      dfnew <- dplyr::data_frame(pkg = pkg, name = name, date = pubdate)
    } else {
      name <- xml2::xml_text(xml2::read_xml(html3[pos + 1]))
      dfnew <- dplyr::data_frame(pkg = pkg, name = name, date = pubdate)
    }
  }

  df <- dplyr::data_frame()
  uri <- sprintf('https://cran.r-project.org/src/contrib/Archive/%s/', pkg)
  out <- plyr::try_default(xml2::read_html(uri), default = "nope", quiet = TRUE)
  if (inherits(out, "xml_document")) {
    out <- xml2::xml_find_all(xml2::read_html(uri), '//a[contains(text(), "tar.gz")]')
    out <- gsub("\\s", "", xml2::xml_text(out))
    names <- unlist(lapply(out, function(z) sub(".tar.gz", "", strsplit(z, "_")[[1]][[2]])))
    df <- dplyr::data_frame(pkg = pkg, name = names, date = NA)

    out <- plyr::try_default(rvest::html_table(xml2::read_html(uri))[[1]], default = "nope")
    # out <- xml2::read_html(uri)
    if (inherits(out, "character")) {
      df <- tibble::data_frame(pkg = pkg, name = NA, date = NA)
    } else {
      df <- stats::na.omit(out[,!names(out) %in% c('','Description')])[-1,]
      df <- df[, c('Name', 'Last modified')]
      names(df) <- c('name', 'date')
      # remove parent directory
      remov <- grep("parent", df$name, ignore.case = TRUE)
      if (length(remov) > 0) df <- df[-remov, ]
      # remove any rows with no version
      df <- df[!df$name == "", ]
      df$pkg <- pkg
      # df$date <- as.Date(df$date, "%d-%b-%Y")
      df$date <- as.Date(unname(sapply(df$date, function(z) strsplit(z, "\\s")[[1]][1])))
      df$name <- gsub(paste0(pkg, "|.tar.gz|_"), "", df$name)
    }
  }

  dplyr::arrange_(rbind(dfnew, df), "desc(name)")
}

#' @export
#' @rdname gather_cran
gather_crans <- function(pkg) {
  out <- vector("list", length = length(pkg))
  for (i in seq_along(pkg)) {
    # out[[i]] <- gather_cran(pkg[i])
    out[[i]] <- plyr::try_default(gather_cran(pkg[i]), data.frame(NULL))
  }
  return(out)
}

#' @export
#' @rdname gather_cran
cran_first_date <- function(x) {
  pkgs <- split(x, x$pkg)
  out <- vector("list", length = length(pkgs))
  for (i in seq_along(pkgs)) {
    out[[i]] <- stats::setNames(pkgs[[i]][pkgs[[i]]$date == min(pkgs[[i]]$date), ],
                         c('pkg', 'version', 'date'))
  }
  dplyr::bind_rows(out)
}
