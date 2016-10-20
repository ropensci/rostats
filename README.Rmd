rOpenSci Stats
==============

Package API

* `ropensci_pkgs()` - get ropensci pkgs from `ropensci/roregistry`
* `cran_first_date()` - get first dates pkgs on CRAN
* `gather_commits()` - gather github commits for ropensci projects
* `gather_cran()` - gather cran version dates - single pkg
* `gather_crans()` - gather cran version dates - many pkgs
* `gather_downloads()` - gather cran.rstudio.com (cranlogs) downloads
* `cum_commits()` - visualize cummulative commits through time
* `cum_contribs()` - visualize cummulative number of contributors through time
* `cum_downloads()` - visualize cummulative CRAN downloads by package

Recently updated data cached:

* Github commits data: `inst/extdata/github_commits_{date}.csv`
* CRAN logs data: `inst/extdata/cran_downloads_{date}.csv`

Scripts:

* Scripts to work with this package in `inst/scripts/`
    * e.g., [a .Rmd](https://github.com/ropensci/rostats/blob/master/inst/scripts/summary.Rmd) & [the output](https://github.com/ropensci/rostats/blob/master/inst/scripts/summary.md)
