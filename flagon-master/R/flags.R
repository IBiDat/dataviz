

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' data.frame of all available countries and their ISO-3166 country codes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"country_codes"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Retrieve the link to image files for flag
#'
#' @param ccodes Country codes in ISO-3166 format e.g. 'au' for Australia
#' @param filetype one of 'png' or 'svg'.  Default: 'png'
#'
#' @return character string of filenames.  If an invalid country code is given,
#' or there was a problem finding the file, an \code{NA_character_} will be
#' returned at this location.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
flags <- function(ccodes, filetype = c('png', 'svg')) {

  ccodes   <- tolower(ccodes)
  filetype <- match.arg(filetype)
  pkg_dir  <- find.package('flagon')

  ifelse(
    ccodes %in% flagon::country_codes$ccode,
    paste0(pkg_dir, "/", filetype, "/", ccodes, ".", filetype),
    NA_character_
  )
}
