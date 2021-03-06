
#' A summary of a numeric vector
#'
#' Custom version of \code{summary} function for a numeric vector
#'
#' @param x a numeric vector
#' @param na.rm an optional logical parameter. \code{TURE} by default
#' @param verbose is TRUE for verbose output
#' @return A named vector with six values
#' @export
#' @examples
#' \dontrun{
#' x <- c(4,7,8)
#' numeric_summary(x)
#'}
#must include email address only for maintainer
#cre stands for maintainer
#cph copy right
#fnd funder

#usethis::use_gpl_license(version = 3, include_future = TRUE)
# This function creates a summary for a numeric vector
numeric_summary <- function(x, na.rm=T,verbose=F){
 if(!is.numeric(x)) stop("x must be a numeric vector")
 if (verbose) message("Opening database")
  min = min(x, na.rm=na.rm)
  max = max(x, na.rm=na.rm)
  mean = mean(x, na.rm=na.rm)
  sd = sd(x, na.rm=na.rm)
  length = length(x)
  Nmiss = sum(is.na(x))

  c(min=min, max=max, mean=mean, sd=sd, length=length, Nmiss=Nmiss)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This function creates a summary for a character vector

#' A summary of a character vector
#'
#' Custom version of summary function for a character vector
#'
#' @param x a character vector
#' @param na.rm an optional logical parameter. TRUE by default
#'
#' @return A named vector with six values
#' @export
#'
#' @examples
#' x <- c("A","b","c","E")
#' char_summary(x)
char_summary <- function(x, na.rm=T){

  length = length(x)
  Nmiss = sum(is.na(x))
  Nunique = length(unique(x))

  c(length = length,
    Nmiss = Nmiss,
    Nunique = Nunique )

}


