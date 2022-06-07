
#must include email address only for maintainer
#cre stands for maintainer
#cph copy right
#fnd funder

#usethis::use_gpl_license(version = 3, include_future = TRUE)
# This function creates a summary for a numeric vector
numeric_summary <- function(x, na.rm=T){

  min = min(x, na.rm=na.rm)
  max = max(x, na.rm=na.rm)
  mean = mean(x, na.rm=na.rm)
  sd = sd(x, na.rm=na.rm)
  length = length(x)
  Nmiss = sum(is.na(x))

  c(min=min, max=max, mean=mean, sd=sd, length=length, Nmiss=Nmiss)

}

# This function creates a summary for a character vector
char_summary <- function(x, na.rm=T){

  length = length(x)
  Nmiss = sum(is.na(x))
  Nunique = length(unique(x))

  c(length = length,
    Nmiss = Nmiss,
    Nunique = Nunique )

}


