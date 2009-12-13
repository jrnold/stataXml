\name{fromStataTime}
\alias{fromStataTime}
\title{Convert Stata date-times to R classes}
\description{
  Converts numeric values corresponding to a Stata date-time format
  to a Date or POSIXct object.
}
\usage{
fromStataTime(x, fmt, tz = "")
}

\arguments{
  \item{x}{
    numeric.
  }
  \item{fmt}{
    character.  Stata date-time format: '\%tc', '\%tC', '\%td', '\%tm',
    '\%tq', '\%th', '\%tg', or '\%ty'.
  }
  \item{tz}{
    character. Time zone. 
  }
}
\details{
  Stata stores dates-time variables as units of time since Jan 1, 1960.
  The unit of time used depends on the Stata format.
  One unit in variable formatted '\%tc', corresponds to 1 millisecond; in '\%td', one day; in '\%tm', one
  month, etc.

  This function is useful for converting date-time variables imported from a Stata
  dataset, such as from read.dta or read.stataXml, into the correct R
  dates values.

  Stata does not use time zones in date-time values. If a time zone is
  important for conversion, supply a time zone in the \bold{tz}
  argument.

  The Stata '\%tC' format includes leap seconds. Since R does not use
  leap seconds in the \bold{POSIXt} classes, values of leap seconds are
  handled according to the POSIX standard when converting '\%tC' values
  to \bold{POSIXct}.
}

\value{
  If \bold{fmt} is '\%tc' or '\%tC', then a \bold{POSIXct} object is returned.
  If \bold{fmt} is '\%td', '\%tm', '\%tq', '\%th', then a \bold{Date} object
  is returned. If \bold{fmt} is '\%tg' or '\%ty', then a numeric object object
  is returned.
}

\references{

  For details of how Stata represents dates and times, see Stata help
  for \bold{dates_and_times}, online at
  \url{http://www.stata.com/help.cgi?dates_and_times}.
  
}

\author{
Jeffrey Arnold
}

\seealso{
  \code{\link{asStataTime}}, \code{\link{Date}}, \code{\link{POSIXt}}
}
\examples{
## clock to POSIXt
fromStataTime(-1:1, 'tc')
## dates to Date
fromStataTime(-1:1, 'td')
## months 
fromStataTime(-1:1, 'tm')
## quarters
fromStataTime(-1:1, 'tq')
## half-years
fromStataTime(-1:1, 'th')
## years
fromStataTime(1959:1960, 'ty')
}
\keyword{chron}
\keyword{manip}
