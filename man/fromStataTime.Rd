\name{fromStataTime}
\alias{fromStataTime}
\title{Convert Stata numeric date/time values to R classes}
\description{
  Converts numeric values corresponding to a Stata date/time format
  ("%t*") into a Date or POSIXct class.
}
\usage{
fromStataTime(x, fmt, tz = "")
}

\arguments{
  \item{x}{
    numeric.
  }
  \item{fmt}{
    character.  Stata format, one of '\%tc', '\%tC', '\%td', '\%tm',
    '\%tq', '\%th', '\%tg'.
  }
  \item{tz}{
    time zone.
  }
}
\details{
  Stata interally stores dates and times, as values since Jan 1, 1960.
  The time value of each unit depends on the format. One unit in '\%tc',
  corresponds to 1 millisecond; in '\%td', one day; in '\%tm', one
  month, etc.

  This is primarily useful when taking data imported from a stata
  dataset, such as from read.dta, and converting the numeric dates into
  the correct R dates.
  
}
\value{
  If \bold{fmt} is '\%tc' or '\%tC', then a POSIXct object is returned.
  If \bold{fmt} is '\%td', '\%tm', '\%tq', '\%th', then a Date object
  is returned. If \bold{fmt} is '\%tg', then a numeric object object
  is returned.
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
}
\keyword{chron}
\keyword{manip}
