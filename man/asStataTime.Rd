\name{asStataTime}
\alias{asStataTime}
\title{Convert R date-time classes to Stata date-times}
\description{
  Converts R date and time classes (chron, Date, POSIXt, yearmon,
  yearqtr) to a numeric value corresponding
  to the most similar Stata date/time format.
}
\usage{
asStataTime(x, useTc = TRUE)
}

\arguments{
  \item{x}{
    chron, Date, POSIXt, yearmon, or yearqtr object.
  }
  \item{useTc}{
    If TRUE, use the '\%tc' format for converting POSIXt objects.
    Otherwise, use the '\%td' format for POSIXt object.
    The '\%tc' format was introduced in Stata 10, so set this to
    FALSE to ensure compatibility with earlier versions of Stata.
  }
}
\details{

  Stata stores date-time variables as a number of time units since 1960-1-1.
  The number of units depends on the format: milliseconds for '\%tc',
  days for '\%td', months for '\%tm', quarters for '\%tq', half-years
  for '\%th'.  \bold{asStataTime} converts to following R data-time
  classes to their most similar Stata date-time format

  \tabular{ll}{
    chron \tab \%td \cr
    Date \tab \%td \cr
    POSIXct \tab \%tc or \%td \cr
    POSIXlt \tab \%tc or \%td \cr
    yearmon \tab \%tm \cr
    yearqtr \tab \%tq 
  }

}
\value{
  numeric vector. The vector has a single attribute \bold{stata.format}:
  a character string with the Stat format.
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
  \code{\link{fromStataTime}}, \code{\link{Date}}, \code{\link{POSIXt}},
  \code{\link{yearmon}}, \code{\link{yearqtr}}
}

\examples{

    ## Date
    asStataTime(as.Date('1960-1-1') + -1:1)

    ## POSIXct
    asStataTime(as.POSIXct('1960-1-1 00:00:00') + -1:1)

}

\keyword{chron}
\keyword{manip}
