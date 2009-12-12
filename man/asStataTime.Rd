\name{asStataTime}
\alias{asStataTime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  \title{Convert R date and time classes to  Stata numeric date/time values}
}
\description{
  Converts R date and time classes (chron, Date, POSIXt, yearmon,
  yearqtr) to a numeric value corresponding
  to the most similar Stata date/time format.
}
\usage{
asStataTime(x, useTc = TRUE)
}
%- maybe also 'usage' for other objects documented here.
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
  \tabular{}{
    chron \tab \%td
    Date \tab \%td
    POSIXct \tab \%tc or \%td
    POSIXlt \tab \%tc or \%td
    yearmon \tab \%tm
    yearqtr \tab \%tq
  }
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jeffrey Arnold
}

\seealso{
  \code{\link{fromStataTime}}, \code{\link{Date}}, \code{\link{POSIXt}},
  \code{\link{yearmon}}, \code{\link{yearqtr}}
}

\examples{
}

\keyword{chron}
\keyword{manip}
