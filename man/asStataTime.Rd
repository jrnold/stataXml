\name{asStataTime}
\alias{asStataTime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
asStataTime(x, useTc = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{useTc}{
%%     ~~Describe \code{useTc} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, useTc=TRUE) {
    ## TODO: handle packages tis, ts, and timeDate

    if ( any(c(is(x, "Date"), is(x, 'dates'), is(x, 'times') ))) {
        ## Date class in base
        ## dates, times classes from chron
        y <- as.numeric(x) - as.numeric(STATA.ORIGIN)
        attr(y, 'stata.format') <- '\%td'
    } else if (any(c(is(x, "POSIXlt"), is(x, "POSIXct"), is(x, 'POSIXt')))) {
        if (useTc) {
            ## POSIXct is number of seconds since 1970
            ## \%tc is number of miliseconds since 1960
            ## R ignores leap seconds as per POSIX, so I will convert to \%tc and not \%tC
            ## as.double unlike as as.POSIXct keeps the fractional seconds
            y <- unclass(as.double(x))
            ## I round to fix floating point errors
            y <- round((y - as.numeric(STATA.ORIGIN) * 86400) * 1000)
            attr(y, 'stata.format') <- '\%tc'
        } else {
            ## If an older version of stata that doesn't support \%tc (version < 10)
            ## Then convert to \%td (dates)
            y <- as.numeric(as.Date(x)) - as.numeric(STATA.ORIGIN)
            attr(y, 'stata.format') <- '\%td'
        }
    } else if (is(x, 'yearqtr')) {
        ## yearqtr from zoo
        ## time is represented year + fractional quarters
        yStata <- as.POSIXlt(STATA.ORIGIN)$year + 1900
        y <- (unclass(x) - yStata) * 4
        attr(y, 'stata.format') <- '\%tq'
    } else if (is(x, 'yearmon')) {
        ## yearqtr from zoo
        ## time is represented year + fractional quarters
        yStata <- as.POSIXlt(STATA.ORIGIN)$year + 1900
        y <- (unclass(x) - yStata) * 12
        attr(y, 'stata.format') <- '\%tm'
    } else {
        ## I don't know what to do with other stuff.
        ## option 1. Treat as dates.
        ## option 2, use \%tg
        y <- as.numeric(x)
        attr(y, 'stata.format') <- '\%tg'
    }
    ##attr(y, 'class') <- 'stataTime'
    y
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
