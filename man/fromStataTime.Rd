\name{fromStataTime}
\alias{fromStataTime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fromStataTime(x, fmt, tz = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{fmt}{
%%     ~~Describe \code{fmt} here~~
}
  \item{tz}{
%%     ~~Describe \code{tz} here~~
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
function(x, fmt, tz='') {

    ## convert.times <- list(tc = "POSIXct",
    ##                       tC = "POSIXct",
    ##                       td = "Date",
    ##                       tm = "Date",
    ##                       tq = "Date"),

    if (fmt \%in\% c("tc", "tC")) {
        ## posixlt uses seconds
        ret <- as.POSIXct(x / 1000, origin = STATA.ORIGIN,
                          format='\%Y-\%m-\%d', tz=tz)
        if (fmt == "tC") {
            ## adjust for leap seconds
            ## subtract the number of leap seconds prior to that time.
            lpSec <- .leap.seconds + seq_along(.leap.seconds)
            adjust <- cut(as.numeric(ret), breaks=c(-Inf, lpSec, Inf),
                          right=TRUE, include.lowest=TRUE, labels=FALSE) - 1
            ret <- ret - adjust
        }
    } else if (fmt == "td") {
        ret <- as.Date( x, origin='1960-01-01')
    } else if (fmt == "tw") {
        ## Stata uses a non-standard week format.
        ## Jan 1 always starts week 1.
        ## Jan 8 always starts week 2, etc.
        ## There is no week 53; week 52 has more than 7 days.
        yy <- floor(x / 52) + STATA.ORIGIN.Y
        wk <- floor(x \%\% 52) + 1
        frac <- (x \%\% 52) \%\% 1
        dstart <- as.Date(paste(yy, 1, 1, sep='-')) + (wk - 1) * 7
        dend <- as.Date(paste( yy + (wk==52), 1, 1, sep='-')) + (wk \%\% 52)  * 7
        ret <- dstart + frac * as.numeric(difftime(dend, dstart))
    } else if (fmt == "tm") {
        ## TODO
        yy <- floor(x / 12) + STATA.ORIGIN.Y
        mm <- floor(x \%\% 12) + 1
        frac <- (x \%\% 12) \%\% 1
        dstart <- as.Date(paste(yy, mm, 1, sep='-'))
        dend <- as.Date(paste(ifelse(mm == 12, yy + 1, yy),
                              ((mm \%\% 12) + 1), 1, sep='-'))
        ret <- dstart + frac * as.numeric(difftime(dend, dstart))
    } else if (fmt == "tq") {
        yy <- floor(x / 4) + STATA.ORIGIN.Y
        qtr <- floor(x \%\% 4) + 1
        frac <- (x \%\% 4) \%\% 1
        dstart <- as.Date(paste(yy, 3 * qtr - 2, 1, sep='-'),
                                format='\%Y-\%m-\%d')
        dend <- as.Date(paste((qtr == 3) + yy,
                              3 * ((qtr \%\% 4) + 1) - 2, 1, sep='-'), format='\%Y-\%m-\%d')
        ret <- dstart + frac * as.numeric(difftime(dend, dstart))
    } else if (fmt == "th") {
        yy <- floor(x / 2) + STATA.ORIGIN.Y
        half <- floor(x \%\% 2) + 1
        frac <- (x \%\% 2) \%\% 1
        dstart <- as.Date(paste(yy, 6 * half - 5, 1, sep='-'),
                                format='\%Y-\%m-\%d')
        dend <- as.Date(paste(yy + (half == 2),
                              6 * ((half \%\% 2) + 1) - 5, 1, sep='-'))
        ret <- dstart + frac * as.numeric(difftime(dend, dstart))
    } else if (fmt == "tg") {
        ## \%tg is a generic time format, one not specifically associated with dates.
        ## Since there is nothing for me to do, I just return the number.
        ret <- x
    }
    ret
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
