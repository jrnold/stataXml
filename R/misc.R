STATA.ORIGIN <- as.Date('1960-1-1')

fromStataTime <- function(x, fmt) {

    ## convert.times <- list(tc = "POSIXct",
    ##                       tC = "POSIXct",
    ##                       td = "Date",
    ##                       tm = "Date",
    ##                       tq = "Date"),

    if (fmt %in% c("tc", "tC")) {
        ## posixlt uses seconds
        z <- as.POSIXct(z / 1000, origin = STATA.ORIGIN, tz=tz)
        if (fmt == "tc") {
            ## adjust for leap seconds
            ## subtract the number of leap seconds prior to that time.
            ## I think I need to do that iteratively.
            z <- sapply(z, function(y) z - sum(z >= .leap.seconds))
        }
    } else if (fmt == "td") {
        ret <- as.Date( x, origin='1960-01-01')
    } else if (fmt == "tw") {
        ## TODO. I need to figure out how Stata counts weeks.
        ret <- 0 + STATA.ORIGIN
    } else if (fmt == "tm") {
        ## TODO
        ret <- 0 + STATA.ORIGIN
    } else if (fmt == "tq") {
        ## TODO
        ret <- 0 + STATA.ORIGIN
    } else if (fmt == "th") {
        ## TODO
        ret <- 0 + STATA.ORIGIN
    } else if (fmt == "tg") {
        ## general format. do nothing.
        ret <- x
    }
    ret
}


asStataTime <- function(x, hasTc=TRUE) {
    ## TODO: unit tests. Do -1, 0, 1 for the relevant stata units
    ## TODO: handle packages tis, ts, and timeDate


    if ( any(c(is(x, "Date"), is(x, 'dates'), is(x, 'times') ))) {
        ## Date class in base
        ## dates, times classes from chron
        y <- as.numeric(x) - as.numeric(STATA.ORIGIN)
        attr(y, 'stata.format') <- '%td'
    } else if (any(c(is(x, "POSIXlt"), is(x, "POSIXct"), is(x, 'POSIXt')))) {
        if (hasTc) {
            ## POSIXct is number of seconds since 1970
            ## %tc is number of miliseconds since 1960
            ## R ignores leap seconds as per POSIX, so I will convert to %tc and not %tC
            ## as.double unlike as as.POSIXct keeps the fractional seconds
            y <- unclass(as.double(x))
            ## I round to fix floating point errors
            y <- round((y - as.numeric(STATA.ORIGIN) * 86400) * 1000)
            attr(y, 'stata.format') <- '%tc'
        } else {
            ## If an older version of stata that doesn't support %tc (version < 10)
            ## Then convert to %td (dates)
            y <- as.numeric(as.Date(x)) - as.numeric(STATA.ORIGIN)
            attr(y, 'stata.format') <- '%td'
        }
    } else if (is(x, 'yearqtr')) {
        ## yearqtr from zoo
        ## time is represented year + fractional quarters
        yStata <- as.POSIXlt(STATA.ORIGIN)$year + 1900
        y <- (unclass(x) - yStata) * 4
        attr(y, 'stata.format') <- '%tq'
    } else if (is(x, 'yearmon')) {
        ## yearqtr from zoo
        ## time is represented year + fractional quarters
        yStata <- as.POSIXlt(STATA.ORIGIN)$year + 1900
        y <- (unclass(x) - yStata) * 12
        attr(y, 'stata.format') <- '%tm'
    } else {
        ## I don't know what to do with other stuff.
        ## option 1. Treat as dates.
        ## optino 2, use %tg
        y <- as.numeric(x)
        attr(y, 'stata.format') <- '%tg'
    }
    ##attr(y, 'class') <- 'stataTime'
    y
}
