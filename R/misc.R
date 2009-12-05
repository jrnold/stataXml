STATA.ORIGIN <- as.Date('1960-1-1')
STATA.ORIGIN <- '1960-01-01'
STATA.ORIGIN.Y <- 1960

fromStataTime <- function(x, fmt, tz='') {

    ## convert.times <- list(tc = "POSIXct",
    ##                       tC = "POSIXct",
    ##                       td = "Date",
    ##                       tm = "Date",
    ##                       tq = "Date"),

    if (fmt %in% c("tc", "tC")) {
        ## posixlt uses seconds
        ret <- as.POSIXct(x / 1000, origin = STATA.ORIGIN,
                          format='%Y-%m-%d', tz=tz)
        if (fmt == "tc") {
            ## adjust for leap seconds
            ## subtract the number of leap seconds prior to that time.
            ## I think I need to do that iteratively.
            ## ret <- sapply(ret, function(y) ret - sum(ret >= .leap.seconds))
        }
    } else if (fmt == "td") {
        ret <- as.Date( x, origin='1960-01-01')
    } else if (fmt == "tw") {
        ## Stata uses a non-standard week format.
        ## Jan 1 always starts week 1.
        ## Jan 8 always starts week 2, etc.
        ## There is no week 53; week 52 has more than 7 days.
        yy <- floor(x / 52) + STATA.ORIGIN.Y
        wk <- floor(x %% 52) + 1
        frac <- (x %% 52) %% 1
        dstart <- as.Date(paste(yy, 1, 1, sep='-')) + (wk - 1) * 7
        dend <- as.Date(paste( yy + (wk==52), 1, 1, sep='-')) + (wk %% 52)  * 7
        ret <- dstart + frac * as.numeric(difftime(dend, dstart))
    } else if (fmt == "tm") {
        ## TODO
        yy <- floor(x / 12) + STATA.ORIGIN.Y
        mm <- floor(x %% 12) + 1
        frac <- (x %% 12) %% 1
        dstart <- as.Date(paste(yy, mm, 1, sep='-'))
        dend <- as.Date(paste(ifelse(mm == 12, yy + 1, yy),
                              ((mm %% 12) + 1), 1, sep='-'))
        ret <- dstart + frac * as.numeric(difftime(dend, dstart))
    } else if (fmt == "tq") {
        yy <- floor(x / 4) + STATA.ORIGIN.Y
        qtr <- floor(x %% 4) + 1
        frac <- (x %% 4) %% 1
        dstart <- as.Date(paste(yy, 3 * qtr - 2, 1, sep='-'),
                                format='%Y-%m-%d')
        dend <- as.Date(paste((qtr == 3) + yy,
                              3 * ((qtr %% 4) + 1) - 2, 1, sep='-'), format='%Y-%m-%d')
        ret <- dstart + frac * as.numeric(difftime(dend, dstart))
    } else if (fmt == "th") {
        yy <- floor(x / 2) + STATA.ORIGIN.Y
        half <- floor(x %% 2) + 1
        frac <- (x %% 2) %% 1
        dstart <- as.Date(paste(yy, 6 * half - 5, 1, sep='-'),
                                format='%Y-%m-%d')
        dend <- as.Date(paste(yy + (half == 2),
                              6 * ((half %% 2) + 1) - 5, 1, sep='-'))
        ret <- dstart + frac * as.numeric(difftime(dend, dstart))
    } else if (fmt == "tg") {
        ## %tg is a generic time format, one not specifically associated with dates.
        ## Since there is nothing for me to do, I just return the number.
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
