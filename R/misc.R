stataVar <- function(df,
                     convert.times=list(tc = "chron",
                     tC = "chron",
                     td = "Date",
                     tm = "yearmon",
                     tq = "yearqtr"),
                     convert.dummy=TRUE) {

    miliSecPerDay <- 24 * 60 * 60 * 1000

    formats <- attr(df, "formats")
    formType <- substr(formats, 2, 3)

    for ( i in seq_len(length(formats))) {
        form <- formType[ i ]
        x <- names(df)[i]
        if ( form %in% names(convert.times) &&
            convert.times[[ form ]] == "none") {
            next
        }

        if ( form == "td") {
            if ( convert.times$td == "POSIX") {
                ## POSIX is number of seconds
                df[[ x ]] <- as.Date( df[[ x ]] * 24 * 60 * 60,
                                     origin='1960-01-01')
            } else if ( convert.times$td == "chron") {
                ## Chron is number of days
                df[[ x ]] <- chron(  df[[ x ]] , origin=c(1, 1, 1960))
            } else if ( convert.times$td == "Date") {
                df[[ x ]] <- as.Date( df[[ x ]], origin='1960-01-01')
            } else {
                stop( convert.times$td, "is not a valid class for %td")
            }
        } else if (form == "tq") {
            z <- yearqtr( df[[ x ]] / 4 + 1960)
            if ( convert.times$tq == "POSIX") {
                df[[ x ]] <- as.POSIXlt(df[[x]])
            } else if ( convert.times$tq == "chron") {
                df[[ x ]] <- as.chron(df[[x]])
            } else if ( convert.times$tq == "Date") {
                df[[ x ]] <- as.Date(df[[x]])
            } else {
                stop( convert.times$tq, "is not a valid class for %tq")
            }
        } else if (form == "tm") {
            z <- yearmon( df[[ x ]] / 12 + 1960)
            if ( convert.times$tm == "yearmon") {
                df[[ x ]] <- z
            } else if ( convert.times$tm == "POSIX") {
                df[[ x ]] <- as.POSIXlt(z)
            } else if ( convert.times$tm == "chron") {
                df[[ x ]] <- as.chron(z)
            } else if ( convert.times$tm == "Date") {
                df[[ x ]] <- as.Date(z)
            } else {
                stop( convert.times$tm, "is not a valid class for %tm")
            }
        } else if (form == "tc") {
             if ( convert.times$tc == "POSIX" ) {
                ## posixlt uses seconds
                df[[x]] <- as.POSIXlt(df[[x]] / 1000 ,
                                origin = '1960-01-01')
            } else {
                ## chron uses days
                df[[ x ]] <- chron(df[[x]] / miliSecPerDay,
                                   origin = c(1,1,1960))
            }
        } else if (form  == "tC") {
            z <- as.POSIXlt(df[[x]] / 1000 ,
                            origin = '1960-01-01')
            ## adjust for leap seconds
            ## uses the current time zone ??
            for (s in .leap.seconds) {
                isGt <- ( z >= s)
                z[ isGt ] <- z[ isGt ] - 1
            }
            if ( convert.times$tC == "POSIX" ) {
                df[[ x ]] <- z
            } else if( convert.times$tC == "chron" ) {
                df[[ x ]]  <- as.chron(z)
            } else {
                stop( convert.times$tc, "is not a valid class for %tC")
            }
        } else if (form %in% c("tw", "th")) {
            ## No equivalent R classes
            ## Could make some similar to yearmon and yearqtr
            ## however tw may be harder to account for number of weeks.
        } else if (form %in% c("ty", "tg")) {
            ## do nothing since already year format.
        }
    }

    ## Convert any dummy variables to logical
    for (x in names(df)) {
        if (all( df[[ x]]  %in% c( NA, 1, 0))) {
            df[[ x ]] <- as.logical(df[[ x ]] )
        }
    }

    return(df)
}

asStataTime <- function(x, hasTc=TRUE) {
    ## TODO: unit tests. Do -1, 0, 1 for the relevant stata units
    ## TODO: handle packages tis, ts, and timeDate
    ORIGIN.R <- as.Date('1970-1-1')
    ORIGIN.STATA <- as.Date('1960-1-1')

    if ( any(c(is(x, "Date"), is(x, 'dates'), is(x, 'times') ))) {
        ## Date class in base
        ## dates, times classes from chron
        y <- as.numeric(x) - as.numeric(ORIGIN.STATA)
        attr(y, 'stata.format') <- '%td'
    } else if (any(c(is(x, "POSIXlt"), is(x, "POSIXct"), is(x, 'POSIXt')))) {
        if (hasTc) {
            ## POSIXct is number of seconds since 1970
            ## %tc is number of miliseconds since 1960
            ## R ignores leap seconds as per POSIX, so I will convert to %tc and not %tC
            ## as.double unlike as as.POSIXct keeps the fractional seconds
            y <- unclass(as.double(x))
            ## I round to fix floating point errors
            y <- round((y - as.numeric(ORIGIN.STATA) * 86400) * 1000)
            attr(y, 'stata.format') <- '%tc'
        } else {
            ## If an older version of stata that doesn't support %tc (version < 10)
            ## Then convert to %td (dates)
            y <- as.numeric(as.Date(x)) - as.numeric(ORIGIN.STATA)
            attr(y, 'stata.format') <- '%td'
        }
    } else if (is(x, 'yearqtr')) {
        ## yearqtr from zoo
        ## time is represented year + fractional quarters
        yStata <- as.POSIXlt(ORIGIN.STATA)$year + 1900
        y <- (unclass(x) - yStata) * 4
        attr(y, 'stata.format') <- '%tq'
    } else if (is(x, 'yearmon')) {
        ## yearqtr from zoo
        ## time is represented year + fractional quarters
        yStata <- as.POSIXlt(ORIGIN.STATA)$year + 1900
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
