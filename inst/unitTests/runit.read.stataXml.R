checkFoo <- function(df) {
    ## Simple and obvious checks
    checkEquals(class(df), 'data.frame')
    checkEquals(dim(df), c(5, 7))

    ## Check classes of variables
    with(df, {
        checkEquals(class(vbyte), 'integer')
        checkEquals(class(vint), 'integer')
        checkEquals(class(vlong), 'integer')
        checkEquals(class(vfloat), 'numeric')
        checkEquals(class(vdouble), 'numeric')
        checkEquals(class(vstr), 'character')
        checkEquals(class(vlabel), 'factor')
    })

    ## Check that all variables have the right values
    for (x in 1:5) {
        checkEqualsNumeric(df[[x]], 1:nrow(df))
    }
    checkEquals(df[["vstr"]], letters[1:nrow(df)])
    checkEquals(levels(df$vlabel), letters[1:nrow(df)])

    ## Check attributes
    checkEquals(attr(df, 'version'), '113')
    checkTrue(is(attr(df, 'time.stamp'), 'POSIXlt'))
    checkEquals(attr(df, 'datalabel'), 'test data label')

    fmtlist <- c('%8.0g', '%8.0g', '%12.0g', '%9.0g', '%10.0g', '%9s', '%8.0g')
    names(fmtlist) <- names(df)
    checkTrue(all(attr(df, 'formats') == fmtlist))

    checkEquals( attr(df, 'types'),
                c('byte', 'int', 'long', 'float', 'double', 'str1', 'byte'))

    checkEquals( attr(df, 'val.labels'),
                c(rep('', 6), 'alpha'))

    checkEquals(attr(df, 'var.labels'),
                c(paste( c('byte', 'int', 'long', 'float', 'double', 'string'),
                        'variable'),
                  'variable with labels'))

    checkEquals(attr(df, 'sort'), c('vbyte', 'vint'))

    ## char should return a list of named character vectors
    checkEquals(attr(df, 'char'),
                list('vbyte'=c(note1='1st vbyte note', note0='1'),
                     '_dta'=c(note1='1st data note', note0='1')))

    ## label.table
    alpha <- as.integer(1:nrow(df))
    names(alpha) <- letters[1:nrow(df)]
    alpha <- list(alpha=alpha)
    checkEquals(attr(df, 'label.table'), alpha)

    checkEquals(attr(df, 'missing'), list())

    checkEquals(attr(df, 'dta_type'), 'xml')

}

checkMissings <- function(df) {
    checkEquals(dim(df), c(28, 5))

    ## The first row is 0, all other rows are missing
    for ( j in 1:ncol(df)) {
        checkEquals(df[[j]], c(0, rep(NA, nrow(df) - 1)))
    }

    ## are the missing values associated with factors with levels
    ## '.', '.a', '.b', ..., '.z'
    for (x in names(df)) {
        checkEquals(attr(df, 'missing')[[x]],
                    factor(paste('.', c('', letters), sep='')))
    }
}

checkDateTimes <- function(df) {

    checkEquals(dim(df), c(3, 8))

    checkEquals(attr(df, 'formats'),
                paste('%', names(df) , sep=''))

    ## Date classes returned by fromStataTime
    checkTrue(all(mapply(function(x, y) is(x, y),
                         df,
                         c("POSIXct", "POSIXct", "Date", "Date",
                           "Date", "Date", "Date", "integer"))))

    ## Since I have separate unit tests for fromStataTime
    ## I won't duplicate them here.

}

test(read.stataXml) <- function() {

    ## Check Reading compact xml dta
    foo <- read.stataXml("../tests/foo.xml")
    checkFoo(foo)

    ## Testing missing values
    missings <- read.stataXml("../tests/missings.xml")
    checkMissings(missings)

    ## Testing Dates and Times
    dateTimes <- read.stataXml("../tests/dateTimes.xml")
    checkDateTimes(dateTimes)

    ## Check reading legibible dta xml
    foo <- read.stataXml("../tests/bar.xml")
    checkFoo(foo)

}
