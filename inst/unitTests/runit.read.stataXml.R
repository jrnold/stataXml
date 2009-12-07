test(read.stataXml) <- function() {
    foo <- read.stataXml("../tests/foo.xml")

    ## Simple and obvious checks
    checkEquals(class(foo), 'data.frame')
    checkEquals(dim(foo), c(5, 7))

    ## Check classes of variables
    with(foo, {
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
        checkEqualsNumeric(foo[[x]], 1:nrow(foo))
    }
    checkEquals(foo[["vstr"]], letters[1:nrow(foo)])
    checkEquals(levels(foo$vlabel), letters[1:nrow(foo)])

    ## Check attributes
    checkEquals(attr(foo, 'version'), '113')
    checkTrue(is(attr(foo, 'time.stamp'), 'POSIXlt'))
    checkEquals(attr(foo, 'datalabel'), 'test data label')

    fmtlist <- c('%8.0g', '%8.0g', '%12.0g', '%9.0g', '%10.0g', '%9s', '%8.0g')
    names(fmtlist) <- names(foo)
    checkTrue(all(attr(foo, 'formats') == fmtlist))

    checkEquals( attr(foo, 'types'),
                c('byte', 'int', 'long', 'float', 'double', 'str1', 'byte'))

    checkEquals( attr(foo, 'val.labels'),
                c(rep('', 6), 'alpha'))

    checkEquals(attr(foo, 'var.labels'),
                c(paste( c('byte', 'int', 'long', 'float', 'double', 'string'), 'variable'),
                  'variable with labels'))

    checkEquals(attr(foo, 'sort'), c('vbyte', 'vint'))

    ## char should return a list of named character vectors
    checkEquals(attr(foo, 'char'), list('vbyte'=c(note1='1st byte note', note0='1'),
                                        '_dta'=c(note1='1st data note', note0='1')))

    ## label.table
    alpha <- as.integer(1:nrow(foo))
    names(alpha) <- letters[1:nrow(foo)]
    alpha <- list(alpha=alpha)
    checkEquals(attr(foo, 'label.table'), alpha)

    checkEquals(attr(foo, 'missing'), list())

    checkEquals(attr(foo, 'dta_type'), 'xml')

}
