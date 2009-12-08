test(write.stataXml) <- function() {
    foo <- data.frame(vlogical=as.logical(1:5 %% 2),
                      vnum=as.numeric(1:5),
                      vchar=as.character(letters[1:5]),
                      vfactor=as.factor(letters[1:5]),
                      vint0=as.integer(rep(100, 5)),  ## just below max int
                      vint1=as.integer(rep(-127, 5)),
                      vint2=as.integer(rep(101, 5)),
                      vint3=as.integer(rep(-128, 5)),
                      vint4=as.integer(rep(-32768, 5)),
                      vint5=as.integer(rep(32741, 5)),
                      stringsAsFactors=FALSE)

    datalabel <- 'testing 1,2,3'
    variableLabels <- paste('a variable named', names(foo))
    sortlist <- c('alpha', 'bravo')

    char <- list('_dta'=c('note0'='1', 'note1'='one fish'),
                 'alpha'=c('note0'='1', 'note1'='two fish'))

    ## Write data
    ## For all of these tests, I write to a temporary file, and
    ## then I read back in the xml using read.stataXml to check
    ## that I wrote the correct thing.
    write.stataXml(foo, tmpfn <- tempfile(),
                   convert.factors='labels',
                   sortlist=sortlist,
                   datalabel=datalabel,
                   variableLabels=variableLabels,
                   char=char)
    ## Check variables
    bar <- read.stataXml(tmpfn)
    checkEquals(foo$vlogical, as.logical(bar$vlogical))
    for ( v in names(foo)[ names(foo) != 'vlogical']) {
        checkEquals(foo[[v]], bar[[v]])
    }

    ## Check attributes
    checkEquals(attr(bar, 'version'), '113')
    checkTrue(is(attr(bar, 'time.stamp'), 'POSIXlt'))
    checkEquals(attr(bar, 'datalabel'), datalabel)
    checkTrue( all( attr(bar, 'formats') != ''))
    checkEquals(attr(bar, 'types'),
                c('byte', 'double', 'factor', 'byte', 'byte',
                  'int', 'int', 'long', 'long'))

    valLabels <- rep('', ncol(bar))
    valLabels[ 4 ] <- 'vfactor'
    checkEquals(attr(bytes, 'val.labels'), valLabels)
    checkEquals(attr(bar, 'var.labels'), variableLabels)
    checkEquals(attr(bar, 'sort'), sortlist)
    checkEquals(attr(bar, 'char'), char)
    checkEquals(attr(bar, 'missing'), list())

    ## Different Factor options
    ## factors -> string
    write.stataXml(foo, tmpfn1 <- tempfile(),
                   convert.factors='string',
                   sortlist=sortlist,
                   datalabel=datalabel,
                   variableLabels=variableLabels,
                   char=char)
    bar1 <- read.stataXml(tmpfn1)
    checkTrue(is.character(bar1$vfactor))
    checkEquals(attr(bar1, 'val.labels'), rep('', ncol(bar1)))
    checkTrue( attr(bar1, 'label.table') == list())

    ## factors -> numeric
    ## only works when factor levels are 'numeric', e.g. '1', '2', '3', ...
    ## write.stataXml(foo, tmpfn2 <- tempfile(),
    ##                convert.factors='numeric',
    ##                sortlist=sortlist,
    ##                datalabel=datalabel,
    ##                variableLabels=variableLabels,
    ##                char=char)
    ## bar2 <- read.stataXml(tmpfn2)
    ## checkTrue(is.character(bar2$vfactor))
    ## checkEquals(attr(bar2, 'val.labels'), rep('', ncol(bar2)))
    ## checkTrue( attr(bar2, 'label.table') == list())

    ## factors -> log
    write.stataXml(foo, tmpfn3 <- tempfile(),
                   convert.factors='codes',
                   sortlist=sortlist,
                   datalabel=datalabel,
                   variableLabels=variableLabels,
                   char=char)
    bar3 <- read.stataXml(tmpfn3)
    checkTrue(is.integer(bar3$vfactor))
    checkEquals(attr(bar3, 'val.labels'), rep('', ncol(bar3)))
    checkTrue( attr(bar3, 'label.table') == list())

    ### Check writing floats
    write.stataXml(foo, tmpfn4 <- tempfile(),
                   convert.factors='codes',
                   sortlist=sortlist,
                   datalabel=datalabel,
                   variableLabels=variableLabels,
                   char=char,
                   double=FALSE)
    qux <- read.stataXml(tmpfn4)
    checkTrue(is.numeric(qux$vnum))
    checkEquals(attr(qux, 'types')[2], 'float')

    ## Default values
    write.stataXml(foo, tmpfn5 <- tempfile())
    quux <- read.stataXml(tmpfn5)
    checkEquals(attr(quux, 'val.labels')[4], 'vfactor')
    checkEquals(attr(quux, 'datalabel'), 'Written by R.')
    checkEquals(attr(quux, 'var.labels'), rep('', ncol(quux)))
    checkEquals(attr(quux, 'sort'), list())
    ## names do not match
    checkTrue(attr(quux, 'char') == list())

    ## Check that using verbose doesn't make a difference
    write.stataXml(foo, tmpfn6 <- tempfile(),
                   convert.factors='labels',
                   sortlist=sortlist,
                   datalabel=datalabel,
                   variableLabels=variableLabels,
                   char=char, verbose=TRUE)
    bar2 <- read.stataXml(tmpfn6)
    checkIdentical(bar, bar2)
}
