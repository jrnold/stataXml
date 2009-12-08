library(svUnit)
library(chron)
library(zoo)

test(asStataTime) <- function() {
    ## Date
    stDate <- asStataTime(as.Date('1960-1-1') + -1:1)
    checkEqualsNumeric(stDate, -1:1)
    checkEquals(attr(stDate, 'stata.format'), '%td')

    ## POSIXt -> %tc
    posxCt <- as.POSIXct('1960-1-1 00:00:00.00', '%Y-%n-%d %H:%M:%S') + c(-0.001, 0, 0.001)
    posxLt <- as.POSIXlt( c('1959-12-31 23:59:59.999',
                            '1960-1-1 00:00:00.000',
                            '1960-1-1 00:00:00.001'), tz='GMT')
    checkEqualsNumeric(asStataTime(posxCt), -1:1)
    checkEquals(attr(asStataTime(posxCt), 'stata.format'), '%tc')
    checkEqualsNumeric(asStataTime(posxLt), -1:1)
    checkEquals(attr(asStataTime(posxLt), 'stata.format'), '%tc')

    ## POSIXt -> %td
    posxCt2 <- as.POSIXct(c('1959-12-31', '1960-1-1', '1960-1-2'), tz='GMT')
    posxLt2 <- as.POSIXlt(posxCt2)
    checkEqualsNumeric(asStataTime(posxLt2, useTc=FALSE), -1:1)
    checkEquals(attr(asStataTime(posxLt2, useTc=FALSE), 'stata.format'), '%td')
    checkEqualsNumeric(asStataTime(posxCt2, useTc=FALSE), -1:1)
    checkEquals(attr(asStataTime(posxCt2, useTc=FALSE), 'stata.format'), '%td')

    ## chron
    stChron <- asStataTime(chron('1/1/1960') + -1:1)
    checkEqualsNumeric(stDate, -1:1)
    checkEquals(attr(stDate, 'stata.format'), '%td')

    ## yearqtr
    stYearqtr <- asStataTime(as.yearqtr(1960 + (-1:1)/4))
    checkEqualsNumeric(stYearqtr, -1:1)
    checkEquals(attr(stYearqtr, 'stata.format'), '%tq')

    ## yearmon
    stYearmon <- asStataTime(as.yearmon(1960 + (-1:1)/12))
    checkEqualsNumeric(stYearmon, -1:1)
    checkEquals(attr(stYearmon, 'stata.format'), '%tm')

    ## Regular numeric -> %tg
    tg <- asStataTime(-1:1)
    checkEqualsNumeric(tg, -1:1)
    checkEqualsNumeric(attr(tg, 'stata.format'), '%tg')

}
