library(svUnit)

test(fromStataTime) <- function() {
    ## explicit timezone
    ## the output strings will not match
    checkEqualsNumeric(fromStataTime(-1:1, "tc", tz='GMT'),
                       as.POSIXct(c('1959-12-31 23:59:59.999',
                                    '1960-01-01 00:00:00.000',
                                    '1960-01-01 00:00:00.001'), tz='GMT'))

    ## implicit timezone
    checkEqualsNumeric(fromStataTime(-1:1, "tc"),
                       as.POSIXct(c('1959-12-31 23:59:59.999',
                                    '1960-01-01 00:00:00.000',
                                    '1960-01-01 00:00:00.001')))


    checkEqualsNumeric(fromStataTime(-1:1, "tC"),
                       as.POSIXct(c('1959-12-31 23:59:59.999',
                                    '1960-01-01 00:00:00.000',
                                    '1960-01-01 00:00:00.001')))

    ## TODO: check that leap seconds are removed
    checkEquals(fromStataTime(-1:1, "td"),
                as.Date('1960-01-01') + -1:1)

   ##  checkEquals(fromStataTime(-1:1, "tw"), ### ?)

    checkEquals(fromStataTime(-13:12, "tm"),
                as.Date(c('1958-12-01',
                          paste(1959, 1:12, 1, sep='-'),
                          paste(1960, 1:12, 1, sep='-'),
                          '1961-01-01')))

    checkEquals(fromStataTime(-5:4, "tq"),
                as.Date(c('1958-10-01',
                          paste(1959, c(1,4,7,10), 1, sep='-'),
                          paste(1960, c(1,4,7,10), 1, sep='-'),
                          '1961-01-01')))

    checkEquals(fromStataTime(-3:2, "th"),
                as.Date(c('1958-07-01',
                          paste(1959, c(1,7), 1, sep='-'),
                          paste(1960, c(1,7), 1, sep='-'),
                          '1961-01-01')))

    checkEquals(fromStataTime(-1:1, "tg"), -1:1)

}
