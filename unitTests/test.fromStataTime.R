library(svUnit)
test(fromStataTime) <- function() {
    checkEquals(fromStataTime(-1:1, "tc"),
                as.POSIXlt(c('1959-01-01 23:59:59.999',
                             '1960-01-01 00:00:00.000',
                             '1960-01-01 00:00:00.001')))

    checkEquals(fromStataTime(-1:1, "tC"),
                as.POSIXlt(c('1959-01-01 23:59:59.999',
                             '1960-01-01 00:00:00.000',
                             '1960-01-01 00:00:00.001')))
    ## TODO: check that leap seconds are removed

    checkEquals(fromStataTime(-1:1, "td"),
                as.Date('1960-01-01') + -1:1)

   ##  checkEquals(fromStataTime(-1:1, "tw"), ### ?)

    checkEquals(fromStataTime(-1:1, "tm"),
                as.Date(c('1959-12-01', '1960-01-01', '1960-01-02')))

    checkEquals(fromStataTime(-1:1, "tq"),
                as.Date(c('1959-10-01', '1960-01-01', '1960-04-01')))

    checkEquals(fromStataTime(-1:1, "th"),
                as.Date(c('1959-07-01', '1960-01-01', '1960-07-01')))

    checkEquals(fromStataTime(-1:1, "tg"), -1:1)

}
