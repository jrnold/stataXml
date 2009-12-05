library(svUnit)

test(fromStataTime) <- function() {

    ## tc
    ## the formatting will differ
    checkEquals(fromStataTime(-1:1, "tc"),
                as.POSIXct('1960-01-01 00:00:00') + -c(0.001, 0, 0.001))

    ## The formating will differ
    checkEquals(fromStataTime(-1:1, "tC"),
                as.POSIXct('1960-01-01 00:00:00') + -c(0.001, 0, 0.001))

    ## TODO: check that leap seconds are removed

    ## td
    checkEquals(fromStataTime(-1:1, "td"),
                as.Date('1960-01-01') + -1:1)

    checkEquals(trunc(fromStataTime(seq(-1, 1, 0.5), 'tw')),
                trunc(as.Date(c('1959-12-24', '1959-12-28', '1960-01-01',
                          '1960-01-04', '1960-01-08'))))

    ## tm
    checkEquals(trunc(fromStataTime(seq(-1, 1, 0.5), "tm")),
                as.Date(c('1959-12-01', '1959-12-16',
                          '1960-01-01', '1960-01-16', '1960-02-01')))

    checkEquals(fromStataTime(-13:12, "tm"),
                as.Date(c('1958-12-01',
                          paste(1959, 1:12, 1, sep='-'),
                          paste(1960, 1:12, 1, sep='-'),
                          '1961-01-01')))

    ## tq
    checkEquals(trunc(fromStataTime(seq(-1, 1, 0.5), "tq")),
                as.Date(c("1959-10-01", "1959-05-17", "1960-01-01",
                          "1960-02-15", "1960-04-01")))

    checkEquals(fromStataTime(-5:4, "tq"),
                as.Date(c('1958-10-01',
                          paste(1959, c(1,4,7,10), 1, sep='-'),
                          paste(1960, c(1,4,7,10), 1, sep='-'),
                          '1961-01-01')))

    ## th
    checkEquals(fromStataTime(-3:2, "th"),
                as.Date(c('1958-07-01',
                          paste(1959, c(1,7), 1, sep='-'),
                          paste(1960, c(1,7), 1, sep='-'),
                          '1961-01-01')))

    checkEquals(fromStataTime(seq(-1, 1, 0.5), "th"),
                as.Date(c('1959-07-01', '1959-10-01',
                          '1960-01-01', '1960-04-01', '1960-07-01')))

    ## tg
    checkEquals(fromStataTime(-1:1, "tg"), -1:1)

}
