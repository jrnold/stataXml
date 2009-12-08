  ## class creation
yearqtr <- function(x) structure(floor(2*x + .001)/2, class = "yearhalf")

## coercion to yearqtr: always go via numeric
as.yearhalf <- function(x, ...) UseMethod("as.yearhalf")
as.yearhalf.default <- function(x, ...) as.yearhalf(as.numeric(x))
as.yearhalf.numeric <- function(x, ...) structure(floor(2*x + .0001)/2, class = "yearhalf")
as.yearhalf.integer <- function(x, ...) structure(x, class = "yearhalf")

# as.jul.yearhalf <- function(x, ...) jul(as.Date(x, ...)) # jul is from tis
as.yearhalf.jul <- # jul is in tis package
as.yearhalf.timeDate <-
as.yearhalf.dates <-
as.yearhalf.Date <-
as.yearhalf.POSIXt <- function(x, ...) as.yearhalf(as.yearmon(x))
as.yearhalf.yearhalf <- function(x, ...) x

as.yearhalf.factor <- function(x, ...) as.yearhalf(as.character(x), ...)
as.yearhalf.character <- function(x, format, ...) {
    non.na <- x[!is.na(x)]
    if (length(non.na) == 0)
        return(structure(rep(NA, length(x)), class = "yearqtr"))
    if (missing(format) || format == "") {
        format <- if (all(regexpr("h", non.na) > 0))  { "%Y h%h"
        } else if (all(regexpr("Q", non.na) > 0)) { "%Y H%h"
        } else "%Y-%h"
    }
    y <- if (regexpr("%[hH]", format) > 0) {
        format <- sub("%h", "%m", format)
        y <- as.numeric(as.yearmon(x, format))
        m0 <- round(12 * (y %% 1))
        floor(y) + ifelse(m0 > 1, NA, m0/2)
    } else as.yearmon(x, format)
    as.yearhalf(y)
}
as.yearhalf.ti <- function(x, ...) as.yearhalf(as.Date(x), ...)

## coercion from yearqtr
# returned Date is the fraction of the way through the period given by frac
as.Date.yearhalf <- function(x, frac = 0, ...) {
	x <- unclass(x)
	year <- floor(x + .001)
        mon <- (round(2 * (x - year)) * 6) + 1
        dd.start <- as.Date(paste(year, mon, 1, sep="-"))
        dd.end <- as.Date(paste(year + (mon == 7),  ## next year if july
                                (mon + 6) %% 12,
                                1, sep="-"))
        dd.start + as.numeric(dd.end - dd.start) * frac
}
as.POSIXct.yearhalf <- function(x, tz = "", ...) as.POSIXct(as.Date(x), tz = tz, ...)
as.POSIXlt.yearhalf <- function(x, tz = "", ...) as.POSIXlt(as.Date(x), tz = tz, ...)
as.numeric.yearhalf <- function(x, ...) unclass(x)
as.character.yearhalf <- function(x, ...) format.yearhalf(x, ...)
as.data.frame.yearhalf <- function(x, row.names = NULL, optional = FALSE, ...)
{
  nrows <- length(x)
  nm <- paste(deparse(substitute(x), width.cutoff = 500), collapse = " ")
  if (is.null(row.names)) {
    if (nrows == 0)
        row.names <- character(0)
    else if(length(row.names <- names(x)) == nrows && !any(duplicated(row.names))) {
    }
    else if(optional) row.names <- character(nrows)
    else row.names <- seq_len(nrows)
  }
  names(x) <- NULL
  value <- list(x)
  if(!optional) names(value) <- nm
  attr(value, "row.names") <- row.names
  class(value) <- "data.frame"
  value
}


## other methods for class yearqtr
c.yearhalf <- function(...) {
    as.yearhalf(do.call("c", lapply(list(...), as.numeric)))
}

cycle.yearhalf <- function(x, ...) as.numeric(quarters(x))

format.yearhalf <- function(x, format = "%Y H%h", ...)
{
    if (length(x) == 0) return(character(0))
    ## like gsub but replacement and x may be vectors the same length
    gsub.vec <- function(pattern, replacement, x, ...) {
        y <- x
        for(i in seq_along(x)) {
            y[i] <- gsub(pattern, replacement[i], x[i], ...)
        }
        y
    }
    x <- as.yearhalf(x)
    x <- unclass(x)
    year <-floor(x + .001)
    half <- round(2 * (x - year)) + 1
    if (format == "%Y H%h") {
        xx <- paste(year, " H", half, sep = "")
    } else {
        xx <- gsub.vec("%h", half, rep(format, length(half)))
        xx <- gsub.vec("%Y", year, xx)
        xx <- gsub.vec("%y", sprintf("%02d", year %% 100), xx)
        xx <- gsub.vec("%C", year %/% 100, xx)
        names(xx) <- names(x)
    }
    xx
}

## halves <- function(x) {
##     hh <- (as.POSIXlt(x)$months < 7) + 1
##     paste("H",
## }

months.yearqtr <- function(x, abbreviate) {
    months(as.Date(x), abbreviate)
}

quarters.yearqtr <- function(x, abbreviate) {
    quarters(as.Date(x), abbreviate)
}


print.yearqtr <- function(x, ...) {
    print(format(x), ...)
    invisible(x)
}

"[.yearhalf" <- function (x, ..., drop = TRUE)
{
    cl <- oldClass(x)
    class(x) <- NULL
    val <- NextMethod("[")
    class(val) <- cl
    val
}

MATCH.yearqtr <- function(x, table, nomatch = NA, ...)
    match(floor(2*as.numeric(x) + .001),
          floor(2*as.numeric(table) + .001), nomatch = nomatch, ...)

Ops.yearqtr <- function(e1, e2) {
    e1 <- as.numeric(as.yearhalf(e1))
    e2 <- as.numeric(as.yearhalf(e2))
    rval <- NextMethod(.Generic)
    if(is.numeric(rval)) rval <- yearhalf(rval)
    return(rval)
}


"-.yearhalf" <- function (e1, e2)
{
    if (!inherits(e1, "yearhalf"))
        stop("Can only subtract from yearhalf objects")
    if (nargs() == 1)
	return(- as.numeric(e1))
    if (inherits(e2, "yearhalf"))
        return(as.numeric(e1) - as.numeric(e2))
    if (!is.null(attr(e2, "class")))
      stop("can only subtract yearhalf objects and numbers from yearhalf objects")
    yearhalf(unclass(e1) - e2)
}

is.numeric.yearhalf <- function(x) FALSE

Axis.yearhalf <- function(x = NULL, at = NULL, ..., side, labels = NULL)
    axis.yearhalf(x = x, at = at, ..., side = side, labels = TRUE)

axis.yearqtr <- function (side, x, at, format, labels = TRUE, ..., N1 = 25, N2 = 7) {
    # If years in range > N1 then only years shown.
    # If years in range > N2 then quarter ticks are not labelled.
    mat <- missing(at) || is.null(at)
    if (!mat) # at not missing
        x <- as.yearqtr(at)
    else x <- as.yearqtr(x)
    range <- par("usr")[if (side%%2)
        1:2
    else 3:4]
    # range[1] <- ceiling(range[1])
    # range[2] <- floor(range[2])
    d <- range[2] - range[1]
    z <- c(range, x[is.finite(x)])
    class(z) <- "yearqtr"
    if (d > N1) { # axis has years only
        z <- structure(pretty(z), class = "yearqtr")
    } else if (d > N2) { # axis has all years and unlabelled quarters
        z <- seq(min(x), max(x), 0.25)
	# z <- seq(floor(min(x)), ceiling(max(x)))
    } else { # years and quarters
        z <- seq(min(x), max(x), 0.25)
    }
    if (!mat)
        z <- x[is.finite(x)]
    z <- z[z >= range[1] & z <= range[2]]
    z <- sort(unique(z))
    class(z) <- "yearqtr"
    if (identical(labels, TRUE)) {
	if (missing(format)) format <- c("%Y", "Q%q")
	if (length(format) == 1) format <- c(format, "")
	if (d <= N2) labels <- format.yearqtr(z, format = format[2])
	idx <- format.yearqtr(z, format = "%q") == "1"
	labels <- rep(NA, length(z))
	labels[idx] <- format.yearqtr(z[idx], format = format[1])
    } else if (identical(labels, FALSE))
        labels <- rep("", length(z))
    axis(side, at = z, labels = labels, ...)
}

summary.yearhalf <- function(object, ...)
  summary(as.numeric(object), ...)

## convert from package date
as.yearhalf.date <- function(x, ...) {
	as.yearhalf(as.Date(x, ...))
}

mean.yearhalf <- function (x, ...) as.yearhalf(mean(unclass(x), ...))

Summary.yearhalf <- function (..., na.rm)
{
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if (!ok) stop(.Generic, " not defined for yearhalf objects")
    val <- NextMethod(.Generic)
    class(val) <- oldClass(list(...)[[1]])
    val
}

Sys.yearqtr <- function() as.yearqtr(Sys.Date())

range.yearqtr <- function(..., na.rm = FALSE) {
	as.yearqtr(range.default(..., na.rm = na.rm))
}

unique.yearqtr <- function(x, incomparables = FALSE, ...) {
	as.yearqtr(unique.default(x, incomparables = incomparables, ...))
}

xtfrm.yearqtr <- function(x) as.numeric(x)
