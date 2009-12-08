library(XML)
## CONSTANTS

## Classes that can be converted to Stata dates and times by asStataTime
DATETIME.CLASSES <- c("Date", 'dates', 'times', 'POSIXlt', 'POSIXct',
                      'yearqtr', 'yearmon')


## Origin date for Stata Date Variables
ST.EPOCH <- as.Date("1960-01-01")
ST.EPOCH.Y <- 1960

## Length of variable label
ST.LABELVAR <- 80L
## length of data label
ST.LABELDATA <- ST.LABELVAR
## length of value label string
ST.LABELVALSTR <- 32000L
## Length of variable name
ST.VARNAME <-32L
## Length of value label name
ST.LABELVALNAME <- ST.VARNAME
# length of one characteristic
ST.CHAR <- 67784L

## Variable Type Limits and information
ST.BYTE.BYTES <- 1L
ST.BYTE.MIN <- -127L
ST.BYTE.MAX <- 100L
ST.INT.BYTES <- 2L
ST.INT.MIN <- -32767L
ST.INT.MAX <- 32740L
ST.LONG.BYTES <- 4L
ST.LONG.MIN <- -2147483647L
ST.LONG.MAX <- 2147483620L

ST.FLOAT.BYTES <- 4L
ST.FLOAT.MIN <- -1.70141173319e+38
ST.FLOAT.MAX <- 1.70141173319e+38

ST.DOUBLE.BYTES <- 8L
ST.DOUBLE.MIN <- -8.9884656743e+307
ST.DOUBLE.MAX <- 8.9884656743e+307

ST.STR.MAX <- 244L

ST.TIMESTAMP <- "%d %b %Y %H:%M"
ST.FILETYPE <- 1
ST.XMLHEADER <- '<?xml version="1.0" encoding="US-ASCII" standalone="yes"?>\n'

.convertUnderscores <- function(x) gsub(x, "_", ".")

## Utility Functions for names and strings
stataVarname <- function(x) {
    gsub("\\.", "_", abbreviate(make.names(x), 32L))
}
stataValLabelName <- function(...) stataVarname(...)

stataVarLabel <- function(x) substr(x, 1, ST.LABELVAR)

stataDataLabel <- function(x) substr(x, 1, ST.LABELDATA)

stataStr <- function(x) substr(x, 1, ST.STR.MAX)

stataValLabelStr <- function(x) substr(x, 1, ST.LABELVALSTR)

stataCharStr <- function(x) substr(x, 1, ST.CHAR)

stataTypeToRclass <- function(x) {
    stataTypeToRClasses <- list(byte="integer",
                                int="integer",
                                long="integer",
                                float="numeric",
                                double="numeric",
                                ## all str1-str244
                                str="character")

    x[ grep("^str", x) ] <- "str"
    sapply(x, function(y) stataTypeToRClasses[[y]])
}

.findStataMissings <- function(x, na.rm=TRUE) {
    ## if not a missing value then NA is returned
    ## if a missing value, then index of c(".", ..., ".z") returned.
    ## I subtract 1 to get 0-26, and the NAs remain NAs
    missings <- paste(".", c("", letters), sep="")
    ## I return a factor so that it is easier to interpret than 0-26
    ret <- match(x, missings) - 1
    if (na.rm) {
       ret <- ret[ ! is.na(ret) ]
    }
    factor(ret, levels=0:26, labels=missings)
}

read.stataXml <- function(file,
                          convert.dates=TRUE,
                          convert.factors=TRUE,
                          convert.underscore=FALSE,
                          missing.type=TRUE)
{
    doc <- xmlParse(file)

    ## Header ##

    ## version of the dataset
    ds_format <- xmlValue(getNodeSet(doc, "//ds_format[1]")[[1]])

    ## byte order LOHI or HILO
    ## byteorder <- xmlValue(getNodeSet(doc, "/dta/header/byteorder[1]")[[1]])
    ## filetype <- xmlValue(getNodeSet(doc, "/dta/header/filetype[1]")[[1]])
    nvar <- as.integer(xmlValue(getNodeSet(doc, "/dta/header/nvar[1]")[[1]]))
    nobs <- as.integer(xmlValue(getNodeSet(doc, "/dta/header/nobs[1]")[[1]]))
    datalabel <- xmlValue(getNodeSet(doc, "/dta/header/data_label[1]")[[1]])
    timestamp <- strptime(xmlValue(getNodeSet(doc, "/dta/header/time_stamp[1]")[[1]]),
                          ST.TIMESTAMP)

    ## Descriptors

    ## Variable types
    typelist <- xpathSApply(doc, "/dta/descriptors/typelist/type", xmlValue)
    ## variable list
    varlist <- xpathSApply(doc, "/dta/descriptors/varlist/variable", xmlGetAttr, name="varname")
    ## sort list
    srtlist <- xpathSApply(doc, "/dta/descriptors/srtlist/sort", xmlGetAttr, name='varname')
    ## format list
    fmtlist <- xpathSApply(doc, "/dta/descriptors/fmtlist/fmt", xmlValue)
    ## value labels attached to each variable
    lbllist <- xpathSApply(doc, "/dta/descriptors/lbllist/lblname", xmlValue)

    ## Variable Labels
    vlabels <- xpathSApply(doc, "/dta/variable_labels/vlabel", xmlValue)

    ## Expansions
    ## currently only char (including notes)
    char <- list()
    charNodes <- getNodeSet(doc, "/dta/expansion/char")
    vnameList <- unique(sapply(charNodes, xmlGetAttr, name="vname"))
    char <- replicate(length(vnameList), list())
    names(char) <- vnameList
    for (node in charNodes) {
        vname <- xmlGetAttr(node, name="vname")
        charname <- xmlGetAttr(node, name="name")
        char[[ c(vname, charname) ]] <- xmlValue(node)
    }
    ## coerce list of lists to a list of character vectors
    char <- lapply(char, unlist)

    ## Value Labels
    valueLabels <- getNodeSet(doc, "/dta/value_labels/vallab")
    vallab <- vector(mode="list", length=length(valueLabels))
    names(vallab) <- sapply(valueLabels, xmlGetAttr, name="name")
    for ( node in valueLabels) {
        labname <- xmlGetAttr(node, "name")
        values <- as.integer(xpathSApply(node, "//label", xmlGetAttr, name="value"))
        names(values) <- xpathSApply(node, "//label", xmlValue)
        vallab[[ labname ]] <- values
    }

    ## Create R Class equivalents for Stata Types
    colClasses <- stataTypeToRclass(typelist)

    ## if types of missing values are stored initialize the list.
    if (missing.type) {
        missingValues <- list()
    }

    ## Parse Data
    df <- data.frame( row.names=as.character(1:nobs))
    for ( j in seq_along(varlist)) {
        x <- varlist[j]
        var <- xpathSApply(doc, "//o", function(obs, j) xmlValue(obs[[j]]), j=j)
        vartype <- colClasses[j]

        ## Missing values
        if (vartype != "character") {

            ## Keep track of the types of missing values
            if (missing.type) {
                ## Only keep values for the missing variables to conserve memory
                xMiss <- .findStataMissings(var)
                if ( length(xMiss)) {
                    missingValues[[ x ]] <- xMiss
                }
            }

            ## replace missings with "". they will be converted to NA by "as"
            ## this gets rid of the warnings.
            var[ grep("^\\.[a-z]?$", var, perl=TRUE) ] <- ""
        }
        var <- as(var, vartype)

        ## if it is a factor
        if ( convert.factors ) {
            lbl <- lbllist[j]
            if ( lbl != "") {
                var <- factor(as.integer(var),
                              labels=names(vallab[[ lbl ]]),
                              levels=vallab[[ lbl ]])
            }
            df[[x]] <- var
        }
    }
    rm(var)

    ## Post processing
    ## Dates
    if (convert.dates) {
        for (i in grep('^%t[cCdwmqh]', fmtlist)) {
            fmt <- substr(fmtlist[i], 2, 3)
            df[[i]] <- fromStataTime(df[[i]], fmt)
        }
    }

    ## Convert underscores in variable names
    if (convert.underscore)  {
        names(df) <- .convertUnderscores(names(df))
    }

    ## Add attributes
    attr(df, "version") <- ds_format
    attr(df, "time.stamp") <- timestamp
    attr(df, "datalabel") <- datalabel
    attr(df, "formats") <- fmtlist
    attr(df, "types") <- typelist
    attr(df, "val.labels") <- lbllist
    attr(df, "var.labels") <- vlabels
    attr(df, "sort") <- srtlist
    attr(df, "char") <- char
    attr(df, "label.table") <- vallab
    attr(df, "dta_type") <- "xml"
    if (missing.type) {
        attr(df, "missing") <- missingValues
    }

    ## Free up memory
    free(doc)

    ## Return new dataframe
    df
}

write.stataXml <- function(dataframe, file,
                           convert.factors="labels",
                           sortlist=character(),
                           fmtlist=NULL,
                           typelist=NULL,
                           datalabel="Written by R.",
                           variableLabels=rep("", ncol(dataframe)),
                           char=list(),
                           verbose=FALSE,
                           double=TRUE)
{
    ## Header Info
    dsFormat <- 113
    ## The byte order doesn't matter in these xml files, but it's still
    ## included for some reason.
    if (.Platform$endian == "little") {
        byteorder <- "LOHI"
    } else {
        byteorder <- "HILO"
    }
    filetye <- ST.FILETYPE

    ## Variables and Observations
    nvar <- ncol(dataframe)
    nobs <- nrow(dataframe)

    ## Dataset label
    datalabel <- stataDataLabel(datalabel)

    ## Variable List
    ## Ensure dataframe names so that they are compatible with stata
    names(dataframe) <- stataVarname(names(dataframe))
    ## Variable list
    varlist <- names(dataframe)

    ## Value Labels
    if (! convert.factors %in% c('string', 'numeric', 'codes')) {
        ## List with an entry for every variable
        ## non-factor variables will have NULL as their element
        valueLabels <- lapply(dataframe,
                              function(x) {
                                  stataValLabelStr(levels(x))
                              })
        ## Keep only defined values
        valueLabels <- valueLabels[ sapply(valueLabels, function(x) length(x) > 0) ]
    } else {
        valueLabels <- list()
    }

    ## Cleaning Data Frame ###
    ## converting factors
    factors <- which(sapply(dataframe, is.factor))
    for(v in factors) {
        if(convert.factors == "string") {
            dataframe[[v]] <- as.character(dataframe[[v]])
        } else if (convert.factors == "numeric") {
            dataframe[[v]] <- as.integer(as.character(dataframe[[v]]))
        } else if (convert.factors == "codes") {
            dataframe[[v]] <- as.integer(dataframe[[v]])
        } else {
            dataframe[[v]] <- as.integer(dataframe[[v]])
        }
    }

    ## Truncate character variables at Stata max string length
    strVars <- which(sapply(dataframe,
                            function(x) is.character(x) & max(nchar(x)) > ST.STR.MAX))
    for (v in strVars) {
        dataframe[[v]] <- stataStr(dataframe[[v]])
    }

    ## Convert logical variables to integer
    logicalVars <- which(sapply(dataframe, is.logical))
    for (v in logicalVars) {
        dataframe[[v]] <- as.integer(dataframe[[v]])
    }

    ## Convert DateTime variables
    for (v in seq_along(dataframe)) {
        if (class(dataframe[[v]]) %in% DATETIME.CLASSES) {
            dataframe[[v]] <- asStataTime(dataframe[[v]])
        }
    }

    ## Variable Types
    ## numeric : by default converted to double, however, setting
    ## the option double=FALSE, will instead store them as float.
    ##
    ## integer : Stata has three kinds of integers: 'byte', 'int', and 'long'.
    ## I find the smallest type that will include the whole range.
    ##
    ## character : convert to the smallest str size that will include all
    ## the characters.
    if (is.null(typelist)) {
        typelist <- sapply(dataframe, function(x) {
            if (is.integer(x)) {
                if (all(is.na(x))) {
                    ret <- 'byte'
                } else {
                    xMin <- min(x, na.rm=TRUE)
                    xMax <- max(x, na.rm=TRUE)
                    if (xMin >= ST.BYTE.MIN & xMax <= ST.BYTE.MAX) {
                        ret <- "byte"
                    } else if (xMin >= ST.INT.MIN & xMax <= ST.INT.MAX) {
                        ret <- "int"
                    } else {
                        ret <- "long"
                    }
                }
            } else  if (is.numeric(x)) {
                if (double) {
                    ret <- "double"
                } else {
                    ret <- "float"
                }
            } else if (is.character(x)) {
                ## strings should already be truncated to correct size
                ret <- paste("str", max(nchar(x)), sep="")
            } else {
                stop(vartype, "not supported.")
            }
            ret
        })
        names(typelist) <- varlist
    }

    ## Variable Display Formats
    if (is.null(fmtlist)) {
        default.formats <- list(byte = "%8.0g",
                                int = "%8.0g",
                                long = "%12.0g",
                                float = "%9.0g",
                                double = "%10.0g")
        fmtlist <- sapply(typelist,
                             function(x) {
                                 ## For strings the default format appears to be
                                 ## %9s for str1-str9
                                 ## %[10-244]s for str10-str244
                                 if (substr(x, 1, 3) == "str") {
                                     dig <- as.integer(substr(x, 4, nchar(x)))
                                     if ( dig <= 9) {
                                         fmt <- "%9s"
                                     } else {
                                         fmt <- paste("%", dig, "s", sep="")
                                     }
                                 } else if (x %in% names(default.formats)) {
                                     fmt <- default.formats[[x]]
                                 } else {
                                     stop("cannot find a format for type", x)
                                 }
                             })
        ## Overwrite defaults for datetime variables
        fmtlist <- mapply(function(x, y) if (is.null(x)) y else x,
                          sapply(dataframe, function(x) attr(x, 'stata.time')),
                          fmtlist)
        names(fmtlist) <- varlist
    }

    ## Value Label List
    lbllist <- sapply(varlist, function(x) if (x %in% names(valueLabels)) x else "",
                      USE.NAMES=TRUE)

    ### Writing out xml
    z <- xmlTree()
    z$addTag("dta", close=FALSE)

    ## header
    z$addTag("header", close=FALSE)
    z$addTag("ds_format", dsFormat)
    z$addTag("byteorder", byteorder)
    z$addTag("nvar", nvar)
    z$addTag("nobs", nobs)
    z$addTag("data_label", datalabel)
    z$addTag("time_stamp", strftime(Sys.time(), ST.TIMESTAMP))
    z$closeTag()

    ## Descriptors Start
    z$addTag("descriptors", close=FALSE)

    ## Type List
    z$addTag("typelist", close=FALSE)
    for (i in seq_along(typelist)) {
        var <- varlist[i]
        z$addTag("type", typelist[i], attrs=c("varname"=var))
    }
    z$closeTag()

    ## Variable List
    z$addTag("varlist", close=FALSE)
    for (x in varlist) {
        z$addTag("variable", attrs=c("varname"=x))
    }
    z$closeTag() ## varlist

    ## sortlist
    z$addTag("srtlist", close=FALSE)
    for (x in sortlist) {
        z$addTag("sort", attrs=c('varname'=x))
    }
    z$closeTag() ## srtlist

    ## Format List
    z$addTag("fmtlist", close=FALSE)
    for (i in seq_along(fmtlist)) {
        var = varlist[i]
        z$addTag("fmt", fmtlist[i], attrs=c("varname"=var))
    }
    z$closeTag() ## fmtlist

    ## Value Label List
    z$addTag("lbllist", close=FALSE)
    for (i in seq_along(lbllist)) {
        var = varlist[i]
        z$addTag("lblname", lbllist[i], attrs=c("varname"=var))
    }
    z$closeTag() ## lbllist

    ##  Descriptors end
    z$closeTag() ## descriptors


    ## Variable Labels
    z$addTag("variable_labels", close=FALSE)
    for (i in seq_along(varlist)) {
        z$addTag("vlabel", variableLabels[i], attrs=c("varname"=varlist[i]))
    }
    z$closeTag()

    ## Expansion
    ## Right now only includes char (such as notes for the dataset)
    ## notes are implemented as char, where
    ## note0 = number of notes.
    ## notes[1-note0] = note value
    z$addTag("expansion", close=FALSE)
    for (vname in names(char)) {
        for (charname in names(char[[vname]])) {
            z$addTag("char", char[[vname]][[charname]],
                      attrs=c("name"=charname, "vname"=vname))
        }
    }
    z$closeTag()

    ## Data
    z$addTag("data", close=FALSE)
    for (i in seq_len(nobs)) {
        if (! verbose) {
            z$addTag('o', close=FALSE)
        } else {
            z$addTag('o', attrs=c("num"=i), close=FALSE)
        }
        for (j in seq_len(nvar)) {
            if (! verbose) {
                z$addTag('v', dataframe[i, j])
            } else {
                z$addTag('v', dataframe[i, j],  attrs=c("varname"=varlist[j]))
            }
        }
        z$closeTag()
    }
    z$closeTag()

    ## Value Labels
    z$addTag("value_labels", close=FALSE)
    nonNullLabels <- names(valueLabels)[ ! sapply(valueLabels, is.null) ]
    browser()
    for (vallab in nonNullLabels) {
        browser()
        z$addTag("vallab", attrs=c('name'=vallab), close=FALSE)
        for ( i  in seq_along(valueLabels[[vallab]])) {
            z$addTag('label', valueLabels[[vallab]][i],  attrs=c('value'=i))
        }
        z$closeTag() # vallab
    }
    z$closeTag() # value_labels

    ## End of dta file
    z$closeTag() #dta

    ## Prefix does not seem to work.
    saveXML(z, file=file, indent=verbose, prefix=ST.XMLHEADER)

}

