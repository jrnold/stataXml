## TODO:
## ensure strings, names, factor names are the correct size.
## convert dates
## What a stata file consists of
## header
## descriptors ( variable names, formats, types, sortlist)
## variable_labels
## data
## value_label table
## My goal: Do everything in R, except the writing.
## writing can go to either xml or binary.
## TODO: use make.names and replace . with _ for data.frame names
library(XML)

## For non variable names, I just need to truncate.
strtrunc <- function(x, n) {
    tooLong <- (nchar(x) > n)
    x[ tooLong ] <- substr(x[ tooLong ], 1, n)
}

## Ensure valid stata names
stataVarname <- function(x) {
    gsub("\\.", "_", abbreviate(make.names(x), 32L))
}
stataValLabelName <- function(...) stataVarname(...)

stataVarLabel <- function(x) strtrunc(x, 80L)
stataDataLabel <- function(...) stataVarLabel(...)

stataStr <- function(x) strtrunc(x, 244L)
stataValLabelStr <- function(x) strtrunc(x, 32000L)
stataCharStr <- function(x) strtrunc(x, 67748L)

## Origin date for Stata Date Variables
STATA.EPOCH <- as.Date("1960-01-01")

## From stata 10
## help limits
STATA.LIMITS <- list(## dataset sizes
                     nobs = as.integer(c(small=1000, IC=2147483647, SE=2147483647)),
                     varnum = as.integer(c(small=99, IC=2047, SE=32767)),  # number of variables
                     datawidth= as.integer(c(small=200, IC=24564, SE=393192)),  # width of a dataset
                     ## labels
                     labeldata = 80L,  # dataset label
                     labelvar = 80L,   # variable label
                     labelvalstr = 32000L, # length of value label string
                     labelvalname = 32L, # length of name of value label
                     labelvalcodings = as.integer(c(small=1000, IC=65536, SE=65536)),
                                        # number of codings within one value label
                     ## Misc
                     strvar = 244L,    # length of string variable
                     varname = 32L,    # Length of variable name
                     char = 67784L    # length of one characteristic
                     )

## help data_types
STATA.DATATYPES <- list(## number of bytes, min, max
                        byte=as.integer(c(1, -127, 100)),
                        int=as.integer(c(2, -32767, 32740)),
                        long=as.integer(c(4, -2147483647, 2147483620)),
                        ## bytes, min, max, 10^-x (negative power of ten for closes to 0 without being 0)
                        float=c(4, -1.70141173319e+38, 1.70141173319e+38, 38),
                        double=c(8, -8.9884656743e+307, 8.9884656743e+307, 323),
                        ## min, max number of bytes
                        str=c(1L, 244L))

STATA.TIMESTAMP.FMT <- "%d %b %Y %H:%M"
STATA.FILETYPE <- 1
STATA.XMLHEADER <- '<?xml version="1.0" encoding="US-ASCII" standalone="yes"?>\n'
.stataVersions <- c("114"="10", "113"="8")

.convertUnderscores <- function(x) gsub(x, "_", ".")

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
                          convert.factors=TRUE,
                          convert.underscore=FALSE,
                          missing.type=TRUE)
{
    doc <- xmlParse(file)

    stataTypeToRClasses <- list(byte="integer",
                            int="integer",
                            long="integer",
                            float="numeric",
                            double="numeric",
                            ## all str1-str244
                            str="character")
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
                          STATA.TIMESTAMP.FMT)

    ## Descriptors
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
    colClasses <- typelist
    colClasses[ grep("^str", typelist) ] <- "str"
    colClasses <- sapply(colClasses, function(x) stataTypeToRClasses[[x]])

    ## if types of missing values are stored initialize the list.
    if (missing.type) {
        missingValues <- list()
    }

    ## Parse Data
    res <- data.frame( row.names=as.character(1:nobs))
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
            res[[x]] <- var
        }
    }
    rm(var)

    ## Convert underscores
    if (convert.underscore)  {
        names(res) <- .convertUnderscores(names(res))
    }

    ## Add attributes
    attr(res, "version") <- ds_format
    attr(res, "time.stamp") <- timestamp
    attr(res, "datalabel") <- datalabel
    attr(res, "formats") <- fmtlist
    attr(res, "types") <- typelist
    attr(res, "val.labels") <- lbllist
    attr(res, "var.labels") <- vlabels
    attr(res, "sort") <- srtlist
    attr(res, "char") <- char
    attr(res, "label.table") <- vallab
    attr(res, "dta_type") <- "xml"
    if (missing.type) {
        attr(res, "missing") <- missingValues
    }

    ## Free up memory
    free(doc)

    ## Return new dataframe
    res
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
    filetye <- STATA.FILETYPE

    ## Variables and Observations
    nvar <- ncol(dataframe)
    nobs <- nrow(dataframe)

    ## list of variable names
    varlist <- names(dataframe)

    ## Cleaning Data Frame

    ## Factors and Generating a Value Label List if any
    valueLabels <- list()

    factors <- which(sapply(dataframe, is.factor))
    for(v in factors) {
        if(convert.factors == "string") {
            dataframe[[v]] <- as.character(dataframe[[v]])
        } else if (convert.factors == "numeric") {
            dataframe[[v]] <- as.integer(as.character(dataframe[[v]]))
        } else if (convert.factors == "codes") {
            dataframe[[v]] <- as.integer(dataframe[[v]])
        } else {
            ## We keep the factors and treat them as values.

            lblname <- names(dataframe)[v]
            valueLabels[[lblname]] <- levels(dataframe[[v]])

            dataframe[[v]] <- as.integer(dataframe[[v]])
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
            vartype <- class(x)[1]
            if (vartype == "numeric") {
                if (double) {
                    ret <- "double"
                } else {
                    ret <- "float"
                }
            } else if (vartype == "integer") {
                xMin <- min(x, na.rm=TRUE)
                xMax <- max(x, na.rm=TRUE)
                if (xMin >= STATA.DATATYPES[["byte"]][2] &
                    xMax <= STATA.DATATYPES[["byte"]][3])
                {
                    ret <- "byte"
                } else if (xMin >= STATA.DATATYPES[["int"]][2] &
                           xMax <= STATA.DATATYPES[["int"]][3])
                {
                    ret <- "int"
                } else {
                    ret <- "long"
                }
            } else if (vartype == "character") {
                maxstr <- min(max(nchar(x)), STATA.DATATYPES[["str"]][2])
                ret <- paste("str", maxstr, sep="")
            } else {
                stop(vartype, "not supported.")
            }
            ret
        })
    }

    ## Variable Display Formats
    if (is.null(fmtlist)) {
        default.formats <- list(byte = "%8.0g",
                                int = "%8.0g",
                                long = "%8.0g",
                                float = "%9.0g",
                                double = "%10.0g")
        fmtlist <- sapply(typelist,
                             function(x) {
                                 ## For strings the default format appears to be
                                 ## %9s for str1-str9
                                 ## %[10-244]s for str10-str244
                                 if (substr(x, 1, 3) == "str") {
                                     dig <- substr(x, 4, nchar(x))
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
    }

    ## Ensuring correct Lengths

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
    z$addTag("time_stamp", strftime(Sys.time(), STATA.TIMESTAMP.FMT))
    z$closeTag()

    ## Descriptors Start
    z$addTag("descriptors", close=FALSE)
    ## Type List
    z$addTag("typelist", close=FALSE)
    for (i in seq_along(typelist)) {
        z$addTag("type", typelist[i], attrs=c("varname"=varlist[i]))
    }
    z$closeTag()

    ## Variable List
    z$addTag("varlist", close=FALSE)
    for (x in varlist) {
        z$addTag("variable", attrs=c("varname"=x))
    }
    z$closeTag()

    ## sortlist
    z$addTag("srtlist", close=FALSE)
    for (x in sortlist) {
        z$addTag("sort", attrs=c('varname'=x))
    }
    z$closeTag()

    ## Format List
    z$addTag("fmtlist", close=FALSE)
    for (i in seq_along(fmtlist)) {
        var = varlist[i]
        z$addTag("fmt", fmtlist[i], attrs=c("varname"=var))
    }
    z$closeTag()

    ## End Descriptors
    z$closeTag()

    ## Variable Labels
    z$addTag("variable_labels")
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

    z$addTag("value_labels", close=FALSE)
    for (vallab in names(valueLabels)) {
        z$addTag("vallab", attrs=c('name'=vallab), close=FALSE)
        for ( i  in seq_along(valueLabels[[vallab]])) {
            z$addTag('label', valueLabels[[vallab]][i],  attrs=c('value'=i))
        }
        z$closeTag() # vallab
    }
    z$closeTag() # value_labels

    z$closeTag() #dta

    ## Prefix does not seem to work.
    saveXML(z, file=file, indent=verbose, prefix=STATA.XMLHEADER)

}

