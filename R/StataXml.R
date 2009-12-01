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

library(XML)

STATA.TIMESTAMP.FMT <- "%d %b %Y %H:%M"
STATA.FILETYPE <- 1
STATA.XMLHEADER <- '<?xml version="1.0" encoding="US-ASCII" standalone="yes"?>'
.stataVersions <- list("114"="10", "113"="8")

.convertUnderscores <- function(x) gsub(x, "_", ".")

.stataMissingType <- function(x) {
    ## if not a missing value then NA is returned
    ## if a missing value, then index of c(".", ..., ".z") returned.
    ## I subtract 1 to get 0-26, and the NAs remain NAs
    missings <- paste(".", c("", letters), sep="")
    ## I return a factor so that it is easier to interpret than 0-26
    factor(match(x, missings) - 1, labels=missings)
}

stataConvertFactors <- function(dataframe, convert.factors) {
    factors <- which(sapply(dataframe,is.factor))
    if(convert.factors == "string") {
        for(v in factors)
            dataframe[[v]] <- I(as.character(dataframe[[v]]))
    } else if (convert.factors == "numeric") {
        for(v in factors)
            dataframe[[v]] <- as.integer(as.character(dataframe[[v]]))
    } else if (convert.factors == "codes") {
        for (v in factors)
            dataframe[[v]] <- as.integer(dataframe[[v]])
    }

    shortlevels <- function(f) {
        ll <- levels(f)
        if (is.null(ll)) return(NULL)
        abbreviate(ll, 80L)
    }
    leveltable <- lapply(dataframe,shortlevels)

    attr(dataframe, "leveltable") <- leveltable
    dataframe
}

read.stataXml <- function(file,
                          convert.factors=TRUE,
                          convert.underscore=FALSE,
                          missing.type=FALSE)
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
    ## byteorder <- xmlValue(getNodeSet(doc, "//byteorder[1]")[[1]])

    ## nvar <- as.integer(xmlValue(getNodeSet(doc, "//nvar[1]")[[1]]))
    nobs <- as.integer(xmlValue(getNodeSet(doc, "//nobs[1]")[[1]]))
    datalabel <- xmlValue(getNodeSet(doc, "//data_label[1]")[[1]])
    timestamp <- strptime(xmlValue(getNodeSet(doc, "//time_stamp[1]")[[1]]),
                          STATA.TIMESTAMP.FORMAT)

    ## Descriptors
    descriptors <- getNodeSet(doc, "//descriptors[1]")[[1]]
    typelist <- xpathSApply(descriptors, "//type", xmlValue)
    ## variable list
    varlist <- xpathSApply(descriptors, "//variable", xmlGetAttr, name="varname")
    ## sort list
    srtlist <- xpathSApply(descriptors, "//sort", xmlValue)
    ## format list
    fmtlist <- xpathSApply(descriptors, "//fmt", xmlValue)
    ## value labels attached to each variable
    lbllist <- xpathSApply(descriptors, "//lblname", xmlValue)

    ## Variable Labels
    vlabels <- xpathSApply(doc, "//vlabel", xmlValue)

    ## Expansions
    ## currently only char
    char <- list()
    charNodes <- getNodeSet(getNodeSet(doc, "//expansion[1]")[[1]], "//char")
    evarnameList <- unique(sapply(charNodes, xmlGetAttr, name="vname"))
    char <- replicate(length(evarnameList), list())
    names(char) <- evarnameList
    for (node in charNodes) {
        evarname <- xmlGetAttr(node, name="vname")
        charname <- xmlGetAttr(node, name="name")
        char[[ c(evarname, charname) ]] <- xmlValue(node)
    }

    ## Value Labels
    vallabelNodes <- getNodeSet(doc, "//vallab")
    vallab <- vector(mode="list", length=length(labelnames))
    names(vallab) <- sapply(vallabelNodes, xmlGetAttr, name="name")
    for ( node in vallabelNodes) {
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
        missingValues <- vector(mode="list", length=length(varlist))
        names(missingValues) <- varlist
    }

    ## Parse Data
    res <- data.frame( row.names=as.character(1:nobs))
    for ( j in seq_along(varlist)) {
        x <- varlist[j]
        var <- xpathSApply(doc, "//o", function(obs, j) xmlValue(obs[[j]]), j=j)
        vartype <- colClasses[j]

        ## Missing values
        if (vartype != "character") {
            ## Store the types of missing values
            ## TODO: these should be sparse vectors
            if (missing.type) {
                missingValues[[ x ]] <- .stataMissingType(var)
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

    ## Post processing

    ## Convert underscores
    if (convert.underscore)  {
        names(res) <- .convertUnderscores(names(res))
    }

    ## Add attributes
    attr(res, "version") <- .stataVersions[[ ds_format ]]
    attr(res, "time.stamp") <- timestamp
    attr(res, "datalabel") <- datalabel
    attr(res, "formats") <- fmtlist
    attr(res, "types") <- typelist
    attr(res, "val.labels") <- lbllist
    attr(res, "var.labels") <- vlabels
    attr(res, "sort") <- srtlist
    attr(res, "char") <- char
    attr(res, "label.table") <- vallab
    if (missing.type) {
        attr(res, "missing") <- missingValues
    }

    ## Free up memory
    free(doc)

    ## Return
    res
}

## header
##     ds_format
##     byteorder
##     filetype
##     nvar
##     nobs
##     data_label
##     time_stamp
## descriptors
##     typelist
##        type[ varname] value
##     varlist
##        variable[varname]
##     srtlist
##        sort[varname]
##     fmtlist
##        varname[varname] format
##     lbllist
##        lblname[make]
## expansion
## data
##    o  {observation}
##      v {variable}
## labels

write.stataXml <- function(dataframe, file,
                           convert.factors="codes",
                           sortlist=NULL,
                           fmtlist=NULL,
                           typelist=NULL,
                           datalabel="Written by R.",
                           variableLabels=rep("", ncol(dataframe)),
                           char=list(),
                           verbose=FALSE)
{
    ## Header Info
    dsFormat <- 113
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
    ## convertfactors
    dataframe <- convertStataFactors(dataframe, convert.factors)

    ## Variable Types
    if (is.null(typelist)) {
        typelist <- sapply(dataframe, function(x) {
            vartype <- class(x)[1]
            if (vartype == "numeric") {
                ret <- "double"
            } else if (vartype == "integer") {
                ret <- "long"
            } else if (vartype == "character") {
                maxstr <- max(nchar(x))
                ret <- paste("str", maxstr, sep="")
            } else {
                stop(vartype, "not supported.")
            }
            ret
        })
    }

    ## Variable Display Formats
    if (is.null(formatlist)) {
        default.formats <- list(byte = "%8.0g",
                                int = "%8.0g",
                                long = "%8.0g",
                                float = "%9.0g",
                                double = "%10.0g")
        formatlist <- sapply(typelist,
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

    ## Sorting List
    ## From what I can tell, srtlist cannot be empty.
    ## If a dataset is saved without any sorted by characteristic
    ## stata seems to choose a random variable but NOT actually sort
    ## the dataset by it.
    ## I will not enforce that the dataset is actually sorted by the sortlist
    if (is.null(sortlist)) {
        sortlist <- varlist[1]
    }

    ### Writing out xml
    z <- xmlTree("dta")

    ## header
    z$addNode("header", close=FALSE)
    z$addNode("ds_format", dsFormat)
    z$addNode("byteorder", byteorder)
    z$addNode("nvar", nvar)
    z$addNode("nobs", nobs)
    z$addNode("data_label", datalabel)
    z$addNode("time_stamp", strftime(Sys.time(), STATA.TIMESTAMP.FMT))
    z$closeTag()

    ## Descriptors Start
    z$addNode("descriptors", close=FALSE)
    ## Type List
    z$addNode("typelist", close=FALSE)
    for (i in seq_along(typelist)) {
        z$addNode("type", typelist[i], attr=list(varname=varlist[i]))
    }
    z$closeTag()

    ## Variable List
    z$addNode("varlist", close=FALSE)
    for (x in varlist) {
        z$addNode("variable", attr=list(varname=x))
    }
    z$closeTag()

    ## sortlist
    z$addTag("srtlist", close=FALSE)
    for (x in sortlist) {
        z$addNode("sort", x)
    }
    z$closeTag()

    ## Format List
    z$addTag("fmtlist", close=FALSE)
    for (i in seq_along(formatlist)) {
        z$addNode("fmt", formatlist[i], attr=list(varname=varlist[i]))
    }
    z$closeTag()

    ## End Descriptors
    z$closeTag()

    ## Variable Labels
    z$addNode("variable_labels")
    for (i in seq_along(varlist)) {
        z$addNode("vlabel", variableLabels[i], attr=list(varname=varlist[i]))
    }
    z$closeTag()

    ## Expansion
    ## Right now only includes char (such as notes for the dataset)
    ## notes are implemented as char, where
    ## note0 = number of notes.
    ## notes[1-note0] = note value
    z$addNode("expansion", close=FALSE)
    for (vname in names(char)) {
        for (charname in names(char[[vname]])) {
            z$addNode("char", char[[vname]][[charname]],
                      attr=list(name=charname, vname=vname))
        }
    }
    ## Add char here
    z$closeTag()

    ## Data
    z$addNode("data", close=FALSE)
    for (i in seq_len(nobs)) {
        if (! verbose) {
            z$addNode('o', close=FALSE)
        } else {
            z$addNode('o', attr=list(num=i), close=FALSE)
        }
        for (j in seq_len(nvar)) {
            if (! verbose) {
                z$addNode('v', dataframe[i, j])
            } else {
                z$addNode('v', dataframe[i, j],  attr=list(varname=varlist[j]))
            }
        }
        z$closeTag()
    }
    z$closeTag()

    ## TODO value_label table

    saveXML(z, file=file)

}

