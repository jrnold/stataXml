\name{read.stataXml}
\alias{read.stataXml}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.stataXml(file, convert.dates = TRUE, convert.factors = TRUE, convert.underscore = FALSE, missing.type = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{convert.dates}{
%%     ~~Describe \code{convert.dates} here~~
}
  \item{convert.factors}{
%%     ~~Describe \code{convert.factors} here~~
}
  \item{convert.underscore}{
%%     ~~Describe \code{convert.underscore} here~~
}
  \item{missing.type}{
%%     ~~Describe \code{missing.type} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(file,
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
        for (i in grep('^\%t[cCdwmqh]', fmtlist)) {
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
