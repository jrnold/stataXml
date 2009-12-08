\name{write.stataXml}
\alias{write.stataXml}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
write.stataXml(dataframe, file, convert.factors = "labels", sortlist = character(), fmtlist = NULL, typelist = NULL, datalabel = "Written by R.", variableLabels = rep("", ncol(dataframe)), char = list(), verbose = FALSE, double = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
%%     ~~Describe \code{dataframe} here~~
}
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{convert.factors}{
%%     ~~Describe \code{convert.factors} here~~
}
  \item{sortlist}{
%%     ~~Describe \code{sortlist} here~~
}
  \item{fmtlist}{
%%     ~~Describe \code{fmtlist} here~~
}
  \item{typelist}{
%%     ~~Describe \code{typelist} here~~
}
  \item{datalabel}{
%%     ~~Describe \code{datalabel} here~~
}
  \item{variableLabels}{
%%     ~~Describe \code{variableLabels} here~~
}
  \item{char}{
%%     ~~Describe \code{char} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{double}{
%%     ~~Describe \code{double} here~~
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
function(dataframe, file,
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
    if (! convert.factors \%in\% c('string', 'numeric', 'codes')) {
        ## List with an entry for every variable
        ## non-factor variables will have NULL as their element
        valueLabels <- lapply(dataframe,
                              function(x) {
                                  stataValLabelStr(levels(x))
                              })
        ## Keep only defined values
        valueLabels <- valueLabels[ sapply(valueLabels, function(x) length(x) > 0) ]
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

    ## Convert DateTime variables
    for ( i in seq_along(dataframe)) {
        if (class(dataframe[[i]]) \%in\% DATETIME.CLASSES) {
            dataframe[[i]] <- asStataTime(dataframe[[i]])
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
            if (is.logical(x)) {
                ret <- "byte"
            } else if (is.integer(x)) {
                xMin <- min(x, na.rm=TRUE)
                xMax <- max(x, na.rm=TRUE)
                if (xMin >= ST.BYTE.MIN & xMax <= ST.BYTE.MAX) {
                    ret <- "byte"
                } else if (xMin >= ST.INT.MIN & xMax <= ST.INT.MAX) {
                    ret <- "int"
                } else {
                    ret <- "long"
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
        default.formats <- list(byte = "\%8.0g",
                                int = "\%8.0g",
                                long = "\%12.0g",
                                float = "\%9.0g",
                                double = "\%10.0g")
        fmtlist <- sapply(typelist,
                             function(x) {
                                 ## For strings the default format appears to be
                                 ## \%9s for str1-str9
                                 ## \%[10-244]s for str10-str244
                                 if (substr(x, 1, 3) == "str") {
                                     dig <- as.integer(substr(x, 4, nchar(x)))
                                     if ( dig <= 9) {
                                         fmt <- "\%9s"
                                     } else {
                                         fmt <- paste("\%", dig, "s", sep="")
                                     }
                                 } else if (x \%in\% names(default.formats)) {
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
    lbllist <- sapply(varlist, function(x) if (x \%in\% names(valueLabels)) x else "",
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
    for (vallab in nonNullLabels) {
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
