\name{write.stataXml}
\alias{write.stataXml}

\title{
Write Files in Stata XML Format
}
\description{
     Writes the data frame to file in the Stata XML format.  
}
\usage{
write.stataXml(dataframe, file, convert.factors = "labels", sortlist = character(), fmtlist = NULL, typelist = NULL, datalabel = "Written by R.", variableLabels = rep("", ncol(dataframe)), char = list(), verbose = FALSE, double = TRUE)
}

\arguments{
  \item{dataframe}{
    data frame.
  }
  \item{file}{
    character: filename.
  }
  \item{convert.factors}{
    character. how to handle factors.
  }
  \item{sortlist}{
    character. variables the data frame is sorted by.
  }
  \item{fmtlist}{
    character. formats for each variable.
  }
  \item{typelist}{
    character. Stata data type of each variable.
  }
  \item{datalabel}{
    character. Data set label.
  }
  \item{variableLabels}{
    character. Variable labels.
  }
  \item{char}{
    list of character vectors. Variable characteristics to be used by
    Stata, including notes.
  }
  \item{verbose}{
    logical. Write xml in a more human readable form. Equivalent to the
    \bold{legible} option in the Stata command \bold{xmluse}.
  }
  \item{double}{
    logical. Convert numerical vectors to double, else convert to float.
  }
}
\details{
  
  The Stata XML format was introduced in Stata 9 as an alternative to
  the binary .dta format to store datasets. This format can be written
  by the Stata command \bold{xmlsave, doctype(dta)}, and read by the
  Stata command \bold{xmluse}.  A file in Stata xml format contains the same
  information contained in the Stata binary .dta files.
  
  The columns in the data frame become variables in the Stata data
  set.  Missing values are correctly handled.
  
  Unless deselected, R date and data-time objects will be converted
  into the Stata format using \link{asStataTime}.  
  
  There are four options for handling factors (the same as in
  \bold{write.dta}.)  The default is to use Stata ‘value labels’ for
  the factor levels. With \bold{convert.factors="string"}, the factor
  levels are written as strings.  With \bold{convert.factors="numeric"}
  the numeric values of the levels are written, or \bold{NA} if they
  cannot be coerced to numeric.  Finally, \bold{convert.factors="codes"}
  writes the underlying integer codes of the factors.  This last used
  to be the only available method and is provided largely for
  backwards compatibility.

  % Note that the Stata formats are documented to be use ASCII strings
  % - R does not enforce this, but use of non-ASCII character strings
  % will not be portable as the encoding is not recorded.  Up to 244
  % bytes are allowed in character data, and longer strings will be
  % truncated with a warning.
  
  % Stata uses some large numerical values to represent missing
  % values.  This function does not currently check, and hence
  % integers greater than ‘2147483620’ and doubles greater than
  % ‘8.988e+307’ may be misinterpreted by Stata.
}
\value{
  \bold{NULL}
}
\references{
  Stata help for \bold{xmlsave}. Online at
  \url{http://www.stata.com/help.cgi?xmlsave}.
  The XML format used by Stata has all the components of the binary
  Stata data format described in \url{http://www.stata.com/help.cgi?dta}.
}
\author{
  Jeffrey Arnold
}

\seealso{
  \code{\link{read.stataXml}}, \code{\link{asStataDate}}, \code{\link{write.dta}}, \code{\link{read.dta}}
}
\examples{
}

\keyword{manip}
\keyword{data}
