\name{read.stataXml}
\alias{read.stataXml}

\title{
  Read Stata XML files
}

\description{
  Reads a file in Stata's XML format into a data frame.
}
\usage{
read.stataXml(file, convert.dates = TRUE, convert.factors = TRUE,
convert.underscore = FALSE, missing.type = TRUE)
}

\arguments{
  \item{file}{
    character. a filename.
  }
  \item{convert.dates}{
    logical. Convert Stata dates to 'Date' or 'POSIXct' class? (see description)
  }
  \item{convert.underscore}{
    logical. Convert '"_"' in Stata variable names to '"."' in R names?
  }
  \item{missing.type}{
    logical. Store information about different types of missing data?
  }
}

\details{

  The variables in the Stata data set become the columns of the
  data frame.  Missing values are correctly handled.  The data label,
  variable labels, timestamps, and variable characteristics are stored
  as attributes of the data frame.
  
  By default, Stata dates and times are converted to R's \bold{Date}
  or \bold{POSIXct} classes using \code{\link{fromStataTime}}.
  Variables with Stata value labels are converted to factors.

  % ‘Date’ class and variables with Stata value labels are converted
  % to factors.  Ordinarily, ‘read.dta’ will not convert a variable to
  % a factor unless a label is present for every level.  Use
  % ‘convert.factors = NA’ to override this.  In any case the value
  % label and format information is stored as attributes on the
  % returned data frame.
  
  Stata 8.0 introduced a system of 27 different missing data values.
  If \bold{missing.type} is \bold{TRUE} a separate list is created with the
  same variable names as the loaded data.  For string variables the
  list value is \bold{NULL}. For other variables the list value is a
  vector with the length of the number of \bold{NA}'s in that variable.
  The first element of this vector is the type of missing value
  corresponding to the first \bold{NA} in the variable, and so on.
  The vector is a factor with 27 levels: ".", ".a", ..., ".z".
  This is attached as the ‘"missing"’ attribute of the
  returned value.

}

\value{
  A data frame with the following attributes:
    \itemize{
      \item{version}{character. With XML files, always '113'.}
      \item{time.stamp}{POSIXct.}
      \item{datalabel}{character.}
      \item{formats}{character}
      \item{types}{character. Stata data types.}
      \item{val.labels}{character. value labels associated with each
	variable.}
      \item{var.labels}{character. variable labels.}
      \item{sort}{character. Variables by which the dataset was sorted.}
      \item{char}{list. Data and variable characteristics, including
	notes. See \url{http://www.stata.com/help.cgi?char}.}
      \item{label.table}{list. Stata value labels}.
      \item{dta_type}{Always equal to "xml". This is used to distinguish
	data frames returned by \bold{read.stataXml} from those returned by \bold{read.dta}.}
      \item{missing}{list with the types of missing values for each variable.}
    }
  
  The data frame returned from \bold{read.stataXml} is slightly different from the
  data frame returned by \bold{read.dta}. The major differences are,

  \itemize{
    \item{missing values are returned as factors, and in vectors of length equal
      to the number of \bold{NA}'s in each variable.}
    \item{variable characteristics and the sort list are returned.}
    \item{all of Stata's date and time variables are converted to R date/time
      classes, not just "%d" and "%td".}
  }

}

\references{
  Stata help for \bold{xmlsave}. Online at
  \url{http://www.stata.com/help.cgi?xmlsave}.
  The XML format used by stata has all the components of the binary
  Stata data format described in \url{http://www.stata.com/help.cgi?dta}.
}

\author{
  Jeffrey Arnold
}

\seealso{
  \code{\link{read.stataXml}}, \code{\link{fromStataDate}}, \code{\link{write.dta}}, \code{\link{read.dta}}
}
\examples{

}

\keyword{manip}
\keyword{data}
