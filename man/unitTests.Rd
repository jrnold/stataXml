\name{unitTests}
\alias{unitTests.stataXml}
\title{ Unit tests for the package svUnit }
\description{ Performs unit tests defined in this
package by running \code{example(unitTests.svUnit)}.
Tests are in \code{runit*.R} files Located in the
'/unitTests' subdirectory or one of its
subdirectories ('/inst/unitTests' and subdirectories
in package sources).
}
\author{Jeffrey Arnold
(\email{jeffrey.arnold@gmail.com})}
\examples{
library(svUnit)
clearLog()

runTest(svSuite("package:stataXml")

errorLog()
}
\keyword{utilities}
