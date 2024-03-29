\name{fLoadTXTIntoDataframe}
\alias{fLoadTXTIntoDataframe}
\title{Load text file with one header and one unit row into data frame}
\description{If gaps with the flag -9999.0 exist, these are set to NA.}
\usage{fLoadTXTIntoDataframe(FileName = FileName.s, 
    Dir = if (!missing(Dir.s)) Dir.s else "", 
    FileName.s, Dir.s = "")}
\arguments{
  \item{FileName}{File name as a character string}
  \item{Dir}{Directory as a character string}
  \item{FileName.s}{deprecated}
  \item{Dir.s}{deprecated way of specifying Dir}
}
\details{Function fLoadFluxNCIntoDataframe, which loads data from NetCDF-Files, has
been moved to add-on package REddyProcNCDF.
In addition, \code{\link{fLoadEuroFlux16}} loads data from several annual
files in format corresponding to Europe-fluxdata 2016.

For using only part of the records, use \code{fFilterAttr} to keep
units attributes.}
\value{Data frame with data from text file.}

\author{Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav <U+0160>igut <sigut.l@czechglobe.cz> [ctb]}



\seealso{\code{\link{help_export}}}
\examples{\donttest{
examplePath <- getExamplePath('Example_DETha98.txt', TRUE)
EddyData.F <- fLoadTXTIntoDataframe(examplePath)
}}
