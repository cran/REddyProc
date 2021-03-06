\name{sEddyProc_sPlotDailySumsY}
\alias{sEddyProc_sPlotDailySumsY}
\title{sEddyProc$sPlotDailySumsY - Plot daily sum of specified year}
\description{The daily sums for a single year are plotted to the current device,
scaled to all data.
The daily sums are only calculated for days with complete data.
This function first computes the average flux for each day.
If the original unit is not "per day", then it need to be converted to
"per day" by argument \code{timeFactor}.
Furthermore, a change of the mass unit is provided by argument
\code{massFactor}.
The default parameters assume original units of mumol CO2 / m2 / second
and convert to gC / m2 / day.
The conversion factors allow plotting variables with different units}
\usage{sEddyProc_sPlotDailySumsY(Var = Var.s, VarUnc = "none", 
    Year = Year.i, timeFactor = if (!missing(timeFactor.n)) timeFactor.n else 3600 * 
        24, massFactor = if (!missing(massFactor.n)) massFactor.n else (44.0096/1e+06) * 
        (12.011/44.0096), unit = if (!missing(unit.s)) unit.s else "gC/m2/day", 
    data = cbind(sDATA, sTEMP), dts = sINFO$DTS, 
    Var.s, VarUnc.s, Year.i, timeFactor.n, 
    massFactor.n, unit.s)}
\arguments{
  \item{Var}{(Filled) variable to plot}
  \item{VarUnc}{Uncertainty
estimates for variable}
  \item{Year}{Year to plot}
  \item{timeFactor}{time
conversion factor with default per second to per day}
  \item{massFactor}{mass
conversion factor with default from mumol CO2 to g C}
  \item{unit}{unit
of the daily sums}
  \item{data}{data.frame with variables to plot}
  \item{dts}{numeric integer}
  \item{Var.s}{
}
  \item{VarUnc.s}{
}
  \item{Year.i}{
}
  \item{timeFactor.n}{
}
  \item{massFactor.n}{
}
  \item{unit.s}{
}
}



\author{AMM, KS
Department for Biogeochemical Integration at MPI-BGC, Jena, Germany <REddyProc-help@bgc-jena.mpg.de> [cph], Thomas Wutzler <twutz@bgc-jena.mpg.de> [aut, cre], Markus Reichstein <mreichstein@bgc-jena.mpg.de> [aut], Antje Maria Moffat <antje.moffat@bgc.mpg.de> [aut, trl], Olaf Menzer <omenzer@bgc-jena.mpg.de> [ctb], Mirco Migliavacca <mmiglia@bgc-jena.mpg.de> [aut], Kerstin Sickel <ksickel@bgc-jena.mpg.de> [ctb, trl], Ladislav Šigut <sigut.l@czechglobe.cz> [ctb]}





