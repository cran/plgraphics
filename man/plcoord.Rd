\name{plcoord}
\alias{plcoord}

\title{ Determines Values for Plotting with Limited "Inner" Plot Range }
\description{
  For plots with an "inner plot range" (see Details) this function
  converts the data values to the coordinates in the plot 
}
\usage{
plcoord(x, range = NULL, innerrange.factor = NULL,
  innerrange.ext = NULL, plext = NULL, ploptions = NULL)
}

\arguments{
  \item{x}{ data to be represented }
  \item{range}{ vector of 2 elements giving the inner plot range. Data
    beyond the given interval will be non-linearly transformed to fit
    within the (outer) plot margins. Defaults to
    \code{\link{robrange}(x, fac=fac)}.
  }
  \item{innerrange.factor}{ factor used to determine the default of
    \code{range} }
  \item{innerrange.ext}{ factor for extending the \code{range} to
    determine the outer plot range }
  \item{plext}{ vector of 1 or 2 elements setting the extension factor
    for the plotting range}
  \item{ploptions}{plotting options}
}
\details{
  When plotting data that contain outliers, the non-outlying data is
  represented poorly. Rather than simply clipping outliers, one can
  split the plotting area into an inner region, where the (non-outlying)
  data is plotted as usual, and a plot area margin, in which outliers
  are represented on a highly non-linear scale that allows to display
  them all.

  This function converts the data to the coordinates used in the
  graphical display, and also returns the inner and outer ranges for
  plotting.
}
\value{
  vector of coordinates used for plotting, that is, unchanged \code{x}
  values for   those within the \code{range} and transformed values
  for those outside.

  Attributes:
  \item{attr(,"plrange")}{the range to be used when plotting}
  \item{attr(,"range")}{the "inner" plot range, either the argument
  \code{range} or the values determined by default.}
  \item{attr(,"nouter")}{the number of modified observations}
}

\author{Werner A. Stahel}
\seealso{ \code{\link{robrange}} }
\examples{
  set.seed(0)
  x <- c(rnorm(20),rnorm(3,5,10))
  ( xmod <- plcoord(x) )

  plot(x,xmod)
## This shows what high level pl functions do by default
  plot(xmod)
  abline(h=attr(xmod,"innerrange"),lty=3, lwd=2)
## plgraphics
  plyx(x)  
}
\keyword{manip}
\keyword{aplot}
