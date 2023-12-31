\name{plyx}
\alias{plyx}

\title{Scatterplot, enhanced
}
\description{A scatterplot or a bunch of them is produced according to
  the concept of the  plplot  package
}
\usage{
plyx(x = NULL, y = NULL, by=NULL, group = NULL, data = NULL, type = "p",
  panel = NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL,
  markextremes = 0, rescale = TRUE, mar = NULL, mf = FALSE,
  plargs = NULL, ploptions = NULL, assign = TRUE, ...)
}

\arguments{
  \item{x}{either a formula or the data to be used for the horizontal
    axis. If a formula of the type 'y~x', the variable 'y' in
    'data' will be plotted against the variable(s) 'x'.
    If a data.frame with more than one column is given,
    each column will be used in turn to produce a plot.
  }
  \item{y}{data to be used as the y axis.
  }
  \item{by}{grouping factor: for each \code{by} group, a plot will be shown
    for the respective subset of the data}
  \item{group}{grouping that determines plotting symbols, colors, and
    line types}
  \item{data}{data.frame containing the variables if 'x' is a formula}
  \item{xlab, ylab}{axis labels}
  \item{xlim, ylim}{plot ranges}
  \item{type}{type of plot, see \code{?\link{plot.default}}}
  \item{panel}{panel function to do the actual drawing. See Details.
  }
  \item{markextremes}{proportion of extreme residuals to be labeled.
    If all points should be labeled, let \code{markextremes=1}.}
  \item{rescale}{logical. Only applies if there are multiple y
    variables. If \code{TRUE}, the vertical axis will be adjusted
    for each of these variables.
  }
  \item{mar}{plot margins, see \code{\link{par}}
  }
  \item{mf}{number of multiple frames. If more than one plot will be
    generated because of a grouping or multiple x variables,
    multiple frames will be produced by calling \code{\link{plmframes}}
    unless \code{mf} is \code{FALSE}.
    If \code{mf} is \code{TRUE}, the function will determine the number
    of rows and columns suitably. If \code{mf} is a vector of length 2,
    these numbers will be used for the number of panels in rows and
    columns (unless they are too large for the restriction in
    \code{ploptions("mframesmax")}).
    If it has lenngth 1, this is used as the total number of
    panels on a page.
  }
  \item{plargs}{result of calling \code{pl.control}.
    If \code{NULL}, \code{pl.control} will be called to generate it.
    If not null, arguments given in \code{...} will be ignored.
  }
  \item{ploptions}{list of pl options.
  }
  \item{assign}{logical: Should the plargs be stored
    in the \code{pl.envir} environment?
  }
  \item{\dots}{ more arguments, to be passed to \code{\link{pl.control}}
  }
}
\details{
    \code{panel} defaults to \code{plpanel}, which results essentially in
    \code{\link{points}} or \code{\link{text}}
    depending on the argument \code{pch} including a smooth line,
    to \code{\link{plmboxes}} if  'x'  is a factor and  'y'  is not or
    vice versa,
    or to a modification of \code{sunflowers} if both are factors.\cr
    The function must have the arguments \code{x} and \code{y}
    to take the coordinates of the points and may have the arguments
    \code{indx} and \code{indy} to transfer the two variables' indexes and
    \code{panelargs} for any additional objects to be passed on.
}
\value{None.}

\note{There are many more arguments, obtained from \code{pl.control},
  see \code{?\link{pl.control}}. These can be passed to \code{plmatrix}
  by an argument \code{plargs} that is hidden in the ... argument list.
}

\author{Werner A. Stahel, ETH Zurich}

\seealso{
  \code{\link{plmatrix}}, \code{\link{plcond}};
  \code{\link{pl.control}}, \code{\link{ploptions}}
}
\examples{
plyx(Petal.Width ~ Sepal.Length, data=iris)
plyx(Petal.Width ~ Sepal.Length+Sepal.Width, data=iris, smooth=TRUE,
     group=Species)
plyx(Petal.Length + Petal.Width ~ Sepal.Length+Sepal.Width,
     by  = Species, data=iris, smooth=TRUE)

}
\keyword{hplot}

