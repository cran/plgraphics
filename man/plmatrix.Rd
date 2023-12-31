\name{plmatrix}
\alias{plmatrix}

\title{Scatterplot Matrix}
\description{
  Plots a scatterplot matrix, for which the variables shown horizontally
  do not necessarily coincide with those shown vertically. If desired,
  the matrix is divided into several blocks such that it fills more than
  1 plot page.
}
\usage{
plmatrix(x, y = NULL, data = NULL, panel = NULL, 
  nrow = NULL, ncol = nrow, reduce = TRUE, 
  xaxmar=NULL, yaxmar=NULL, xlabmar=NULL, ylabmar=NULL,
  xlab=NULL, ylab=NULL, mar=NULL, oma=NULL, diaglabel.csize = NULL,
  plargs = NULL, ploptions = NULL, assign = TRUE, ...) 
}

\arguments{
  \item{x}{data for columns (x axis), or formula defining column
    variables. If it is a formula containing a left hand side,
    the left side variables will be used last.}
  \item{y}{data or formula for rows (y axis). Defaults to \code{x}}
  \item{data}{data.frame containing the variables in case \code{x}
    or \code{y} is a formula}
  \item{panel}{a function that generates the marks of the individual
    panels, see Details.
  }
  \item{nrow, ncol}{maximum number of rows and columns of panels on a page}
  \item{reduce}{if y is not provided and \code{reduce==TRUE},
    the first row and the last column are suppressed.}
%%   \item{keeppar}{logical. If TRUE, the graphical settings
%%     "mfrow", "mar", "cex", and "mgp" will be maintained, otherwise,
%%     the old values will be restored (default).}
  \item{xaxmar, yaxmar}{margin in which the axis (tick marks and
    corresponding labels) should be shown: either 1 or 3 for
    \code{xaxmar} and 2 or 4 for \code{yaxmar}.
  }
  \item{xlabmar, ylabmar}{in which margin should the x- [y-] axis be
    labelled?}
  \item{xlab, ylab}{not used (introduced to avoid confusion with
    \code{xlabmar, ylabmar})}
  \item{mar, oma}{width of margins, see \code{\link{par}}}
  \item{diaglabel.csize}{Character expansion for labels appearing in the
    "diagonal" of the scatterplot matrix (if present)}
  \item{plargs}{result of calling \code{pl.control}.
    If \code{NULL}, \code{pl.control} will be called to generate it.
    If not null, arguments given in \code{...} will be ignored.
  }
  \item{ploptions}{list of pl options.
  }
  \item{assign}{logical: Should the plargs be stored in the \code{pl.envir}
    environment?
  }
  \item{\dots}{further arguments passed to the \code{panel} function
    and possibly further to functions called by the panel function\cr}
}
\details{
  The \code{panel} function can be user written. It needs \eqn{>=5}
  arguments which must correspond to the arguments of
  \code{\link{plpanel}}: \code{x, y, indx, indy, plargs}.
  If some arguments are not used, just introduce them as arguments
  to the function anyway in order to avoid (unnecessary) error messages
  and stops.
  \cr
  Since large scatterplot matrices lead to tiny panels, \code{plmatrix}
  splits the matrix into blocks of at most \code{nrow} rows and
  \code{ncol} columns. If these numbers are missing, they default to
  \code{nrow=5} and \code{ncol=6} for landscape pages, and to
  \code{nrow=8} and \code{ncol=5} for portrait pages.

  The \code{panel} argument defaults to \code{plpanel}, which results
  essentially in \code{\link{points}} or \code{\link{text}}
  depending on the argument \code{pch}, including a smooth line,
  to \code{\link{plmboxes}} if  'x'  is a factor and  'y'  is not or
  vice versa,
  or to a modification of \code{sunflowers} if both are factors.\cr
  The function must have the arguments \code{x} and \code{y}
  to take the coordinates of the points and may have the arguments
  \code{indx} and \code{indy} to transfer the variables\' index.
  If there is an argument \code{plargs}, the current value of
  \code{plargs} will be passed on. It is a list and can be extended
  to pass any additional items to the function.
}
\value{
  none
}
\author{Werner A. Stahel, ETH Zurich}
\seealso{\code{\link{pairs}}, \code{\link{plyx}}
}
\note{There are many more arguments, obtained from \code{pl.control},
  see \code{?\link{pl.control}}. These can be passed to \code{plmatrix}
  by an argument \code{plargs} that is hidden in the ... argument list.
}
\examples{
plmatrix(iris, pch=as.numeric(Species))
plmatrix(~Sepal.Length+Sepal.Width, ~Petal.Length+Petal.Width,
    data=iris, smooth=TRUE, plab=substr(Species,1,2))
}
\keyword{hplot}
