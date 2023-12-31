\name{pl.control}
\alias{pl.control}
\title{Arguments for plotting functions}
\description{Arguments that can be specified calling \code{plyx} and
  other 'pl' functions are checked and data is prepared for plotting.  
}
\usage{
pl.control(x=NULL, y=NULL, condvar = NULL, data = NULL, subset = NULL,
  transformed = TRUE, distinguishy = TRUE, gensequence = NULL,
  csize = NULL, csize.pch = NULL, 
  psize = NULL, plab = FALSE, pch = NULL, pcol = NULL,  
  smooth.weights = NULL, smooth.weight = NULL,
  markextremes = NULL, smooth = NULL,
  xlab = NULL, ylab = NULL, varlabel = NULL,
  vcol = NULL, vlty = NULL, vpch = NULL, plscale = NULL, log = NULL,
  main = NULL, sub = NULL, .subdefault = NULL, mar = NULL, 
  gencoord = TRUE,
  plargs = pl.envir, ploptions = NULL, .environment. = parent.frame(),
  assign = TRUE, ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y, data}{as in \code{\link{plyx}}
  }
  \item{condvar}{conditioning variables for \code{\link{plcond}}
  }
  \item{subset}{subset of data.frame 'data' to be used for plotting.
    See details.}
  \item{transformed}{logical: should transformed variables be used?
  }
  \item{distinguishy}{logical: should multiple y's be distinguished?
    This is \code{TRUE} if \code{pl.control} is called from
    \code{plyx}.
  }
  \item{gensequence}{logical: if only \code{x} or only \code{y} is set,
    should the other of these be specified as the sequence
    \code{1:nobs} (where \code{nobs} is the number of observations)?
  }
  \item{csize}{character expansion, applied to both labels and plotting
    characters.
  }
  \item{csize.pch}{expansion of plotting symbol relative to
    \code{par("pch")}.
    By default, it adjusts to the number of observations.
  }
  \item{psize, plab, pch, pcol}{Plotting characteristics of points,
    specified as a (unquoted) variable name found in \code{data}
    or as a vector.
    They set the size of the plotting symbols, labels (character
    strings), plotting character, and color, respectively.\cr
    \code{plabs = TRUE} asks for using the row names of \code{data}.
  }
  \item{smooth.weights, smooth.weight}{weights to be used in calculating
    smooth lines. Both are equivalent.
  }
  \item{markextremes}{scalar: proportion of extreme points to be labelled
  }
  \item{smooth}{logical: should a smooth line be added?
  }
  \item{xlab, ylab}{axis labels}
  \item{varlabel}{
    labels for variables replacing their names in the \code{x} and
    \code{y} arguments, either a simple vector of strings with an
    element for each variable,
    or a named vector, where names correspond to such variables.
  }
  \item{vcol, vlty, vpch}{color, line type and plotting character
    to be used when multiple y-s are plotted (in the sense of
    \code{matplot})
  }
  \item{plscale}{plot scale: name of the function to be used for
    generating a plotting scale, like \code{"log"}.
    A named character vector can be given, where the names correspond
    to variable names in \code{data}.
  }
  \item{log}{requires log scale as in R's basic plot function,
    e.g., equals either \code{"x"}, \code{"y"} or \code{"xy"}}
  \item{main, sub}{string. Main title of the plot(s).
    If \code{sub} starts by \code{":"} (the default),
    \code{pl.control} tries to generate an informative subtitle,
    determined by the data or a model formula.
  }
  \item{.subdefault}{for internal use: default of subtitle}
  \item{mar}{plot margins}
  \item{gencoord}{logical: should plotting coordinates be generated?
    This is avoided for low level pl graphics.
  }
  \item{plargs}{pl arguments, a list with components
    \code{ploptions}, see the following argument;
    \code{pldata}, the data used for plotting;
    \code{pmarpar}, graphical parameters defining margins.
  }
  \item{ploptions}{Plotting attributes, e.g., plotting character,
    line types, colors and the like, for different aspects of plots.
    Result of \code{\link{ploptions}}. Defaults to
    \code{pl.envir$ploptions}.
  }
  \item{.environment.}{used by the calling function to provide the
    environment for evaluating \code{x} and \code{y}
  }
  \item{assign}{logical: should the result of \code{pl.control} be
    assigned to the \code{pl.envir} environment?
    This will be done for high level pl functions, but avoided for low
    level ones. It allows for reusing the settings and helps debug
    unexpected behavior.
  }
  \item{\dots}{further arguments. These may include:
    \describe{
      \item{\code{psize, plab, pch, pcol, group, smooth.group,
	smooth.weights}: }{these specify graphical elements for each
	observation (row of \code{data}).
	the respective columns are added to the \code{pldata}
	data.frame.}
      \item{\code{...}: }{further \code{...} arguments will be passed on to
	\code{ploptions}. The respective settings will be used
	in the calling pl function, but not permanently stored
	in \code{ploptions} in the \code{pl.envir} environment.
      }
    }
  }
}
\details{
  The function selects the data according to the arguments
  \code{x, y, data} and \code{subset} (the latter by calling
  \code{\link{plsubset}}).
  The argument \code{subset} should be used instead of
  \code{data[subset,]} if the dataset \code{data} contains variable
  attributes like \code{varlabel, ticksat, ...}.
  The argument is evaluated in the dataset defined by \code{data},
  i.e., variable names may be used to define the subset.
}

\value{A list containing all the arguments, possibly in modified form.
  Specifically, the evaluations of the variables contained in
  \code{x} and \code{y} along with 
  \code{psize, plab, pch, pcol, smoothGroup, smoothWeights}
  are collected in the component \code{pldata}.
  The component, \code{ploptions}, collects the ploptions, and 
  \code{plfeatures} contains a list of additional features, both
  to be used in the calling high level pl function
}
\author{Werner A. Stahel
}
\seealso{
  \code{\link{plyx}}, \code{\link{plmatrix}}, \code{\link{ploptions}}
}
\examples{
plyx(Sepal.Width~Sepal.Length, data=iris, axp=7, plab=TRUE, csize.plab=0.6)
## same as
plargs <- pl.control(Sepal.Width~Sepal.Length, data=iris)
plargs$pdata$plab <- row.names(iris)
plargs$csize.lab <- 0.6
plargs$axp <- 7
plyx(Sepal.Width~Sepal.Length, plargs=plargs)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
