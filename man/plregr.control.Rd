\name{plregr.control}
\alias{plregr.control}
\title{
Further Arguments to \code{plregr}
}
\description{
  Specify some arguments of minor importance for the function
  \code{\link{plregr}}
}
\usage{
plregr.control(x, data = NULL, xvar = TRUE, transformed = FALSE,
  weights = NULL, stdresid = TRUE, mar = NULL,
  glm.restype = "working", condquant = TRUE, smresid = TRUE,
  partial.resid = NULL, addcomp = NULL, cookdistlines = NULL,
  leveragelimit = NULL, condprob.range = NULL,
  testlevel = 0.05,
  refline = TRUE, 
  smooth = 2, 
  smooth.sim = NULL,
  xlabs = NULL, reslabs = NULL, markextremes = NULL,
  mf = TRUE, mfcol = FALSE, multnrow = 0, multncol = 0, marmult = NULL,
  oma = NULL, assign = TRUE, ...)
} 
\arguments{
  \item{x}{an object (result of a call to a model fitting function
    such as \code{lm, glm, ...}. This is the only argument
    that is needed. All others have useful defaults.}
  \item{data}{see \code{?\link{plregr}}}
  \item{xvar}{variables for which residuals shall be plotted.
    Either a formula like \code{~ x1 + x2} or a character vector of
    names. Defaults to all variables (or terms, see \code{transformed})
    in the model.
  }    
  \item{transformed}{see \code{?\link{plregr}}}
  \item{weights}{logical: should residuals be plotted against weights?
    Used in \code{plresx}.}
  \item{stdresid}{logical: should leverages and standardized residuals
    be calculated? This is avoided for \code{plresx}}
  \item{mar}{plot margins
    }
  \item{glm.restype}{type of residuals to be used for glm models.
    In addition to those allowed in \code{residuals()} for
    \code{glm} objects, type \code{condquant} is possible for
    (ungrouped) binary regression.
    See \code{?\link{residuals.regrpolr}} for an explanation.
    Warning: type "deviance" will not work with simulated smooths since
    NAs will emerge.}
  \item{condquant}{logical: should conditional quantiles be shown for
    censored observations, binary and ordered responses?}
  \item{smresid}{logical: Should residuals from smooth be used for
    'tascale' and 'qq' plots?}
  \item{partial.resid, addcomp}{logical, synonyms: Should component
    effects be added to the residuals? This leads to what some authors
    call "partial residual plot".}
  \item{cookdistlines}{levels of Cook distance for which contours are
    plotted in the leverage plot}
  \item{leveragelimit}{bound for leverages to be used in standardizing
    residuals and in calculation of standardized residuals from smooth
    (if \code{smresid} is \code{TRUE}).}
  \item{condprob.range}{numeric vector of length 2.
    In the case of residuals of class \code{condquant}, quartile bars
    are only drawn for residuals with probability between
    \code{condprob.range[1]} and \code{condprob.range[1]}.
    Default is \code{c(0.05,0.8)} for less than 50 observations, and
    \code{c(0,0)}, suppressing the bars, otherwise.}
  \item{testlevel}{level for statistical tests}
  \item{refline}{logical: should reference line be shown?
    If \code{refline==2}, a confidence band be drawn for the component
    effects
  }
  \item{smooth}{if TRUE (or 1), smooths are added to the plots where
    appropriate. If \code{==2}, smmooths to positive and negative
    residuals-from-smooth are also shown.}
  \item{smooth.sim}{number of simulated smooths added to each plot.
    If NULL (the default) 19 simulated smooths will be generated if
    possible and sensible (i.e., none if \code{smooth.group} is set).}
  \item{xlabs}{labels for x variables. Defaults to \code{vars}}
  \item{reslabs}{labels for vertical axes}
  \item{markextremes}{proportion of extreme residuals to be labeled.
    If all points should be labeled, let \code{markextremes=1}.}
  \item{mf}{vector of 2 elements, indicating the number of rows and
    columns of panels on each plot page.
    Defaults to \code{c(2,2)}, except for multivariate models, where
    it adjusts to the number of target variables.
    \code{mf=c(1,1)} or \code{mf=1} asks for a single frame per page.
    \code{mf=NA} or \code{mf=0} leaves the framing (and \code{oma})
    unchanged.}
  \item{mfcol}{if TRUE, the panel will be filled columnwise}
  \item{multnrow, multncol}{number of rows and columns of panels on
    one page, for residuals of multivariate regression only}
  \item{marmult}{plot margins for scatterplot matrices in the case of
    multivariate regression}
  \item{oma}{vector of length 4 giving the number of lines in the outer
    margin. If it is of length 2, they refer to top an right margins.}
    \item{assign}{logical: should the result of \code{pl.control} be
    assigned to the \code{pl.envir} environment?
    This will be done for high level pl functions, but avoided for low
    level ones. It allows for reusing the settings and helps debug
    unexpected behavior.
  }
  \item{...}{further arguments in the call, to be ignored by
    'plotregr.control'}
}

\value{A list containing all the items needed to specify plotting
  in \code{\link{plregr}} and \code{\link{plresx}}
}
\note{This function is not explicitly called by the user, but by
  \code{\link{plregr}} and \code{\link{plresx}}.
  All the arguments specified here can and should be given as
  arguments to these functions.
}
  
\author{Werner A. Stahel, Seminar for Statistics, ETH Zurich}

\seealso{
  \code{\link{plregr}} and \code{\link{plresx}}
}
\examples{
data(d.blast)
( r.blast <-
  lm(log10(tremor)~location+log10(distance)+log10(charge), data=d.blast) )

plargs <- plregr.control(r.blast, formula = ~.+distance, transformed=TRUE,
smooth.group = location )
showd(plargs$pdata)
names(plargs)

}
\keyword{utilities}

