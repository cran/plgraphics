\name{Tobit}
\alias{Tobit}
\title{Prepare a Response for a Tobit Model}
\description{
Returns a \code{Surv} object that allows for setting up a Tobit
regression model by calling \code{survreg}
}
\usage{
Tobit(data, limit = 0, limhigh = NULL, transform = NULL, log = FALSE, ...)
}
\arguments{
  \item{data}{
    the variable to be used as the response in the Tobit regression
}
  \item{limit}{
    Lower limit which censors the observations.
    If \code{log} is \code{TRUE}, then the default is the minimal value
    of \code{\link{logst}}(data), and if \code{limit>0}, it refers to the
    untransformed data.
}
  \item{limhigh}{
    Upper limit which censors the observations (for untransformed data).
}
  \item{transform}{
    if data should be transformed, specify the function to be used.
  }
  \item{log}{
    logical. If \code{TRUE}, \code{data} will be log transformed
    by calling \code{\link{logst}}. Argument \code{transform} will be
    ignored.
  }
  \item{...}{
    any additional arguments to the \code{transform} function
  }
}
\details{
Tobit regression is a special case of regression with left censored
response data. The function \code{survreg} is suitable for fitting.
In \code{regr}, this is done automatically.
}
\value{
A \code{Surv} object.
}
\author{
Werner A. Stahel
}
\seealso{
  \code{\link{Surv}()} from package \CRANpkg{survival}.
}
\examples{

if(requireNamespace("survival")) {
data("tobin", package="survival")

Tobit(tobin$durable)
(t.r <- survival::survreg(Tobit(durable) ~ age + quant,
                          data = tobin, dist="gaussian"))
if(interactive())
    plregr(t.r)
}
}
\keyword{regression}

