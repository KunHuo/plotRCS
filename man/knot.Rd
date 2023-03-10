% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knots.R
\name{knot}
\alias{knot}
\title{Location of knots for RCS}
\usage{
knot(k = 3)
}
\arguments{
\item{k}{number of knots, must be strictly between 3 and 7.}
}
\value{
a numeric vector.
}
\description{
Location of knots from Harrell (2015), Regression Modeling Strategies.
}
\details{
Harrell (2015) states that for many datasets, k = 4 offers an adequate fit of
the model and is a good compromise between flexibility and loss of pocesion
caused by overfitting a small sample”. If the sample size is small, three knots
should be used in order to have enough observations in between the knots to be
able to fit each polynomial. If the sample size is large and if there is reason
to believe that the relationship being studied changes quickly, more than five
knots can be used.
}
\examples{
# Location of 3 knots
knot(3)

# Location of 7 knots
knot(7)
}
\references{
Harrell FE (2015). Regression models for continuous y and case study in ordinal
regression. In: Harrell FE, ed. Regression Modeling Strategies. New York:
Springer.
}
