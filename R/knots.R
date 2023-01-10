#' Location of knots for RCS
#'
#' @description
#' Location of knots from Harrell (2001), Regression Modeling Strategies.
#'
#' @param k number of knots, must be strictly between 3 and 7.
#'
#' @details
#' Harrell (2001) states that for many datasets, k = 4 offers an adequate fit of
#' the model and is a good compromise between flexibility and loss of pocesion
#' caused by overfitting a small sample”. If the sample size is small, three knots
#' should be used in order to have enough observations in between the knots to be
#' able to fit each polynomial. If the sample size is large and if there is reason
#' to believe that the relationship being studied changes quickly, more than five
#' knots can be used.
#'
#' @return a numeric vector.
#'
#' @references
#' Harrell, Frank E. Regression modeling strategies: with applications to linear
#' models, logistic regression, and survival analysis. Vol. 608. New York:
#' springer, 2001.
#'
#' Croxford, R. (2016). Restricted Cubic Spline Regression : A Brief Introduction.
#' @export
knot <- function(k = 3){
  if(k < 3 | k > 7){
    stop("The number of knots must be strictly between 3 and 7.", call. = FALSE)
  }
  k <- as.character(k)
  switch (k,
    "3" = c(0.10,  0.50,   0.90),
    "4" = c(0.05,  0.35,   0.65,   0.95),
    "5" = c(0.05,  0.275,  0.50,   0.725, 0.95),
    "6" = c(0.05,  0.23,   0.41,   0.59,  0.77,   0.95),
    "7" = c(0.025, 0.1833, 0.3417, 0.50,  0.6583, 0.8167, 0.975)
   )
}
