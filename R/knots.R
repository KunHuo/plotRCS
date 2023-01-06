#' Location of knots for RCS
#'
#' @return a numeric vector.
#' @export
knots <- function(n = 3){
  n <- as.character(n)
  switch (n,
    "3" = c(0.10,  0.50,   0.90),
    "4" = c(0.05,  0.35,   0.65,   0.95),
    "5" = c(0.05,  0.275,  0.50,   0.725, 0.95),
    "6" = c(0.05,  0.23,   0.41,   0.59,  0.77,   0.95),
    "7" = c(0.025, 0.1833, 0.3417, 0.50,  0.6583, 0.8167, 0.975)
   )
}
