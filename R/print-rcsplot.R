#' Print  rcsplot
#'
#' @param x an object of rcsplot
#' @param ... more arguments.
#'
#' @keywords internal
#' @return No return value, called for drawing.
#' @export
print.rcsplot <- function(x, ...){
  if(attr(x, "explain")){
    cat("\n")
    cat(attr(x, "title"))
    cat("\n")
    cat(attr(x, "note"))
    cat("\n\n")
  }
  plot(x)
}
