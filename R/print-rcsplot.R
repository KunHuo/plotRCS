#' Print  rcsplot
#'
#' @param x an object of rcsplot
#' @param ... more arguments.
#'
#' @keywords internal
#' @export
print.rcsplot <- function(x, ...){
  if(attr(x, "explain")){
    cat("\n")
    cat(attr(x, "title"))
    cat("\n")
    cat(attr(x, "note"))
    cat("\n\n")
    plot(x)
  }else{
    plot(x)
  }
}
