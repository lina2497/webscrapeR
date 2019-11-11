#' Run functions safely
#'
#' @param x A function.
#' @param return_na Logical, true to return NA when function fails, FALSE to return nothing.
#' @return The output \code{x} when \code{x} runs succesfully. When \code{x} is unsuccessful, returns NA when \code{return_na} is TRUE, otherwise NULL..
#' @examples
#' error_proof(mean("a"), return_na=TRUE)
#' @export


error_proof <- function(x, return_na = FALSE){
  tryCatch(
    x,
    error = function(e) {cat("ERROR :", conditionMessage(e), "\n")
      if(return_na){return(NA)}
      }
    )
}
