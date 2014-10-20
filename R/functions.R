
#' Checks if all values in a vector are the same
#'
#' Checks if all values in a vector are the same. \cr
#' Uses \code{\link[base]{unique}}
#'
#' @param x vector
#' @param na.rm exclude missing values before checking if the data in x is constant
#' @param ... other arguments passed to unique
#' @return
#' returns \code{TRUE} if all the data in x have the same value, else it returns \code{FALSE}. For vectors of length 0, returns a logical element of length zero
#' @export
#' @seealso \code{\link[base]{unique}}
#' @examples
#' x <- c(1,1,1,1,NA)
#' is.constant(x)
#' is.constant(x, na.rm=TRUE)
#' x <- 1:100
#' is.constant(x)
#' is.constant(numeric(0))
#' \dontrun{
#' is.constant(list(x = 1:10))
#' }
is.constant <- function(x, na.rm = FALSE, ...) {
  if(!is.vector(x)){
    stop("x is not a vector")
  }
  if(length(x) == 0){
    return(logical(0))
  }
	values <- unique(x, ...)
	if(na.rm){
	 values <- values[!is.na(values)]
  }
	length(values) == 1
}
