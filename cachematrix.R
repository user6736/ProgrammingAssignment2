# Inverting matrices can be a costly operation. This code defines two functions 
# which allow for caching of matrix inverses so that they are not needlessly
# computed.

#' Creates a special 'matrix' which has a cache-able inverse.
#' @param x a matrix, default is the empty matrix \code{matrix()}
#' @return a list with the following components
#'  \item{set}{a function to set the value of the special matrix}
#'  \item{get}{a function that returns the value of the special matrix}
#'  \item{set.inverse}{a function that sets the inverse of the special matrix
#'    with a provided value}
#'  \item{get.inverse}{a function that returns the value of the inverse
#'    associated with the special matrix}
makeCacheMatrix <- function(x = matrix()) {
  
  # Initializes the matrix inverse to the NULL value
  inv = NULL
  
  # Sets the matrix to the value y
  # Resets the matrix inverse to NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Returns the matrix value
  get = function() x
  
  # Sets the inverse to the value inverse
  set.inverse = function(inverse) inv <<- inverse
  
  # Returns the current inverse value (may be NULL)
  get.inverse = function() inv
  
  # Returns list with all set/get functions
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}

#' Returns the cached matrix inverse or computes and caches the matrix inverse
#' if no cached matrix inverse is present.
#' @param x a 'special' matrix
#' @parame ... further arguments passed to \code{solve()}
#' @return the inverse of \code{x}
cacheSolve <- function(x, ...) {
  inverse = x$get.inverse()
  
  if(!is.null(inverse)) {
    message('getting cached data')
    return(inverse)
  }
  else {
    inverse = solve(x$get())
    x$set.inverse(inverse)
    return(inverse)
  }
}
