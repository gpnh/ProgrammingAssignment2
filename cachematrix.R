
## These functions return the inverse of a matrix using the function solve.
## If the inverse matrix is already held in cache, then this is used
## in order to avoid the need to repeat the inversion calcualtion.


makeCacheMatrix <- function(x = matrix()) {

## The first function receives a matrix as input.
## NB:The input must be a square matrix which is invertible
  
      m <- NULL
      set <- function(y) {
      x <<- y
      m <<- NULL
      }
  
##  The function then return a list of the four functions used to
##  process, get and store the matrix in the local environment
  
      get <- function() x
      setmatrix <- function(mnew) m <<- mnew
      getmatrix <- function() m
      list(set = set, get = get,
      setmatrix = setmatrix,
      getmatrix = getmatrix)
}




cacheSolve <- function(x, ...) {
## This function returns a matrix that is the inverse of 'x'
  
          m <- x$getmatrix()
          
##  If the matrix is already held in cache
##  and so has not been set to NULL, the cached
##  version is returned

          if(!is.null(m)) {
            message("getting cached data")
            return(m)
          }        

##   If the matrix is not held in cache then the inverse is calculated
##   using solve and this is returned.  The cache is updated with this
##   new matrix.
          d <- x$get()
          m <- solve(d, ...)
          x$setmatrix(m)
          m
}



