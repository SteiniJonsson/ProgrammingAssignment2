## =================================================================================================
## The functions will store an inverted matrix in cache in order to save the expensive
## operation to invert a matrix every time.
## Author: Steini Jonsson, steini@straumnes.is
## =================================================================================================

## -------------------------------------------------------------------------------------------------
## Function makeCacheMatrix creates an object holding a matrix as well
## as an inverse instance of the matrix.
## If no argument is passed to the makeCacheMatrix function an empty matrix is created.
## The inverse is empty (NULL) until setinv has been called.
## -------------------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## -------------------------------------------------------------------------------------------------
## The cacheSolve function will solve the matrix and store the inversed
## matrix. If a matrix has already been solve a cached version of the inversed
## matrix is retrieved, otherwise the matrix is solved and stored in cache.
## -------------------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
