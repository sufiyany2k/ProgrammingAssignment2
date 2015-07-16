## Below are set of functions that will 
## cache and return the inverse of a matrix.

## This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y;
    inverseMatrix <<- NULL;
  }
  get <- function() return(x);
  setinv <- function(inv) inverseMatrix <<- inv;
  getinv <- function() return(inverseMatrix);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function returns the inverse of the special
## "matrix" returned by function 'makeCacheMatrix'. If the inverse is 
## already available and the matrix has not changed, then
## 'cacheSolve' function will fetch and return the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getinv()
  if(!is.null(inverseMatrix)) {
    message("Getting cached data...")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setinv(inverseMatrix)
  return(inverseMatrix)
}

