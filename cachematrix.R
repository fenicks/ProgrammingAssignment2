## The both functions create a matrix with cacheable capability only for 
## the inverse of the matrix.
## The 'makeCacheMatrix' create the special matrix.
## The 'cacheSolve' retrieve the inverted matrix but the computation 
## is made only once.

## This function create a cacheable matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function retrieve a cached inverted matrix if exist.
## If it doesn't then calculates the inverse.
library('MASS')
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message('getting cached data')
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
