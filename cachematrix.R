## cachematrix.R
## Ken Hopwood 16-APR-2015
##
## This set of functions will create a matrix and it's inverse.  Caching is used
## to speed processing time.

## makeCacheMatrix provides matrix get and set functions.  It also provides get
## and set functions for the inverse of the matrix.  The inverse matrix will be
## cached for later retrieval saving computational time.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve will check to see if the inverse of the matrix has been previously cached.
## If not, the inverse of the matrix will be calculated.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
