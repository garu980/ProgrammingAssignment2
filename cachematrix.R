## These functions create the inverse of a matrix and cache the result
## in order to retrieve the result of the inverse operation from the cache
## if it was already calculated. The reason is that the inverse of a matrix
## is a costly operation.

## This function creates a special vector which contains the list of
## functions needed to set/get the value of the inverse of the matrix
## and get/set (calculate) the inverse of a matrix.

makeCacheMatrix <- function(m = matrix()) {
  x <- NULL
  set <- function(x) {
    m <<- x
    x <<- NULL
  }
  get <- function() m
  setinverse <- function(solve) x <<- solve
  getinverse <- function() x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of a matrix if it was not already
## calculated, in which case, it returns the cached inverse of the matrix.

cacheSolve <- function(x, ...) {
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
