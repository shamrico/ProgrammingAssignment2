## This file contains two methods used to cache the inverse 
## of a matrix, since this can be an expensive operation.

## This function maintains the cache containing the inverse 
## of a matrix whose inverse exists.

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function()
    x
  setinv <- function(inv)
    minv <<- inv
  getinv <- function()
    minv
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}


## This function returns a cached inverse, if it 
## has been computed previously. Additionally, a
## message to the effect that a cached value
## is being returned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
