## Functions for helping cache matrix inverses.

## Return a cacheable matrix object. This object is not intended to be
## interacted with directly; the `cacheSolve` function should be used
## instead.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Get the inverse of a matrix from a cache. If the cache is not primed,
## calculate the inverse of a matrix, store it in cache, and return the
## calculated value.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}