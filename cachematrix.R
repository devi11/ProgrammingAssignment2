## This program contains two functions. One function - "makeCacheMatrix" makes special matrix which can 
## cache its inverse. The other function - "cacheSolve" gets called from outside. It returns inverse
## if available in cache. Otherwise computes it and returns it.

## This function creates a matrix that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i<<- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(Inv) i <<- Inv
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calculate inverse of a function. First checks it the inverse is already cached.
## If it is cached, it returns from cache. Otherwise, it computes the inverse, stores
## it in the cache and returns newly computed inverse.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
