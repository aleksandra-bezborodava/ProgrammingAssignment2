## A pair of functions that cache the inverse of a matrix
## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  cachedinverse <- NULL
  
  set <- function (newx) {
    x <<- newx
    cachedinverse <<- NULL
  }
  get <- function(){
    x
  } 
  setcachedinverse <- function(inverse) {
    cachedinverse <<- inverse
  }
  getcachedinverse <- function() {
    cachedinverse
  }
  
  list(set = set, get = get, setcachedinverse = setcachedinverse, getcachedinverse = getcachedinverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  cached <- x$getcachedinverse()
  if (! is.null(cached)) {
    return(cached)
  }
  m <- x$get()
  cached <- solve(m)
  x$setcachedinverse(cached)
  cached
}