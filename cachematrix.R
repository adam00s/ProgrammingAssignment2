## This pair of function are handling caching of matrix inversions

## make matrix inversion cache

makeCacheMatrix <- function(x = matrix()) {
  
  cache <- NULL
  
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() {
    x
  }
  setinverted <- function(inverted) {
    cache <<- inverted
  }
  getinverted <- function() {
    cache
  }
  
  list(set = set, 
       get = get, 
       setinverted = setinverted, 
       getinverted = getinverted)
}


## solve inversion from matrix cache (count it if missing)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverted <- x$getinverted()
  
  if (!is.null(inverted)) {
    message("getting cached data")
  } else {
    inverted <- solve(x$get(), ...)
    x$setinverted(inverted)
  }
    
  inverted
}
