## Basic caching functionality regarding inverse matrices.

## Creates a cachable matrix with a variable to store the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  get <- function() x
  set <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  
  getInverse <- function() inverse
  setInverse <- function(matrix) inverse <<- matrix
  
  list(get = get, set = set, 
       getInverse = getInverse, 
       setInverse = setInverse)
}


## Returns the inverse of a non-singular square CacheMatrix.
cacheSolve <- function(x, ...) {
  m <- x$get();
  
  if (ncol(m) != nrow(m)) {
    stop("Non-square matrices cannot be inverted");
  }
  if(det(m) == 0) { 
    stop("Singular matrices cannot be inverted");
  }
  
  inv <- x$getInverse();
  if(!is.null(inv)) {
    message("Read cached matrix");
    return(inv);
  }
  inv <- solve(m, ...);
  x$setInverse(inv);
  inv;
}