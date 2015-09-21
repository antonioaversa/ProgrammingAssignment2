## These functions are wrappers of numeric matrices, implementing inverse 
## memoization (caching) for fast retrieval of an already computed result.
## makeCacheMatrix builds a wrapper around a matrix, able to cache the inverse
## cacheSolve takes a wrapper and performs the solve, using/updating the cache

## Given a numerical matrix x, the function returns a list of 4 functions: the 
## read/write accessors to x and the read/write accessors to its inverse
makeCacheMatrix <- function(x = matrix()) {
  i = NULL;
  set <- function(y) {
    x <<- y
    i <<- NULL
  };
  get <- function() x;
  setinverse <- function(inverse) i <<- inverse;
  getinverse <- function() i;
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse);
}


## Given a special matrix wrapper x (build around a numeric matrix using 
## makeCacheMatrix), the function returns its inverse. The result is cached.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
