## Matrix inversion is costly, so there is a benefit to caching the inverse of a matrix.
## A pair of functions are used to create a special object that stores a matrix and caches its inverse

## The makeCacheMatrix function creates a "matrix" object that cache its inverse
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


## The cacheSolve function computes the inverse of the "matrix" created by
## makeCacheMatrix, and if the inverse has already been calculated and the matrix
## has not changed, then it should retrieve the inervse fromt the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
