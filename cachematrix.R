## Functions to solve the matrix inverse and generate a cache to store the matrix and its inverse.

## Creates a set of functions to get, set the matrix x and getinverse and setinverse to get/set the inverse of x in the cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Solve the matrix inverse of x and store it into a cache for later & faster use.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  funs <- makeCacheMatrix(x)
  inv <- funs$getinverse()
  if (is.null(inv)) {
    inv <- solve(x) %*% x
    funs$setinverse(inv)
  }
  inv
}
