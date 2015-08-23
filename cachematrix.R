## Functions in this file allow the creation of a 'cache matrix' which is able to cache its underlying 
## matrix and inverse. A cacheSolve function calculates and stores the inverse, which is returned from cache
## if it exists. Some MD5 calculation functions (digest package) are used to ensure the cached inverse
## was calculated with the same calculation options as currently specified

# dependency on digest package for MD5 hash calculation
library(digest)

## makeCacheMatrix
##    parameters:
##        x   : matrix
##    returns:
##        a cached matrix object
##
## From an input matrix, return a 'cacheMatrix' which is an object that represents the matrix and 
## its cached inverse. The returned object has methods for getting / setting the underlying matrix, 
## and getting/setting the cached matrix inverse. The cached inverse is cleared if the underlying matrix changes

makeCacheMatrix <- function(x = matrix()) {
  
  storedMatrix <- x
  cachedInverse <- NULL
  
  get <- function() {
    return(storedMatrix)
  }
  
  set <- function(y) {
    storedMatrix <<- y
    # clear cached inverse - underlying matrix has changed
    cachedInverse <<- NULL
  }
  
  getInverse <- function() {
    return(cachedInverse)
  }

  setInverse <- function(inverse) {
    cachedInverse <<- inverse
  }
  
  # return the cacheMatrix as a list of function closures
  return(
    list(
      get = get,
      set = set, 
      getInverse = getInverse,
      setInverse = setInverse
    )
  )  
}


## cacheSolve
##    parameters:
##        x   : cacheMatrix object (as returned by makeCacheMatrix)
##        ... : additional arguments used for the matrix inverse solve method
##    returns:
##        matrix object which is the inverse of the input underlying matrix
##
## Caches the result of the matrix inversion operation within the cacheMatrix object input.
## If the cached inverse matrix is not set, this is computed and stored, 
## along with the additional calculation option arguments passed (e.g. tolerance). 
## If the cached inverse matrix is available, and the calculation options used to create it
## match the current calculation options, this is just returned as the result, and no inverse calculation
## operation is performed.

cacheSolve <- function(x, ...) {
  
  # determine the MD5 signature of additional calculation options passed in
  currCalcOptions = digest(list(...), algo="md5", serialize=TRUE)
  
  xInverse = x$getInverse()
  if (!is.null(xInverse)) {
    
    # the cached inverse is stored as a list of the inverse matrix and calculation options
    
    if (xInverse$calcOptions == currCalcOptions) {
      # cached inverse matrix should be valid - same calculation options were used, so return the cached value
      message("cacheSolve: returning cached inverse matrix")
      return(xInverse$matrix)
    }
  }
  
  # calculate the inverse matrix
  xMatrix <- x$get()
  xInverseMatrix <- solve(xMatrix, ...)
  
  # store the inverse and the calculation options used
  x$setInverse(list(matrix=xInverseMatrix, calcOptions=currCalcOptions))
  
  # return the inverse matrix
  return(xInverseMatrix)
}