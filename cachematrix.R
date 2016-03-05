###############################################################################
# name: makeCacheMatrix
#  args:
#       x - a matrix
###############################################################################  
# This function creates a special "matrix" object that can cache its inverse.
# It creates a special "matrix", which is really a list containing a function to:
#     set the value of the matrix
#     get the value of the matrix
#     set the value of the inverse
#     get the value of the inverse
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function(){
    x
  }
  setinverse <- function(inv) {
    inverse <<- inv
  }
  getinverse <- function() {
    inverse
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

###############################################################################
# name: cacheSolve
#  args:
#       x - a matrix
############################################################################### 
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# Here, we assume that the matrix supplied is always invertible.
###############################################################################
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  
}
