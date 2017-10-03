## Coursera - R Programming: Assignment 2
## Date: Oct. 3, 2017
## Objective: Caching the inverse of a matrix

### Background info:
## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix 
## inversion that we will not discuss here). Your assignment is to write a pair of functions that 
## cache the inverse of a matrix.


### Objective: 
## Write two functions in order to cach the inverse of the matrix:

### Code: 
## 1. This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inverse <<- solveMatrix
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## 2. This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix has not changed), then the 
##    cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
