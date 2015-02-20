## Coursera  Programming in R, Assignment #2
##
## Author: Sean Downey February 20, 2015
## 
##
## Below are two functions that are used to create a special object that stores a matrix and caches its inverse.

## This function sets up a matrix, storing it in anothher environment
## it also sets up 'setters' and 'getters' for doing the inverse calculations

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL

  ## this acts to cache the matrix in a seperate environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  ## set up the matrix-related functions
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function will, when called, check to see if the inversae calculation was already done. 
## if the calculation has already been down, we use the cached result, otherwise we do the calculation

cacheSolve <- function(x, ...) {
  
  ## attempt to get the calculated matrix
  m <- x$getInverse()
  
  ## if we already did it, return those results, we're done
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # haven;t done anything, we need to do the calculations 
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}