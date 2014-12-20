# Caching the Inverse of a Matrix
# This script contists of 2 functions to optimize processing time of inversion of a given matrix,
# by caching it in local memory.
# Note that for this script the matrix supplied is always invertible!

# This 1st function (makeCacheMatrix) is for loading a matrix object, and for caching the inverse
makeCacheMatrix <- function(x = matrix()) {
      # set empty matrix-object
      s <- NULL
      set <- function(y) {    
            x <<- y
            s <<- NULL
      }
      # get the value of the matrix
      get <- function() { x }
      # set the value of the solve function (solve = inverse)
      setsolve <- function(solve) {
            s <<- solve 
      }
      # get the value of the solved function
      getsolve <- function() { s }
      # put all information on what is done in a list
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}
# 2nd function (cacheSolve) does the actual work and before calculating, 
# checks whether the calculation on that matrix is already done to avoid any redudant processing;
cacheSolve <- function(x = matrix()) {
      # get inverse matrix
      s <- x$getsolve()
      # if this inverse matrix is not null, return it and end of function
      if(!is.null(s)) {
            return(s)
      }
      # else get inverse via makeCacheMatrix function
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
}