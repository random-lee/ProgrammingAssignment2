## R Programming Assignment 2 - Caching the Inverse of a Matrix

## This function make a list of functions to get a matrix and cache its inverse 

makeCacheMatrix <- function(x = matrix()) {

  inv_m <- NULL # initializes the inverse to NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x # Getting the starting matrix
  setInverse <- function(inverse_m) inv_m <<- inverse_m # Setting the inverse matrix
  getInverse <- function() inv_m # Getting the inverse matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Calculates the inverse matrix, if it is not already cahced

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_m <- x$getInverse()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  m <- x$get()
  inv_m <- solve(m, ...)
  x$setInverse(inv_m)
  inv_m
}


# Test:
# 
# > test_mat <-rbind (c(4,7), c(2,6))
# > testX <- makeCacheMatrix(test_mat)
# > testX$get()
#       [,1] [,2]
# [1,]    4    7
# [2,]    2    6
# > testX$getInverse()
# NULL
# > cacheSolve(testX)
#       [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
# > testX$getInverse()
#       [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
